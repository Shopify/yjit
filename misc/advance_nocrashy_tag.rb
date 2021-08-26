# nocrashy.rb

require 'net/http'
require 'json'

unless ENV["NOCRASHY_TOKEN"] && ENV["NOCRASHY_USER"]
    puts "To update a branch, you're going to need a GitHub token with the appropriate permissions."
    puts "Please set the env var NOCRASHY_TOKEN to that token and NOCRASHY_USER to the Github username."
    exit -1
end

# We'd love to make sure the last three top-level merge commits on YJIT have succeeded.
# We'd love to not create and manage a personal access token when the script only uses
# 100% public information. For better or worse, that means web scraping.
#
# A commit generates the workflows below. So we'd like to know: for the last three top-level
# merge commits, did the most recent of each of these workflows succeed or fail?

WORKFLOWS = %w(asm_tests.yml baseruby.yml check_dependencies.yml check_misc.yml codeql-analysis.yml compilers.yml macos.yml mjit.yml spec_guards.yml ubuntu.yml windows.yml yjit.yml)

# At the top level, grab this many most-recent commits. We hope it'll be enough.
COMMITS_TO_QUERY=150

def ghapi_get_response(api_uri, ok_404: false)
    uri = URI("https://api.github.com" + api_uri)

    req = Net::HTTP::Get.new(uri)
    req.basic_auth ENV['NOCRASHY_USER'], ENV['NOCRASHY_TOKEN']
    req['Accept'] = "application/vnd.github.v3+json"
    result = Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) { |http| http.request(req) }

    if ok_404 && result.is_a?(Net::HTTPNotFound)
        return :not_found
    end

    unless result.is_a?(Net::HTTPSuccess)
        $stderr.puts "Error in HTTP GET: #{result.inspect}"
        raise "HTTP error when querying results for #{api_uri}!"
    end

    JSON.load(result.body)
end

SYM_TO_NET_HTTP_VERB = {
    post: Net::HTTP::Post,
    put: Net::HTTP::Put,
    patch: Net::HTTP::Patch,
}

def ghapi_post(api_uri, params, verb: :post)
    uri = URI("https://api.github.com" + api_uri)

    req_class = SYM_TO_NET_HTTP_VERB[verb]

    req = req_class.new(uri)
    req.basic_auth ENV['NOCRASHY_USER'], ENV['NOCRASHY_TOKEN']
    req['Accept'] = "application/vnd.github.v3+json"
    req['Content-Type'] = "application/json"
    req.body = JSON.dump(params)
    result = Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) { |http| http.request(req) }

    unless result.is_a?(Net::HTTPSuccess)
        $stderr.puts "Error in HTTP #{verb.upcase}: #{result.inspect}"
        $stderr.puts result.body
        $stderr.puts "------"
        raise "HTTP error when posting to #{api_uri}!"
    end

    JSON.load(result.body)
end

def get_last_merge_commits(to_check: 10)
    # Grab the most recent commits to try to get enough valid top-level commits
    data = []
    queried = 0
    page = 1

    # Query in pages
    loop do
        per_page = [ COMMITS_TO_QUERY - queried, 30 ].min
        this_page = ghapi_get_response("/repos/Shopify/yjit/commits?per_page=#{per_page}&page=#{page}&sha=main")
        data.concat(this_page)
        page += 1
        queried += 30
        break if queried >= COMMITS_TO_QUERY
    end
    if data.size != COMMITS_TO_QUERY
        $stderr.puts "Something went wrong querying commits in pages! The total returned size is too small."
        exit -1
    end

    commit_by_sha = {}
    data.each { |commit| commit_by_sha[commit["sha"]] = commit }

    merge_commits = []
    left_to_check = to_check
    next_sha = data[0]["sha"]

    loop do
        unless commit_by_sha[next_sha]
            puts "Couldn't find an entry for #{next_sha.inspect} in the most recent #{COMMITS_TO_QUERY} commits. D'oh!"
            exit -1
        end
        msg = commit_by_sha[next_sha]["commit"]["message"]
        #unless msg.include?("Merge pull request")
        #    raise "Top-level SHA #{next_sha.inspect} is not a merge of a PR!"
        #end

        merge_commits.push(next_sha)
        next_sha = commit_by_sha[next_sha]["parents"][0]["sha"]
        left_to_check -= 1

        break if left_to_check == 0
    end
    puts "Checked enough top-level commits successfully"

    merge_commits
end

def commit_statuses_for_workflow(workflow_id, num_results: 30)
    # If you wanted to see the workflow yjit.yml for the main branch, only completed runs, only started by a push,
    # you query with the following curl command:
    #
    # curl -H "Accept: application/vnd.github.v3+json" "https://api.github.com/repos/Shopify/yjit/actions/workflows/yjit.yml/runs?per_page=5&event=push&status=completed&branch=main"

    data = ghapi_get_response("/repos/Shopify/yjit/actions/workflows/#{workflow_id}/runs?per_page=#{num_results}&event=push&status=completed&branch=main")
    results = data["workflow_runs"].map { |run| run.slice("head_sha", "conclusion") }
end

def current_nocrashy
    result = ghapi_get_response("/repos/Shopify/yjit/git/ref/tags/nocrashy", ok_404: true)
    if result == :not_found
        nil
    else
        result["object"]["sha"]
    end
end

def update_nocrashy(current_sha, next_sha)
    if current_sha
        puts "Updating nocrashy tag from #{current_sha} to #{next_sha}..."
        ghapi_post("/repos/Shopify/yjit/git/refs/tags/nocrashy", { "sha" => next_sha, "force" => true }, verb: :patch)
    else
        puts "Creating nocrashy tag for SHA #{next_sha}..."
        ghapi_post("/repos/Shopify/yjit/git/refs", { "ref" => "refs/tags/nocrashy", "sha" => next_sha }, verb: :post)
    end
end

# If we're looking at all recent commits. It's likely that some commits haven't
# completed all workflows, and we need to ignore those.
last_merge_commits = get_last_merge_commits(to_check: 10)

success_by_workflow = {}
puts "Getting statuses for all workflows"
WORKFLOWS.each do |workflow|
    success_by_workflow[workflow] = {}
    last = commit_statuses_for_workflow(workflow)
    last.each do |data|
        sha = data["head_sha"]
        result = data["conclusion"]
        success_by_workflow[workflow][sha] = result
    end
end

#pp success_by_workflow

puts "Calculating..."
commits_considered = last_merge_commits.dup

success_by_commit = {}
commits_considered.each do |sha|
    if WORKFLOWS.any? { |wf| success_by_workflow[wf][sha] == "failure" }
        success_by_commit[sha] = :failed
    elsif WORKFLOWS.all? { |wf| success_by_workflow[wf][sha] == "success" }
        success_by_commit[sha] = :success
    else
        success_by_commit[sha] = :incomplete
    end
end

#pp success_by_commit

# Grab the latest series that's sufficiently un-crashy
latest_series = commits_considered.detect.with_index do |sha, idx|
    idx + 2 < commits_considered.size &&
        success_by_commit[sha] == :success &&
        success_by_commit[commits_considered[idx + 1]] == :success &&
        success_by_commit[commits_considered[idx + 2]] == :success
end
latest_series_index = commits_considered.index(latest_series)
latest_nocrashy = commits_considered[latest_series_index + 2] # We want the third of the series, so there are two successes after it

repo_nocrashy = current_nocrashy()

if repo_nocrashy == latest_nocrashy
    puts "Great! We're already up to date."
    exit 0
end

puts "Creating/updating nocrashy tag on the repo..."
update_nocrashy(current_nocrashy, latest_nocrashy)
puts "Tag updated. All's well."
