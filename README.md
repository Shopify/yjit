<p align="center">
  <a href="https://yjit.org/" target="_blank" rel="noopener noreferrer">
    <img src="https://user-images.githubusercontent.com/224488/131155756-aa8fb528-a813-4dfd-99ac-8785c3d5eed7.png" width="400">
  </a>
</p>

YJIT - Yet Another Ruby JIT
===========================

YJIT has finally been merged upstream, and is now an official part of the Ruby 3.1 release.

For more information on how to build and use YJIT, see the [YJIT README](https://github.com/ruby/ruby/blob/master/doc/yjit/yjit.md).

If you want to report bugs or ask questions about YJIT, please [open an issue](https://github.com/Shopify/yjit/issues) on this repository. When reporting bugs, please include as much detail as possible regarding your current setup, and the console commands you used to run YJIT.

To cite this project in your publications, please use this bibtex snippet:

```
@inbook{yjit_vmil2021,
author = {Chevalier-Boisvert, Maxime and Gibbs, Noah and Boussier, Jean and Wu, Si Xing (Alan) and Patterson, Aaron and Newton, Kevin and Hawthorn, John},
title = {YJIT: A Basic Block Versioning JIT Compiler for CRuby},
year = {2021},
isbn = {9781450391092},
publisher = {Association for Computing Machinery},
address = {New York, NY, USA},
url = {https://doi.org/10.1145/3486606.3486781},
abstract = {Ruby is a dynamically typed programming language with a large breadth of features which has grown in popularity with the rise of the modern web, and remains at the core of the implementation of many widely-used websites.  CRuby, the default implementation of the language, features a JIT compiler known as MJIT, but developers often do not enable it in production environments, because it does not always yield performance improvements on real-world software. Attempts to independently reimplement the Ruby language, such as JRuby and TruffleRuby have shown impressive performance results on benchmarks, but often lag behind CRuby when it comes to supporting new additions to the language, which limits their adoption.  We introduce YJIT, a new JIT compiler built inside CRuby based on a Lazy Basic Block Versioning (LBBV) architecture. We show that while our compiler does not match the peak performance of TruffleRuby, it offers near-100% compatibility with existing Ruby code, impressively fast warmup, and speedups from 15% to 19% on sizeable benchmarks based on real-world software.},
booktitle = {Proceedings of the 13th ACM SIGPLAN International Workshop on Virtual Machines and Intermediate Languages},
pages = {25–32},
numpages = {8}
}
```
