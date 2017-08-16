CDE - C/C++ development environment for emacs

This project provides all-in-one set of features for comfort
C/C++ development.
It includes code navigation (find definition/references)
Code completion.
Switch source/headers files.
Smart includes opening.
On-the-fly syntax checking (experimantal so far)
Cde uses .prj files for identify project settings. (Support of
compile_commands.json and some other formats is in roadmap)


Build dependencies:
POSIX OS
clang-4.x-dev (or higher)
llvm-4.x-dev (or higher)
libdb-5.1++-dev (or higher)

Usage dependencies:
emacs with company-mode installed.

Known issues:

1. In some cases unwanted clearing of highlighting errors
2. Some multiple open projects issues

No releases so far, project is in early development stage.