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
clang-dev (NOT libclang)
Berkeley DB with c++ bindings

Usage dependencies:
emacs with company-mode installed.

Known issues:
Unstable work with clang 3.7.0 (will be fixed in future)

No releases so far, project is in early development stage.