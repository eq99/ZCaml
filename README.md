# Compiler Homework

OCamle 语法参考：https://ocaml.org/manual/language.html

# compile and run llvm sample project

Install llvm: https://apt.llvm.org/

```shell
apt-get install clang-format clang-tidy clang-tools clang clangd libc++-dev libc++1 libc++abi-dev libc++abi1 libclang-dev libclang1 liblldb-dev libllvm-ocaml-dev libomp-dev libomp5 lld lldb llvm-dev llvm-runtime llvm python3-clang
```

Test install: https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl03.html#full-code-listing

Copy this full code to `ch02.cpp` and build:
```shell
clang++ -g -O3 ch02.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core`

./a.out
```

Do not use `g++`


# Install OCaml

Install OCaml: https://cs3110.github.io/textbook/chapters/preface/install.html

```shell
sudo add-apt-repository ppa:avsm/ppa
sudo apt update
sudo apt install opam -y
```

# Tasks

## ch01

```shell
clang++ -g -O3 ch01.cpp `llvm-config --cxxflags`

./a.out
```

## ch02

```shell
clang++ -g -O3 ch02.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core`

./a.out
```














