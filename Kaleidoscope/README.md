Tutorial from llvm: https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html

Rust version: https://github.com/TheDan64/inkwell/tree/master/examples/kaleidoscope

# compile and run llvm tutorial

## Install llvm

tutorial here: https://apt.llvm.org/

```shell
apt-get install clang-format clang-tidy clang-tools clang clangd libc++-dev libc++1 libc++abi-dev libc++abi1 libclang-dev libclang1 liblldb-dev libllvm-ocaml-dev libomp-dev libomp5 lld lldb llvm-dev llvm-runtime llvm python3-clang
```

## ch02

```shell
clang++ -g -O3 ch02.cpp `llvm-config --cxxflags`

./a.out
```

## ch03

```shell
clang++ -g -O3 ch03.cpp `llvm-config --cxxflags --ldflags --system-libs --libs core`

./a.out
```

⚠️ Use `g++ ch03.cpp` may cause error.