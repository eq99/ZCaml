# Compiler Homework

OCamle 语法参考：https://ocaml.org/manual/language.html

# Install OCaml

Install OCaml: https://cs3110.github.io/textbook/chapters/preface/install.html

```shell
sudo add-apt-repository ppa:avsm/ppa
sudo apt update
sudo apt install opam -y
```

# Install Rust

Install Rust for WSL: https://www.rust-lang.org/tools/install

```shell
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# 验证
cargo --version

```


# 运行解释器

温馨提示：目前解释器只支持单行代码，请不要使用换行格式的代码，可以复制 `tests` 文件夹下的测试哦。

```shell
cargo run
```

![image](https://user-images.githubusercontent.com/41274826/163586571-5ccf5f1c-575c-462c-b259-92f99de13c2d.png)


# 运行测试

```shell
cargo test
```

# 语法参考

我自己写的 OCaml 叫ZCaml, 语法请参考 `reference` 文件夹下的 `ZCaml.bnf` 文件。

目前支持的语法：

- 整数：`1`, `1_000`
- 布尔值：`true`, `false`
- 算术表达式：`1 + 1 * (2 + 3)`
- 布尔表达式：`true && false || true`
- 单目运算符：`-1`, `+（1 + 2）`
- 函数定义：`let rec even n = n = 0 || odd (n - 1) and odd n = n <> 0 && even (n - 1);;`
- Let 表达式：`let x = 1 in x + 1`
- 函数调用：`add 2 (-1)`

即将支持的语法：
- if 语句
- for 语句
- 链表，数组
- 类型
- 字符串
- 浮点数

