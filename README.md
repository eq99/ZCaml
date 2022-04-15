# Compiler Homework

OCamle 语法参考：https://ocaml.org/manual/language.html

# Install OCaml

Install OCaml: https://cs3110.github.io/textbook/chapters/preface/install.html

```shell
sudo add-apt-repository ppa:avsm/ppa
sudo apt update
sudo apt install opam -y
```

# 运行解释器

注意：目前解释器只支持单行代码解析，请不要使用换行的代码，可以复制 `reference` 中的例子。

```shell
cargo run
```

# 运行测试

```shell
cargo test
```

支持的语法：

- 整数：`1`, `1_000`
- 布尔值：`true`, `false`
- 算术表达式：`1 + 1 * (2 + 3)`
- 布尔表达式：`true && false || true`
- 单目运算符：`-1`, `+（1 + 2）`
- 函数定义：`let rec even n = n = 0 || odd (n - 1) and odd n = n <> 0 && even (n - 1);;`
- Let 表达式：`let x = 1 in x + 1`
- 函数调用：`add 2 (-1)`

打算支持：
- 字符串
- 浮点数
- 类型
- 数据结构：链表，数组，记录
- if 语句
- for 语句