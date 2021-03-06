---
title: Haskell: о типах
tags: Haskell
---

<h3>Строгая статическая проверка</h3>

Haskell имеет статическую проверку типов. Это означает, что тип каждого выражения в программе обязательно проверяется на стадии компиляции. Более того, Haskell весьма строг по отношению к типам, чего, к сожалению, нельзя сказать о C-подобных языках. Дело в том, что в C или C++ допустимо неявное приведение одного типа к другому. Например, так:

```cpp
int f() {
    return 12.9;
}
```

Компилятор C или C++ не только проглотит это, но даже не обмолвится ни единым предупреждением (в том числе и при использовании флага <code>-Wall</code>), при этом аккуратно отбросив дробную часть, ведь тип возращаемого значения будет незримо приведён к <code>int</code>. В Haskell подобный код не имеет ни малейших шансов пройти компиляцию: нужно вернуть <code>int</code> - будь добр предоставить выражение, имеющее (в конечном итоге) тип <code>int</code>.

<h3>Автоматическое выведение</h3>

В Haskell применяется принцип автоматического выведения типов, что позволяет программисту не указывать тип выражения явно. Похожий подход используется в C++11 с использованием ключевого слова <code>auto</code>:

```cpp
auto i = 12;
```

Но в Haskell пишут ещё проще:

```haskell
i = 12
```

Компилятор сам поймёт, что тип выражения <code>i</code> - это именно <code>Integer</code>.

Впрочем, при желании программист *может* указать тип выражения явно (иногда это бывает полезно, в частности, при отладке и для большей самодокументируемости кода). Например, если я напишу так:

```haskell
let i = 12 :: Double
```

то тип <code>i</code> будет уже <code>Double</code>.

Вот и всё, теперь вы знаете фундаментальные утверждения о типах в Haskell. Продолжение следует...
