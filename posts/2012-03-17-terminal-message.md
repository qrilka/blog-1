---
title: Консольное сообщение: маленький пример
tags: C++
---

Как вы уже знаете, Style Revisor сможет работать в командной строке. Вот маленький кусочек кода, отвечающий за вывод на консоль сообщения о некой проблеме.
```cpp
template <typename What>
inline void console_report_about( What const& happened ) {
    std::cerr << "Something wrong: " << happened() << std::endl;
}
```

А вызов выглядит так:
```cpp
if( unknown_arguments_exists_in( probably_valid_arguments ) ) {
    console_report_about( unknown_argument{} );
}
```

Хочу отметить литературность письма. Прочтём сердцевину как текст:
<pre>
    console report about What happened
        Something wrong happened 
}
</pre>
и вызов также:
<pre>
if unknown arguments exists in probably valid arguments
    console report about unknown argument
</pre>

По-моему, симпатишно...
