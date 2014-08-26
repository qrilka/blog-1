---
layout: post
title: Консольное сообщение: маленький пример
category: articles
tags: C++
comments: true
share: true
---
Как вы уже знаете, Style Revisor сможет работать в командной строке. Вот маленький кусочек кода, отвечающий за вывод на консоль сообщения о некой проблеме.
{% highlight cpp %}
template <typename What>
inline void console_report_about( What const& happened ) {
    std::cerr << "Something wrong: " << happened() << std::endl;
}
{% endhighlight %}

А вызов выглядит так:
{% highlight cpp %}
if( unknown_arguments_exists_in( probably_valid_arguments ) ) {
    console_report_about( unknown_argument{} );
}
{% endhighlight %}

Хочу отметить литературность письма. Прочтём сердцевину как текст:
<pre>
    console report about What happened
        Something wrong happened 
}
</pre>и вызов также:
<pre>
if unknown arguments exists in probably valid arguments
    console report about unknown argument
</pre>

По-моему, симпатишно...
