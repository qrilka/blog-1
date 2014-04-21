---
layout: post
title: 'Консольное сообщение: маленький пример'
created: 1331929821
categories:
- !binary |-
  0KXQvtGA0L7RiNC10LUg0L/QuNGB0YzQvNC+
- !binary |-
  0JvQuNGC0LXRgNCw0YLRg9GA0L3QvtC1INC/0YDQvtCz0YDQsNC80LzQuNGA
  0L7QstCw0L3QuNC1
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
