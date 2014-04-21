---
layout: post
title: 'Пояснение без комментариев: пример'
category: articles
tags: [C++]
comments: true
share: true
---
Итак, есть функция:
{% highlight cpp %}
void set_margins( int left, int top, int right, int bottom );
{% endhighlight %}
Ну, из сигнатуры сразу понятно, что делает эта функция. Однако наиболее вероятным примером использования оной будет такой:
{% highlight cpp %}
placer->set_margins( 2, 2, 2, 2 );
{% endhighlight %}
И вот тут возникает известная проблема "магических чисел". Мы ведь может и забыть, какой по счёту аргумент отвечает за отступ сверху, а какой - снизу. Каждый раз возвращаться к объявлению функции абсолютно не хочется. Что же делать?

Решение первое, классическое:
{% highlight cpp %}
constexpr int left   = 2;
constexpr int top    = 2;
constexpr int right  = 2;
constexpr int bottom = 2;
placer->set_margins( left, top, right, bottom );
{% endhighlight %}
Решение неплохое, однако если в пределах одной области видимости нам нужно вызвать функцию **set_margins** более одного раза, нам придётся либо заключать аргументные переменные в искусственные области видимости, либо давать им уникальные имена, либо, сняв константность, использовать их везде, изменяя, где нужно, их значения (а это чревато ошибками, можно элементарно запутаться).

Решение второе, плоховатое:
{% highlight cpp %}
placer->set_margins(   2 // left
                     , 2 // top
                     , 2 // right
                     , 2 // bottom 
                   );
{% endhighlight %}
Итак, комментарии. Это решение не такое уж и плохое, но... Лично я бы не стал так делать (хотя в прошлом много раз так делал).

Решение третье, оригинальное:
{% highlight cpp %}
inline constexpr int left  ( int const px ) { return px; }
inline constexpr int top   ( int const px ) { return px; }
inline constexpr int right ( int const px ) { return px; }
inline constexpr int bottom( int const px ) { return px; }
{% endhighlight %}
Итак, мы определили четыре функции-пустышки, не делающие ничего, кроме как возвращающие свой единственный аргумент. Однако теперь мы можем написать так:
{% highlight cpp %}
placer->set_margins(   left( 2 )
                     , top( 2 )
                     , right( 2 )
                     , bottom( 2 ) );
// Чуть ниже, в этой же области видимости...
placer->set_margins(   left( 3 )
                     , top( 5 )
                     , right( 4 )
                     , bottom( 4 ) );
{% endhighlight %}
В итоге мы получаем короткое письмо без промежуточных переменных, без конфликтов имён, понятное с первого прочтения и при этом не использующее комментарии. Кроме того, из-за использования **constexpr** мы совершенно не теряем в эффективности на стадии выполнения.

По-моему, красиво.
