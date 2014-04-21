---
layout: post
title: Функтор-предикат?
created: 1317893857
categories:
- !binary |-
  0JvQuNGC0LXRgNCw0YLRg9GA0L3QvtC1INC/0YDQvtCz0YDQsNC80LzQuNGA
  0L7QstCw0L3QuNC1
---
<!--break-->
Смотрите:

{% highlight cpp %}
    if ( This( document ).has_invalid <encoding>() )
        report_about_invalid <encoding>(). in( document );
{% endhighlight %}

Читаем как текст:

{% highlight cpp %}
    If   this  document   has invalid  encoding -
        report about invalid  encoding     in  document.
    
    if ( This( document ).has_invalid <encoding>() )
        report_about_invalid <encoding>(). in( document );
{% endhighlight %}

НЕ ДУМАТЬ о том, как это реализовано! :) Думать о смысле! :)

Ну а если серьёзно, то взгляните на это решение. Предикат, свидетельствующий о неверной кодировке текстового документа, состоит как бы из двух частей. И сделано это именно для естественного прочтения.

Имя This, разумеется, должно начинаться с большой буквы, ключевые слова всё-таки нужно учитывать. Также обратите внимание на то, что document - это объект, а encoding - тип для шаблонизации. Последнее соответствует концепции программирования на основе политик (или стратегий). Это может дать весьма гибкую структуру.

Но опять-таки, детали реализации не так важны, важна суть этого "программного письма".
