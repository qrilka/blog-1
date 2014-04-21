---
layout: post
title: 'Синтаксическая подсветка в Qt: кусочек'
created: 1327579528
categories:
- !binary |-
  0KXQvtGA0L7RiNC10LUg0L/QuNGB0YzQvNC+
- !binary |-
  0JvQuNGC0LXRgNCw0YLRg9GA0L3QvtC1INC/0YDQvtCz0YDQsNC80LzQuNGA
  0L7QstCw0L3QuNC1
---
Переопределённая функция, отвечает за подсветку ключевых слов в переданном извне куске текста.
{% highlight cpp %}
void editor_highlighter::highlightBlock( QString const& text ) {
    QTextCharFormat keywords_format;
    keywords_format.setFontWeight( QFont::Bold );
    auto keywords( kernel.list_of_keywords_of_current_language() );
    highlight( this, keywords ).based_on( keywords_format ).in_this( text );
}
{% endhighlight %}Обратите внимание на последнюю строку. Функтору **highlight** необходимо было передать указатель **this**, и здесь это сделано в литературной манере:
{% highlight cpp %}
    highlight this keywords...
{% endhighlight %}То есть слово **this** не только явилось указателем на текущий класс, но и сыграло литературную роль именно как английское слово "this". Мне очень понравилось такое решение. 

