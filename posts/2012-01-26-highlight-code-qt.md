---
layout: post
title: Синтаксическая подсветка в Qt: кусочек
category: articles
tags: Qt
comments: true
share: true
---
Переопределённая функция, отвечает за подсветку ключевых слов в переданном извне куске текста.
```cpp
void editor_highlighter::highlightBlock( QString const& text ) {
    QTextCharFormat keywords_format;
    keywords_format.setFontWeight( QFont::Bold );
    auto keywords( kernel.list_of_keywords_of_current_language() );
    highlight( this, keywords ).based_on( keywords_format ).in_this( text );
}
```
Обратите внимание на последнюю строку. Функтору **highlight** необходимо было передать указатель **this**, и здесь это сделано в литературной манере:
```cpp
    highlight this keywords...
```
То есть слово **this** не только явилось указателем на текущий класс, но и сыграло литературную роль именно как английское слово "this". Мне очень понравилось такое решение. 

