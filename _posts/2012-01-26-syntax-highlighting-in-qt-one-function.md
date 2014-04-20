---
layout: post
title: 'Syntax highlighting in Qt: one function'
created: 1327580358
categories:
- !binary |-
  0KXQvtGA0L7RiNC10LUg0L/QuNGB0YzQvNC+
- !binary |-
  0JvQuNGC0LXRgNCw0YLRg9GA0L3QvtC1INC/0YDQvtCz0YDQsNC80LzQuNGA
  0L7QstCw0L3QuNC1
---
Function for syntax highlighting of keywords in text.
<cpp>
void editor_highlighter::highlightBlock( QString const& text ) {
    QTextCharFormat keywords_format;
    keywords_format.setFontWeight( QFont::Bold );
    auto keywords( kernel.list_of_keywords_of_current_language() );
    highlight( this, keywords ).based_on( keywords_format ).in_this( text );
}
</cpp>Pay your attention to last line. Functor <strong>highlight</strong> require <strong>this</strong>-pointer, and we do it in literary manner:
<cpp>
    highlight this keywords...
</cpp>So word <strong>this</strong> is not only pointer to this class, but exactly English word "this". I very like this solution. 

