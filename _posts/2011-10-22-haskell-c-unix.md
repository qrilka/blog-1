---
layout: post
title: 'Haskell и C++: чтение файла в духе Unix'
created: 1319304346
categories:
- !binary |-
  0KTRg9C90LrRhtC40L7QvdCw0LvRjNC90L7QtSDQv9GA0L7Qs9GA0LDQvNC8
  0LjRgNC+0LLQsNC90LjQtQ==
- haskell
- c++11
---
<!--break-->
Итак, перед нами простая задача: открыть текстовый файл и вывести его содержимое на консоль (разумеется, с сохранением первоначального форматирования). 

Для простоты опустим все проверки (будем исходить из того, что файл по указанному пути существует, что у нас есть права на его чтение и что в нём действительно текст).

Наиболее красивое и компактное решение на C++, которое я знаю, выглядит так:

<cpp>
#include <iostream>
#include <fstream>
#include <iterator>
#include <algorithm>

int main() {
    std::ifstream my_file( "/home/denis/test" );
    typedef std::istreambuf_iterator<char> ReadIterator;
    typedef std::ostream_iterator<char>    WriteIterator;
    std::copy(   ReadIterator( my_file )
               , ReadIterator()
               , WriteIterator( std::cout ) );
}
</cpp>

А вот решение на Haskell:

<hs>
main = putStr =<< readFile "/home/denis/test" 
</hs>

Хм... Я не знаю, какое из этих двух решений более эффективное в плане выполнения (не проверял), но в плане простоты восприятия кода вердикт очевиден: решение на Haskell лучше. Дело не в том, что оно компактнее, нет. Самое главное в том, что оно проще. Решение на C++ подразумевает знакомство читателя с концепцией итератора. А решение на Haskell не содержит ничего лишнего, а значит, оно находится на более высоком уровне абстракции. Элегантно и в истинном духе Unix.

А вы как думаете?
