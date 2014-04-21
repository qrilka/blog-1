---
layout: post
title: Ищем в строке
created: 1351747203
categories:
- !binary |-
  0Y3RhNGE0LXQutGC0LjQstC90L7RgdGC0Yw=
- c++
---
Канонически известно, что проивзодить поиск в контейнере лучше с помощью его собственных алгоритмов (если таковые имеются), нежели с помощью обобщённых. У Мейерса написано именно так. Ну и решил я на днях провести простой эксперимент. А поскольку в Style Revisor мне нужно очень часто производить поиск в строке, то эксперимент был мне особенно интересен.

Итак:
{% highlight cpp %}
using namespace std::chrono;

int main() {
    std::string const s( "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabcd" );
    std::string const what( "bcd" );
    
    auto const begin( high_resolution_clock::now() );
    for( size_t i = 0; i < 100000; ++i ) {
        auto const it( std::search(   s.begin()
                                    , s.end()
                                    , what.begin()
                                    , what.end() ) );
    }
    auto const end( high_resolution_clock::now() );
    std::cout << "tm: " 
              << duration_cast <microseconds>( end - begin ).count() 
              << std::endl;
}
{% endhighlight %}Итак, ищем подстроку обобщённым способом. Во втором варианте цикла ищем через собственную функцию:
{% highlight cpp %}
    ...
    for( size_t i = 0; i < 100000; ++i ) {
        auto const pos( s.find( what ) );
        auto const it( s.begin() + pos );
    }
    ...
{% endhighlight %}Ну казалось бы, что может быть проще? В обоих случаях получаем итератор на начало нужной нам подстроки.

Как вы думаете, какой код выполняется быстрее? Я бы удивлён результатами: первый вариант оказался быстрее, причём значительно! Во втором случае имеем в среднем 6900 мкс (худший результат на моём компьютере - 7188 мкс), в то время как в первом - 2900 мкс (худший результат - 3383 мкс).

Вот тебе и канонический поиск...
