---
layout: post
title: C++11: std::unordered_map против std::map
category: articles
tags: [C++]
comments: true
share: true
---

Известно, что в C++11 в пространство имён **std** внесён контейнер **unordered_map**. Для его использования необходимо включить в программу одноимённый заголовочник.

Известно также и то, что поиск значения по ключу в **unordered_map** производится быстрее, чем в **map**. Но насколько быстрее? Простой эксперимент покажет нам.

Итак:
```cpp
#include <map>
#include <unordered_map>

template< typename T >
void fill( T& cont ) {
    const size_t test_size = 1000000;
    for ( size_t i = 0; i < test_size; ++i ) {
        cont.insert( { i, "a" } );
    }
    BOOST_ASSERT ( test_size == cont.size() );
}

int main() {
    std::map< int, std::string >           m;
    std::unordered_map< int, std::string > um;
    
    fill( m );
    fill( um );
    
    // Использую поиск через at(), как наиболее часто используемый способ доступа к элементу.
    // Для простоты замера использую поиск в цикле.    

    const size_t test_key = 98798;
    
    time_stamp();
    for ( size_t i = 0; i < 100000; ++i ) {
        const std::string value1 = m.at( test_key );
    }
    time_stamp();

    time_stamp();
    for ( size_t i = 0; i < 100000; ++i ) {
        const std::string value2 = um.at( test_key );
    }
    time_stamp();
}
```

Вывод:
2011-Apr-07 04:35:15.257240
2011-Apr-07 04:35:15.313906
2011-Apr-07 04:35:15.313945
2011-Apr-07 04:35:15.320404

Итак, огрублённо имеем **56** мс против **6** мс. Ну что ж, впечатляет: доступ к элементу в **unordered_map** быстрее приблизительно в 9 раз.
