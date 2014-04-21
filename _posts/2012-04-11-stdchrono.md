---
layout: post
title: ВременнЫе замеры с std::chrono
created: 1334164907
categories:
- c++11
---
Стандарт C++11 ввёл новый набор классов для работы с временем, в заголовочнике <chrono>. И вот простой рецепт (спасибо сайту cplusplus.com), как измерить время работы некой функции. Достаточно часто это нужно знать.

Итак:
{% highlight cpp %}
typedef std::chrono::duration <int> seconds;

void for_second() {
    std::this_thread::sleep_for( seconds( 1 ) );
}

int main() {
    using namespace std::chrono;
    auto begin = high_resolution_clock::now();
    for_second();
    auto end = high_resolution_clock::now();
    std::cout << duration_cast <milliseconds>( end - begin ).count() 
              << std::endl
              ;
}
{% endhighlight %}
Таким образом, мы сразу получаем количество милисекунд, прошедших за время вызова функции.

Если нужен замер поточнее, можем использовать микросекунды:
{% highlight cpp %}
duration_cast <microseconds>( end - begin ).count() 
{% endhighlight %}и даже наносекунды:
{% highlight cpp %}
duration_cast <nanoseconds>( end - begin ).count() 
{% endhighlight %}
