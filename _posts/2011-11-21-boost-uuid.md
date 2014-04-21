---
layout: post
title: 'Boost.UUID: досадная оплошность'
created: 1321883708
categories:
- boost
---
<!--break-->
Итак, пример:
{% highlight cpp %}
int main() {
    boost::uuids::uuid u;
    std::cout << u << std::endl;
}
{% endhighlight %}И что же у нас на экран выведется? К сожалению, мусор, случайный хлам.

Совершенно непонятно, о чём думали авторы этой библиотеки... Неужели было сложно сделать так:
{% highlight cpp %}
    if ( u.empty() )
        std::cerr << "Empty UUID!" << std::endl;
{% endhighlight %}или так:
{% highlight cpp %}
    if ( !u )
        std::cerr << "Empty UUID!" << std::endl;
{% endhighlight %}
Получается, что если мне в функцию передали UUID, то у меня даже нет возможности узнать, был ли он нормально проинициализирован!

Нда, несолидно, совсем несолидно...
