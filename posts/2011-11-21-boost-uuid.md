---
title: Boost.UUID: досадная оплошность
tags: boost
---

Итак, пример:
```cpp
int main() {
    boost::uuids::uuid u;
    std::cout << u << std::endl;
}
```И что же у нас на экран выведется? К сожалению, мусор, случайный хлам.

Совершенно непонятно, о чём думали авторы этой библиотеки... Неужели было сложно сделать так:
```cpp
    if ( u.empty() )
        std::cerr << "Empty UUID!" << std::endl;
```или так:
```cpp
    if ( !u )
        std::cerr << "Empty UUID!" << std::endl;
```
Получается, что если мне в функцию передали UUID, то у меня даже нет возможности узнать, был ли он нормально проинициализирован!

Нда, несолидно, совсем несолидно...
