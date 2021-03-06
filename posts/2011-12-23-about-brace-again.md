---
title: В стотысячный раз - о фигурной скобке
tags: C++
---

Да-да, я знаю, об этом холиварили уже, как говорится, 100500 раз. И всё же я изложу свою позицию, и обосную её. Речь пойдёт об открывающей фигурной скобке.

Я не считаю этот вопрос сверх-принципиальным и гипер-важным, любой программист может писать и так, и эдак. Однако я убеждён, что в языках программирования, где фигурные скобки ограничивают тело констукции, **открывающая скобка не должна переноситься на следующую строку**. И вот почему.

Рассмотрим два примера:
```cpp
if ( predicat )
{
    some_action;
}
```и```cpp
if ( predicat ) {
    some_action;
}
```
В чём тут разница? Сразу скажу: экономия лишней строки во втором примере, конечно же, не играет никакой роли.

А ведь разница имеется, и достаточно важная. Главный довод защитников первого примера звучит так: *"Открывающая и закрывающая скобки симметричны друг по отношению к другу, и мы сразу видим их соответствие, поскольку одна расположена над другой"*. Когда-то я тоже придерживался этого довода, и всегда писал открывающую над закрывающей. Однако потом...

Друзья, разве цель в том, чтобы соблюсти симметричность *скобок*? Нет, цель в том, чтобы соблюсти симметричность *всей конструкции*. Ведь конструкция - это не скобки, это описательная строка + скобки. То есть:
```cpp
if ( predicat )   // Это описательная строка if-конструкции
{                 // Это начало тела if-конструкции
    some_action;  // Это тело if-конструкции
}                 // Это конец тела if-конструкции
```
Но мы знаем, что описательная строка неотделима от тела, ведь это две части единого целого. Поэтому смотрим на этот пример ещё раз:
```cpp
if ( predicat )   // Это начало if-конструкции?
{                 // Или это?
    some_action;   
}                 // Это конец if-конструкции
```
Понимаете мою мысль? Симметричными должны быть начало и конец *всей конструкции*, а не начало и конец её *тела*. Поэтому в примере номер два мы избавляемся от этой неоднозначности:
```cpp
if ( predicat ) { // Это начало if-конструкции 
    some_action;   
}                 // Это конец if-конструкции
```
Здесь закрывающая скобка (обозначающая конец всей конструкции) симметрична с буквой **i** (обозначающей начало всей конструкции).

То же справедливо и для всех остальных конструкций языка (for, class, enum и т.д). 
