---
title: Итераторы: вновь получил по башке
tags: C++
---

Итак, опять эта ошибка... Я уж думал, что никогда больше не наступлю на эти грабли, однако вездесущий copy-paste опять сыграл со мною свою гадкую шутку...

Итак, кусочек кода из Style Revisor, разбирающего Objective-C-шную конструкцию **for...in** :
```cpp
// ...
        auto open_round_bracket_it( formatable.begin() + open_round_bracket_position );
        open_round_bracket_it -= multibyte_ASCII_offset(   formatable.begin()
                                                         , open_round_bracket_it
                                                         , traits );

        from_begin_to_variable_expr.erase(   from_begin_to_variable_expr.begin()
                                           , open_round_bracket_it );
        
        replacer += first_with_comments
                    + from_begin_to_variable_expr;
// ...
```
Ну и глючит это хозяйство, причём "нипадецки": вылезли какие-то дичайшие сдвиги строк в файле, причём с поломкой кодировки.

Ну и в чём же причина? Да проста как две копейки! Алгоритму **erase** в качестве второго аргумента передаётся итератор, указывающий на *другую* строку.

Подобные ошибки весьма любопытны. Начиная с определённого момента своего профессионального развития ты начинаешь думать, что, мол, уж теперь-то я не допущу подобных глупостей. Ан нет... И это отрезвляет...
