---
title: Objective-C и Boost.Xpressive: конфликт слов
tags: Objective-C, boost
---

Увы и ах, но испльзование некоторых из библиотек Boost недопустимо в Objective-C++ коде, как я смог убедиться в этом буквально вчера.

У меня в коде приложения StrictCode использовалась Boost.Xpressive. И в процессе переноса парсингового кода из Ревизора вдруг вылетела (в огромных количествах) следующая ошибка:
```cpp
/usr/local/include/boost/fusion/container/list/cons_fwd.hpp:13:5: Declaration of anonymous struct must be a definition
```

А вот и проблемный код:
```cpp
namespace boost { namespace fusion
{
    struct nil;      <<<<<<<<<<< Ругается на эту строчку...

    template <typename Car, typename Cdr = nil>
    struct cons;
}}
```

Ну, казалось бы, а в чём проблема?** struct nil **предварительно объявляется, как это часто делается в С++. А проблема в том, что слово **nil** в языке Objective-C является одним из ключевых. Естественно, компилятор не может это проглотить, как скажем, не смог бы проглотить строку вида:
```cpp
struct void;
```

Кто же в этом виноват? Да никто. Авторы библиотек Boost не обязаны ориентироваться на ключевые слова других языков... 
