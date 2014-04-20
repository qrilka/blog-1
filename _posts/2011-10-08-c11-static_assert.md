---
layout: post
title: 'C++11: static_assert'
created: 1318068225
categories:
- static_assert
- c++11
---
<!--break-->
Представим, что у нас есть такая функция:

<cpp>
template <typename Number>
std::string as_binary( const Number number ) {
    std::string representation;
    // Представляем number в бинарно-строковом виде, не важно как...
    return representation;
}
</cpp>

Но у нас есть техническое ограничение: функция <strong>as_binary()</strong> не может принимать очень большое число, а только <strong>int</strong> или меньше. Как оградить функцию от случайной передачи чрезмерно больших чисел?

Классическое решение - с помощью assert().

<cpp>
template <typename Number>
std::string as_binary( const Number number ) {
    assert ( sizeof (T) <= sizeof (int) && "Sorry, too big number!" )
    std::string representation;
    // Представляем number в бинарно-строковом виде, не важно как...
    return representation;
}
</cpp>

Ну, или с помощью BOOST_ASSERT, не суть важно. Важно то, что у этого решения есть один минус - такая проверка работает только на стадии выполнения программы. А мы знаем, что обнаружить ошибку на стадии компиляции гораздо предпочтительнее.

На помощь нам приходит <strong>static_assert</strong>.

<cpp>
template <typename Number>
std::string as_binary( const Number number ) {
    static_assert ( sizeof (T) <= sizeof (int), "Sorry, too big number!" )
    std::string representation;
    // Представляем number в бинарно-строковом виде, не важно как...
    return representation;
}
</cpp>

Выражение <strong>static_assert</strong> принимает условие (которое, разумеется, можно проверить на стадии компиляции), а также строковый литерал (который будет выведен в случае невыполнения этого условия). Просто, понятно и полезно.

<em>P.S. Разумеется, есть важное "НО": пока шаблон функции as_binary() не инстанцирован, static_assert не сработает. Иными словами, если мы нигде в коде не вызвали эту функцию, то и проверка не произойдёт. Хотя если проверка находится, скажем, в простой функции, то, поскольку её код полноценно компилируется независимо от того, используется ли функция в коде, проверка гарантированно произойдёт.</em>
