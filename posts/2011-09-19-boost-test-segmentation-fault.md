---
title: Boost.Test: ловим Segmentation Fault
tags: boost
---

Я сам удивился, и удивился чрезвычайно. В своё время изрядно намучившись с Segmentation Fault, я был готов дать голову на отсечение, что перехватить эту ошибку на языковом уровне нельзя, поскольку эта ошибка не языковая, строго говоря, а системная. Платформенно-зависимые обработчики сигналов не в счёт, это некрасиво.

Так вот, оказывается, способ есть: Boost.Test Execution Monitor. Сразу пример:

```cpp
struct dangerous_call {
    // Внутри этой функции происходит гарантированная Segmentation Fault.
    int operator()() const {
        int* ip = 0;
        *ip = 1;     // Ахтунг! Разыменование нулевого указателя!
        return 1;
    }
};

int main( int argc, char* argv[] ) {     
    boost::execution_monitor ex_mon;
    try {
        ex_mon.execute( dangerous_call() );
    } catch ( const boost::execution_exception& ex ) {
        // А тут мы перехватываем Segmentation Fault
        // как обыкновенное исключение.
        std::cout << "Caught exception: " << ex.what() << std::endl;
    }
    // Перехватив Segmentation Fault, идём дальше! Процесс не умирает.
    std::cout << "---end of program---" << std::endl;
    return 0;
}
```

Разумеется, перехватив такую ошибку, продолжать выполнение программы *как ни в чём не бывало* неразумно, поскольку для её исправления всё равно придётся править код. Но ценность в том, что перехватив её, можно корректно об этом сообщить, да ещё и провести процедуру некой зачистки процесса, а не так просто, БАЦ - и умерла программа.

Это лишь отдельный пример. Подробнее читайте **<a href="http://www.boost.org/doc/libs/1_47_0/libs/test/doc/html/execution-monitor.html">тут</a>**. Очень рекомендую к ознакомлению!
