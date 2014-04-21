---
layout: post
title: Пользователям boost::ptr_container, важно!
created: 1316355702
categories:
- boost
---
<!--break-->
Недавно я потерял добрую половину рабочего дня на исправление одной хитрой ошибки при использовании boost::ptr_vector.

Итак, имеется следующий код:
{% highlight cpp %}
struct A {
    A( int ii ) : i( ii ) {}
    int i;
    bool operator==( int ii ) { return ii == i; }
};

int main() {
    typedef boost::ptr_vector< A > V;
    V v;
    v.push_back( new A( 1 ) );
    v.push_back( new A( 2 ) );
    v.push_back( new A( 3 ) );
    // Теперь удаляем значение по предикату (фактически, по оператору проверки на равенство).
    std::cout << "before: " << v.size() << std::endl;
    v.erase( std::remove( v.begin(), v.end(), 2 ), 
             v.end() );
    std::cout << "after: " << v.size() << std::endl;
    // Теперь проверяем, остался ли в контейнере объект со значением 2.
    V::iterator it = std::find( v.begin(), v.end(), 2 );
    if ( v.end() != it ) {
        std::cout << "2 - yes" << std::endl;
    } else {
        std::cout << "2 - no" << std::endl;
    }
    return 0;
}
{% endhighlight %}

Казалось бы, тривиальнее этого кода и придумать тяжело. Запускаем программу. Вывод:
{% highlight cpp %}
before: 3
2 - no
after: 2
{% endhighlight %}

Ну, казалось бы, что тут интересного? Было три объекта, один удалили, потом даже проверили факт удаления, и размер контейнера уменьшился, как и ожидалось, на 1.

В абсолютной уверенности о корректности данного кода пребывал и я. До тех пор, пока моя программа не начала вдруг ни с того, ни с сего падать с Segmentation Fault.

Вы будете удивлены, но данный код работает неправильно. Он прекрасно компилируется и запускается, однако его поведение вовсе не такое, как мы ожидаем. И для того, чтобы в этом убедиться, определим деструктор в структуре А с меткой, а также пометим момент удаления в main().
{% highlight cpp %}
struct A {
    A( int ii ) : i( ii ) {}
    ~A() {
        std::cout << "what erased, i: " << i << std::endl;
    }
    int i;
    bool operator==( int ii ) { return ii == i; }
};

int main() {
    // то же самое...
    v.erase( std::remove( v.begin(), v.end(), 2 ), 
             v.end() );
    // Перед этой меткой мы увидим, как вызывается деструктор объекта со значением 2? Ну-ну...
    std::cout << "-----------" << std::endl;
    // то же самое...
}
{% endhighlight %}

Теперь снова запустим программу. Вывод:
{% highlight cpp %}
before: 3
what erased, i: 3
2 - no
----------------
after: 2
what erased, i: 1
what erased, i: 3
{% endhighlight %}

Ой! Итак, видим следующие странности:
<ol>
 <li>Непонятно с какой радости уничтожается объект со значением 3 (он же последний).</li>
 <li>Деструктор этого объекта вызывается дважды (в момент erase-remove, а также при выходе из программы).</li>
 <li>Не видим, чтобы вызвался деструктор объекта со значением 2, хотя именно этого мы и ожидали.</li>
 <li>Если объект со значением 2 не уничтожился (то есть остался в контейнере), до почему проверочный поиск показывает, что этого объекта нет?</li>
</ol>

Чтобы запутать вас окончательно, добавлю поиск объекта со значением 3.
{% highlight cpp %}
int main() {
    // то же самое...
    v.erase( std::remove( v.begin(), v.end(), 2 ), 
             v.end() );
    // то же самое...
    // Как мы убедились, объект со значением 3 почему-то удаляется.
    // Получается, он уже не будет найден, не так ли? 
    V::iterator it = std::find( v.begin(), v.end(), 3 );
    if ( v.end() != it ) {
        std::cout << "3 - yes" << std::endl;
    } else {
        std::cout << "3 - no" << std::endl;
    }
    // то же самое...
}
{% endhighlight %}

Запускаем. Вывод:
{% highlight cpp %}
before: 3
what erased, i: 3
2 - no
3 - yes
----------------
after: 2
what erased, i: 1
what erased, i: 3
{% endhighlight %}

Ещё глупее. Если объект со значением 3 удаляется, то почему же он остался в контейнере? Однако вы можете быть уверены - объекта со значением 3 уже нет (деструктор не обманывает). Если не верите - попробуйте обратиться к нему, и вас ожидает неприятный сюрприз...

Чтобы разобраться с данной проблемой, необходимо внимательнее почитать документацию на boost::ptr_container. Читаем и находим там такую строчку:

**Unfortunately it is not possible to use pointer containers with mutating algorithms from the standard library.**

Оказывается, использовать **изменяющие** стандартные алгоритмы совместно с ptr_container нельзя! Вместо этого необходимо использовать *встроенные* алгоритмы.
Определим более читабельный предикат в структуре А и перепишем алгоритм.
{% highlight cpp %}
struct A {
    A( int ii ) : i( ii ) {}
    int i;
    bool operator==( int ii ) { return ii == i; }
    bool correspond( int ii ) { return ii == i; }
};

int main() {
    typedef boost::ptr_vector< A > V;
    V v;
    v.push_back( new A( 1 ) );
    v.push_back( new A( 2 ) );
    v.push_back( new A( 3 ) );
    v.erase_if( boost::bind( A::correspond, _1, 2 ) );
    // Будьте уверены - теперь работает правильно. 
    return 0;
}
{% endhighlight %}

Итак, используя библиотеку, будьте внимательны, и не пренебрегайте документацией.
