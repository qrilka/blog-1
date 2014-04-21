---
layout: post
title: 'Данные заранее: я передумал...'
created: 1343918248
categories:
- !binary |-
  0J/RgNC+0LPRgNCw0LzQvNC40YDQvtCy0LDQvdC40LU=
---
Эта задача возникала передо мною часто. А сейчас, в коде Style Revisor, я вдруг... передумал.

Каждый языковой плагин содержит ряд функторов, отвечающих за языковые конструкции. Скажем, функтор **if_else** отвечает за конструкцию <tt>if-else</tt>, функтор **for** - за конструкцию <tt>for</tt>, и т.д. И мой канонический подход был раньше таким:
{% highlight cpp %}
struct plugin::implementation {
    implementation() {
        // Итак, наполняю контейнер заранее,
        // причём все типы сохраняемых функторов являются
        // наследниками от типа language_structure, для удобства.
        structures.push_back( new if_else_structure() );
        structures.push_back( new do_while_structure() );
        // и т.д...
    }
private:
    boost::ptr_vector <language_structure> structures;
public:
    void revise() {
        for( auto structure : structures ) {
            structure.some_work( /*  */ );
        }
    }
};
{% endhighlight %}Ну и потом я вдруг подумал: а зачем мне заранее сохранять функторы-обработчики, если можно использовать их на месте и без этого?

Разумеется, если каждый из функторов-обработчиков был бы сложным в создании - тогда да: заранее создал, сохранил указатель - и работай. Но если каждый из этих функторов является *чистым*, тогда это совершенно не нужно:
{% highlight cpp %}
struct plugin::implementation {
public:
    void revise() {
        for( auto structure : structures ) {
            if_else_structure{}.some_work( /*  */ );
            do_while_structure{}.some_work( /*  */ );
        }
    }
};
{% endhighlight %}То есть прямо тут, в месте использования, создаю функторы, использую и сразу удаляю. Зачем заранее-то их сохранять, беспокоясь о жизненном цикле, месте инициализации и т.п?
