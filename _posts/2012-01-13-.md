---
layout: post
title: Логические флаги в сигнатуре функции - не делайте этого!
created: 1326400264
categories:
- !binary |-
  0J/RgNC+0LPRgNCw0LzQvNC40YDQvtCy0LDQvdC40LU=
---
Ух, наболело...

Как вам вызов такой функции:
{% highlight cpp %}
auto handler = create_packets_handler( channel, true );
{% endhighlight %}

Знаете, я когда вижу подобную функцию, мне хочется закричать громким-прегромким голосом... Вот скажите, что значит true? Да, я понимаю, что вторым аргументом функция, очевидно, принимает значение типа bool. Но ёлки-палки, что значит это bool по своей сути?? Да ничего оно не значит.

Поэтому я вынужден смотреть объявление этой функции, в надежде на то, что автор хотя бы там указал значение этого флага. К сожалению, и это не всегда бывает, так что приходится, ещё раз крикнув громким голосом, смотреть уже определение функции.

Все известные мне корифеи C++ (и не только C++) много раз говорили о том, что логические флаги в качестве аргументов функции - это зло, Зло и ещё раз **ЗЛО**. Да только всё не впрок, как в той басне...

Ладно, хватит ругаться, давайте лучше рассмотрим варианты избавления от этого кошмара.

Для примера допустим, что изначальная сигнатура функции такова:
{% highlight cpp %}
handler create_packets_handler( packets_channel const& channel
                                , bool async = false );
{% endhighlight %}

Вариант первый, самый дубовый:
{% highlight cpp %}
bool const make_async = true;
auto handler = create_packets_handler( channel, make_async );
{% endhighlight %}
Уже значительно лучше. При первом же взгляде на такой вызов мы понимаем предназначение этого флага. Этот вариант не требует изменения сигнатуры функции.

Вариант второй, получше. Сигнатуру меняем:
{% highlight cpp %}
handler create_packets_handler( packets_channel const& channel
                                , handler::type = make_sync );
{% endhighlight %}
Итак, мы избавились от типа bool и добавили в класс handler enum с двумя значениями. Поэтому при вызове пишем:
{% highlight cpp %}
auto handler = create_packets_handler( channel, handler::make_async );
{% endhighlight %}
Неплохо.

Но третий вариант - самый лучший (на мой взгляд). Сигнатуру меняем кардинально и просто:
{% highlight cpp %}
handler create_sync_packets_handler( packets_channel const& channel );
handler create_async_packets_handler( packets_channel const& channel );
{% endhighlight %}
Итак, раз уж возможны лишь два состояния обработчика, синхронный и асинхронный, тогда избавимся от переключателя вовсе, и введём две понятные функции. Никаких bool и никаких enum.

Вот вроде бы простые вещи говорю, никому Америку не открываю... Эх... 
