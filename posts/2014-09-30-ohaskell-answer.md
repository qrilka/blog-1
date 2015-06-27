---
title: О Haskell по-человечески: немного вопросов и ответов
tags: Haskell
description: Один из моих читателей задал несколько заслуживающих внимания вопросов, касаетельно книги. Я решил ответить на них здесь, на тот случай, если у кого-то возникли такие же...
---

Не так давно один из моих читателей задал мне несколько вопросов относительно книги. И, по его любезному согласию, я публикую ответы здесь, на тот случай, если у кого-то из вас возникли такие же вопросы.

> Хотелось бы, чтобы в книге учитывалась и аудитория читателей, работающих под Windows. :) Соответственно, все консольные команды не плохо было бы давать в двух вариантах: Linux и Windows. При этом на Windows в качестве командной строки можно было бы, при желании, использовать PowerShell.exe (и его команды) вместо старого cmd.exe. Хотя отсутствие этой альтернативы, конечно же не является критичным.

Скажу вам честно, я не стану этого делать. Не считайте мне врединой, просто в моём доме уже давно нет компьютеров с Windows (мы с женой работаем на OS X). И, честно говоря, я очень этому рад: с 2008 года я окончательно перешёл в мир Unix, и возвращаться оттуда не хочу.

> Неплохо было бы в начале книги дать информацию о важности отступов (пробелов и Tab-ов) в файлах исходного кода (*.hs-файлах).  На стр. 19 присутствует замечание об обязательных двух пробелах в файле Real.cabal, но об остальных случаях информация не дана. Например, насколько я вижу, не дана информация как выполнять перенос, если тело функции не помещается в одну строку. Методом тыка можно понять, что в начале переносимой части строки следует поставить пробел (одного у меня было достаточно) или Tab, однако неплохо было бы дать эту информацию и в тексте.

Я рассказываю об этом в главе [О форматировании](http://ohaskell.dshevchenko.biz/ru/miscellaneous/about-formatting.html). Впрочем, вы правы, будет лучше переместить эту главу поближе к началу книги. Учту.

> На мой взгляд, неплохо было бы в тексте книги сравнить размеры полученных простеньких exe файлов, выводящих на консоль обычный "Hello World!", скомпилированных в Haskell и в C\C++. Мне, как читателю, было бы очень интересно прочесть об этом, дабы не заниматься этой проверкой самостоятельно. Как оказалось, разница получается огромная: файл сгенерированный Haskell у меня весил 1 904 Кб, в то время как файл, созданный C++ (Release) весил всего 10 Кб. Т.е. разница получается почти в 200 раз - это очень много. Если есть какой-нибудь способ уменьшить её (может быть за счёт каких-то опций компилятора Haskell), то интересно было бы прочесть об этом в книге. Я пробовал для ghc-options помимо опции -W указывать и -O (оптимизация, насколько я понял из консольной справки), но размер exe файла после перекомпиляции не изменился.

Вы правы, размер исполняемого файла Haskell-приложения действительно тяжеловат. Однако компиляторная оптимизация тут ни при чём, просто по умолчанию в исполняемый файл жёстко линкуются необходимые библиотеки. В этом есть и свой плюс - меньше внешних зависимостей. Однако, если вам непременно нужен маленький .exe-шник, можно скомпилировать его иначе. Подробнее - в нашем [обсуждении на Хабре](http://habrahabr.ru/post/214835/#comment_7384549).

> Неплохо было бы дать информацию о том, что hs-файлы исходного кода, содержащие не англоязычные символы (например кирилицу) нужно сохранять в кодировке UTF-8. Если же таких символов нет, то компилятор Haskell успешно "скушает" и кодировку ANSI.

Согласен, о кодировке и о не-английском тексте в программе необходимо рассказать отдельно. Я планирую выделить под это целую главу.

> На стр. 21 последнее предложение начинается со слова "Теперь". На следующей странице очередное предложение начинается с этого же слова. Хотя второе предложение и числится в новом разделе "Упоминаем", но поскольку текст читается последовательно, то "Теперь-Теперь" чётко бросается в глаза. Во втором предложении лучше заменить это слово чем-то другим.

Поправлю. Кстати, в нескольких главах я заметил несколько форматных ошибок, скоро исправлю.

> Не указано, какие символы, допустимы в имени модуля. Например, на стр. 25 Вы импортируете модуль Data.Text, входящий (как я понял) в состав ранее установленного пакета text. Меня несколько смущает точка... Правильно ли я понимаю, что у такого модуля файл исходного кода будет иметь имя Data.Text.hs, а в самом начале этого файла указана строчка module Data.Text where (по аналогии, как это было сделано для Helpers)? Или же Data - это своего рода некоторое пространство имён, как в C++\C#? 

Я подробно рассказываю об этом в главе [О модулях](http://ohaskell.dshevchenko.biz/ru/miscellaneous/about-modules.html).

> В Visual Studio, при помощи механизма MSBuild.exe (используется за кулисами IDE), имеется возможность создавать конфигурации (x86|x64, Debug|Release и в зависимости от этого - разные настройки опций компилятора и линковщика, а так же постфиксной и префиксной дополнительных операций). Можно ли подобное делать при помощи cabal?

Признаться, не знаю. Кстати, нужно будет изучить этот вопрос.

> Импортируется всё содержимое модуля или же только то, которое реально используется в коде, к которому этот модуль подключается?

И об этом рассказано в главе [О модулях](http://ohaskell.dshevchenko.biz/ru/miscellaneous/about-modules.html).

> На стр. 22 в параметре hs-source-dirs дополнительный каталог указан с новой строки. Однако можно дописывать и в ту же строку через запятую.

Это моя старая привычка, перечислять в столбик. Ведь если каталогов много, то, указывая их через запятую, мы получим длиииную строку.

> Вопрос по поводу ленивых (отложенных) вычислений (стр. 33-35): 

```haskell
main = print (let 
	n = (replicate 100 "127.0.0.1") 
	m = n 
	k = m
 in take 80 n ++
	take 50 m ++
	take 90 k )
```

> Сколько элементов в этом случае Haskell  закрепит за литералами (если я их верно называю) n, m, и k? Это будет один общий массив с 90 элементами (т.е. берётся наибольший вариант из запрошенных), или же это будет три разных массива с 80, 50 и 90 элементами соответственно? Может быть в книге стоит показать и подобный пример (подправив по необходимости)?

Я не знаю. И скажу вам прямо, таких примеров в книге не будет. Дело в том, что я всегда ориентируюсь на реальную программистскую жизнь. А в реальной жизни писать *так* ни один вменяемый разработчик не станет. Конечно, при желании можно "развернуть" эту цепочку и ответить на подобный вопрос, но зачем?

> Любая функция в Haskell, в т.ч. и пользовательская, ленива автоматически, по своей природе? Или же нужно как-то помечать функцию, чтобы Haskell знал, что именно эта функция является ленивой? Исходя из текста раздела "Лень" на стр. 32, я подозреваю, что ленивыми являются все автоматически, но на всякий случай уточняю (мало ли).

Да, по умолчанию, функция в Haskell ленива. Если же мы непременно хотим строгих вычислений (как в C++), мы должны явно об этом попросить: либо использовать строгую версию ленивой функции (многие пакеты в Hackage предоставляют такие), либо воспользоваться оператором строгого применения функции `($!)`. [Подробнее об этом](http://www.haskell.org/haskellwiki/Performance/Strictness).

> А можно ли в лямбда-функции указывать типы её аргументов и возвращаемого значения? Ведь иначе мы получаем функцию без объявления, что само по себе не рекомендуется (см. стр. 44).

Не рекомендуется оставлять *сложную* функцию без объявления. Для тривиальных же случаев сигнатура избыточна, а большинство лямбда-функций крайне тривиальны. И об этом я тоже пишу.

> Стр 50:
этот вызов происходит в два этапа:
1.Функция divide применяется к первому аргументу 10.03 и — внимание! — возвращает функцию типа Double -> Double.
2.Эта возвращённая функция, в свою очередь, применяется ко второму аргументу 2.1 и возвращает конечное значение 4.77.
...
 Функция применяется только к одному значению: сначала к 10.03, а уже потом функция, возвращённая первым вызовом, применяется к 2.1.

> Я не понял этой информации... Как функция divide может примениться к первому аргументу, если во первых, её сигнатура требует двух аргументов, а во вторых - для её выполнения в принципе необходимы два (там ведь деление выполняется). Кроме того, даже если применять функцию к одному аргументу, то его деление на самого себя даёт единицу. Что-то у меня не укладывается в голове этот момент, на нём я застопорился - прошу пояснить его...

Понимаю вас, в начале это действительно удивляет. Как я пишу в главе, посвящённой [функциям высшего порядка](http://ohaskell.dshevchenko.biz/ru/about-functions/higher-order-functions.html), все чистые функции в Haskell применяются только к одному аргументу. Всегда. Поэтому, когда функция `divide` объявлена как принимающая два аргумента, воспринимайте это как синтаксический сахар. Рассмотрим её вызов ещё раз:

```haskell
divide 10.03 2.1
```

Как я уже говорил, подобный вызов происходит в два этапа. На первом этапе происходит вот это:

```haskell
divide 10.03
```

Это совершенно честная запись: функция `divide` применяется к одному-единственному значению. Более того, с точки зрения этой функции, второго значения, равного `2.1`, вообще не существует! Но мы-то с вами знаем, что оно существует и терпеливо ждёт своего часа. Поэтому результатом первого этапа вызова функции `divide` является *частично применённая функция*. Воспринимайте её как чёрный ящичек, в который мы положили первое значение `10.03` и - ничего с ним не сделали, просто сохранили. То есть на первом этапе никакого деления ещё не произошло. А весь фокус в том, что этот чёрный ящичек также является чистой функцией от одного аргумента. Поэтому на втором этапе мы применяем эту чёрно-ящичковую функцию к нашему второму значению, что можно изобразить так:

```haskell
black_box 2.1
```

И благодаря тому, что внутри этого ящичка сохранено первое значение, здесь и происходит маленькое волшебство: функция `black_box` берёт сохранённое в ней значение `10.03` и переданное ей значение `2.1` и наконец-то совершает нужное нам деление, возвращая результат оного в виде числа типа `Double`. В этом и заключается магия каррирования: сколько бы аргументов ни принимала исходная фукнция, её вызов всегда сводится к пошаговому накапливанию значений аргументов, через механизм частичного применения. И только после того, как были накоплены *все* необходимые значения, происходит, собственно, вызов функции (то есть производится та реальная работа, ради которой эта функция и была создана).

Вы спросите, а как именно происходит это накопление? Ведь если вызов `divide` произошёл в два этапа, то где и как было сохранено первое значение `10.03` между этими этапами? Отвечаю: это неважно. Всё это, по сути, подкапотные дела, о которых программист не обязан ни заботиться, ни даже знать. Что говорит нам сигнатура функции `divide`? Что она работает с двумя значениями. Передали ей два значения - и получаем результат деления. Дело сделано. 

Ещё раз напоминаю, что ваши вопросы и пожелания очень важны для развития книги. Так что не стесняемся и спрашиваем. Можно лично мне, а можно в комментариях.
