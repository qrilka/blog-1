---
layout: post
title: Haskell: о функциях
category: articles
tags: [Haskell]
comments: true
share: true
---

Если вы помните **<a href="http://dshevchenko.biz/ru/content/haskell-%D0%BE-%D0%BB%D1%8F%D0%BC%D0%B1%D0%B4%D0%B0-%D1%84%D1%83%D0%BD%D0%BA%D1%86%D0%B8%D1%8F%D1%85">заметку о лямбда-функциях</a>**, математическое определение функции очень простое:

**Функция - это описание зависимости чего-то от чего-то.**

Так вот чистые функции в языке Haskell - это и есть функции в математическом смысле. Они представляют собой описание того, *как* входное выражение (или совокупность таковых) определяет выходное выражение. Именно поэтому они не имеют побочных эффектов, и если мы миллион раз подадим на вход одно и то же значение, то на выходе мы миллион раз получим один и тот же ответ.

А теперь за дело.

<h3>Объявляем</h3>

Начинается всё с объявления функции:

```haskell
simple_sum :: Int -> Int
```

Перед нами - объявление выражения. До символа <code>::</code> указывается имя выражения, а после - тип выражения. 

```haskell
simple_sum :: Int -> Int
|        |    |        |
   имя           тип
```

Итак, перед нами выражение с именем <code>simple_sum</code>, имеющее тип функции. Пусть вас не смущают эти слова: в Haskell функция - это тоже тип. Но откуда же мы знаем, что тип этого выражения представляет собой именно функцию? Давайте рассмотрим описание типа подробнее:

```haskell
Int -> Int
```

Обратите внимание на стрелочку. Именно эта стрелочка и говорит нам о том, что перед нами функция. Слева от неё указан тип единственного аргумента функции (в данном случае это стандартный тип **<code>Int</code>**), а справа от неё - тип выходного выражения (тот же **<code>Int</code>**). Саму же стрелочку можно воспринимать как ментальное указание на поток информации через функцию - от входа к выходу, слева направо.

В силу того, что чистая функция в Haskell является математической, невозможно написать аналоги вот таких C-шных функций:

{% highlight cpp %}
void f();
int f();
void f( int i );
```

Чистая функция в Haskell обязана иметь входной аргумент (хотя бы один) и выходное значение. Почему? Потому что это отражает суть математической функции: что-то *обязательно* подаём на вход и что-то *обязательно* получаем на выходе.

Кстати, о количестве аргументнов. Разумеется, чистая функция может принимать несколько аргументов. В этом случае её тип будет выглядеть так:

```haskell
Int -> Int -> Int
```

Я понимаю, выглядит чуток странно, но читать это очень просто: ищем последнюю по счёту (самую правую) стрелочку - она-то и будет тем самым разделителем: слева от неё идёт список типов аргументов, справа - тип возвращаемого выражения:

```haskell
Int -> Int -> Int -> Int
типы аргументов  |  | тип того, что будет на выходе 
```

Итак, с объявлением разобрались. Идём дальше.

<h3>Определяем</h3>

Теперь функцию нужно определить. Кстати, определить нужно *обязательно*. Известно, что в том же C++ мы можем спокойно объявить функцию и не определять её (при условии, что она никогда нигде не вызывается). В Haskell более строгий подход: если объявил - будь добр и определить, в противном случае компилятор выскажет своё недовольство.

Итак, сразу после объявления пишем тело функции:

```haskell
simple_sum :: Int -> Int
simple_sum value = value + value
```

Рассмотрим его подробнее:

```haskell
simple_sum value = value + value
```

Здесь ментальным разделителем является знак равенства. Напоминаю, что в Haskell нет оператора присваивания, поэтому знак равенства здесь - это именно *знак равенства*, как в математике. Скелет данного выражения можно представить так:

```haskell
name arguments = body_expression
```

Здесь:

<ul>
  <li><code>name</code> - имя функции.</li>
  <li><code>arguments</code> - список имён аргументов (именно имён, а не их типов).</li>
  <li><code>body_expression</code> - тело функции.</li>
</ul>

В данном случае у нас имеется один-единственный аргумент по имени <code>value</code>, а также имеется чрезвычайно простое тело, в котором мы просто-напросто складываем аргумент с самим собой.

<h3>Вызываем</h3>

Теперь нашу функцию можно вызывать с аргументом 4 (или, как привычно говорить в мире ФП, апплицировать функцию к аргументу 4):

```haskell
main = putStrLn (show (simple_sum 4))
```

В ответ получаем ожидаемое значение:

<bash>
8
</bash>

<h3>А теперь подробнее...</h3>

Естественно, всё вышесказанное - это лишь самые азы. Теперь же необходимо уточнить некоторые детали.

<h4>Выход из функции</h4>

В C++, если у нас есть функция с возвращаемым значением, мы обязаны где-то в её теле написать инструкцию **<code>return</code>**. Это - точка выхода из тела функции. Кроме того, мы знаем, что точек выхода из тела функции может быть сколько угодно. В Haskell же всё обстоит иначе. Во-первых, точка выхода из чистой функции может быть только одна, а во-вторых, аналога инструкции **<code>return</code>** в Haskell нету.

Да, я понимаю, это удивляет. Мол, а как же нам быть, если мы хотим указать точку выхода явно? Но если мы вспомним математическую природу чистой функции, то поймём, что иначе быть не может. Ведь чистая функция представляет собой описание зависимости выходного выражения от совокупности входных выражений, поэтому её тело представляет собой совокупность выражений, которые вычисляются (в некотором порядке) и в конечном итоге оставляют одно-единственное, последнее выражение. Так вот это последнее выражение и будет являться точкой выхода из функции.

Чтобы стало понятнее, приведу пример:

```haskell
indicate :: String -> String
indicate address = 
    if address == "127.0.0.1"
    then
        "localhost"
    else 
        address
```

Эта функция принимает единственный аргумент типа **<code>String</code>**, соответствующий некоему IP-адресу. В теле функции происходит проверка аргумента на равенство адресу "127.0.0.1", в результате мы оказываемся в одной из двух логических ветвей. Если бы это была функция на C++, это выглядело бы примерно так:

{% highlight cpp %}
std::string indicate( const std::string& address ) {
    if( address == "127.0.0.1" ) {
        return "localhost";
    } else {
        return address;
    }
}
```

Обратите внимание, что мы явно указали две точки выхода из функции. Но в Haskell это не нужно, потому что когда мы окажемся в одной из двух логических ветвей, то выражение, на котором мы окажемся, оно и будет возвращено.

А чтобы стало совсем понятно, перепишем тело этой функции для того, чтобы избавиться от выражения **<code>if then else</code>**:

```haskell
indicate :: String -> String
indicate "127.0.0.1" = "localhost"
indicate address = address
```

Мы вводим вместо одного тела два, для каждого из которых мы определяем свою зависимость. Мы как бы говорим: "Если входной аргумент будет равен "127.0.0.1", то пусть возвращаемым функцией значением будет строка "localhost", если же аргумент будет какой-либо другой, то пусть возвращаемым функцией значением будет сам этот аргумент." Следовательно, когда компилятор увидит вызов этой функции в коде, он просто *подставит* на место этого вызова соответствующее выражение: либо строку "localhost", либо фактически переданный аргумент.

Теперь всё встало на свои места: явно определять точку выхода из чистой функции в Haskell не нужно потому, что конечное выражение в теле этой функции просто заменит собой вызов функции. То есть если написано так:

```haskell
main = putStrLn (indicate "127.0.0.1")
```

то это то же самое, что было бы написано так:

```haskell
main = putStrLn "localhost"
```

потому что компилятор, вычислив внутренности функции, просто заменит место вызова конечным (итоговым) выражением.

<h4>Локальные выражения</h4>

Очень полезно бывает ввести некое локальное значение в теле функции, например, для избавления от дубляжа кода или от магических чисел. Например, у нас есть такая функция:

```haskell
prepare_length :: Double -> Double
prepare_length line = 
    line * 0.4959
```

Итак, мы готовим длину некой линии путём умножения её первоначальной длины на поправочный коэффициент. Но перед нами - магическое число, а это очень плохо. Добавлять комментарий - это не лучшее решение. Поэтому добавим локальное поясняющее выражение:

```haskell
prepare_length :: Double -> Double
prepare_length line = 
    line * coefficient
    where coefficient = 0.4959
```

Красиво, не так ли? Ключевое слово **<code>where</code>** вводит локальное выражение, которое можно использовать в теле функции. Разумеется, локальных выражений может быть несколько:

```haskell
prepare_length :: Double -> Double
prepare_length line = 
    line * coefficient - correction
    where coefficient = 0.4959
          correction = 0.0012
```

Есть ещё один способ ввести локальное вспомогательное выражение, с помощью ключевого слова **<code>let</code>**. На примере нашей последней функции это выглядит так:

```haskell
prepare_length :: Double -> Double
prepare_length line = 
    let coefficient = 12.4959
        correction = 0.0012
    in
    line * coefficient - correction
```

Общая модель такая: **<code>let</code>** <code>bindings</code> **<code>in</code>** <code>expression</code>.

Но у вас, очевидно, возник вопрос, в чём же разница между **<code>where</code>** и **<code>let</code>**?

Во-первых, выражение **<code>where</code>** может быть только одно и только в конце тела функции, в то время как выражение **<code>let</code>** может присутствовать многократно и в любой части тела функции.

Во-вторых, выражение, введённое ключевым словом **<code>where</code>**, всегда видимо в любой точке тела функции, в то время как выражение, введённое ключевым словом **<code>let</code>**, может быть "супер-локальным". Например:

```haskell
prepare_length :: Double -> Double
prepare_length line = 
    let coefficient = 12.4959
        correction = 0.0012
    in
    line * coefficient - correction - (let s = 10.9 in s + 1) - s
```

Понимаю, пример несколько надуманный, но всё-таки... Здесь мы ввели выражение с именем <code>s</code>, которое действует только внутри круглых скобок. Именно поэтому пример этот не скомпилируется, ведь второе выражение <code>s</code> находится уже не в скобках, и поэтому компилятор вполне справедливо возмутится:

<bash>
[2 of 2] Compiling Main             ( src/main.hs, dist/build/Haskell/Haskell-tmp/Main.o )

src/main.hs:43:61: Not in scope: `s'
</bash>

<h4>Про объявление</h4>

Как вы помните, нельзя объявить функцию и при этом не определить её. А можно ли определить функцию без объявления? Отвечаю: можно, но не рекомендуется.

Общепринятой практикой является объявлять функцию и тут же определять её. И несмотря на то, что мы *можем* написать так:

```haskell
prepare_length line = 
    let coefficient = 12.4959
    in
    line * coefficient
```

делать так не рекомендуется, поскольку определение функции становится как бы беднее, ведь сигнатура типов аргументов и возвращаемого выражения помогает лучше понять функцию.

К тому же в случае отсутствия объявления тип аргумента <code>line</code> становится неопределённым, и поэтому строгость проверки этого самого типа (на стадии компиляции) снизится. Если вы делаете это *сознательно* - три раза подумайте. Во всех же остальных случаях -  объявляйте.

<h4>Вот и всё?</h4>

Конечно, это далеко не всё, что можно сказать о чистых функциях, но этот базис достаточен для того, чтобы понять суть чистых функций и то, как их можно использовать в своём коде. Продолжение следует...