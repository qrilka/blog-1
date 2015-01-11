---
title: Haskell: собираем простейший проект
tags: Haskell, cabal
---

За основу я взял хорошее пошаговое руководство с **<a href="http://www.haskell.org/haskellwiki/How_to_write_a_Haskell_program#Structure_of_a_simple_project">официального сайта</a>** Haskell, и снабдил его некоторыми пояснениями. Не пугайтесь, что дальше идёт довольно много текста, потому что фактических команд, которые вам нужно будет выполнить в терминале, всего несколько. Объём этой статьи обусловлен подробностью моих комментариев. 

Итак, подразумевается, что у вас уже установлена **<a href="http://www.haskell.org/platform/">Haskell Platform</a>**. В составе этой самой платформы присутствует мощная утилита под названием <code>cabal</code>. Не знаю как вам, а мне это название напомнило о персонаже из кровопролитной игры Mortal Kombat по имени Kabal. Впрочем, не пугайтесь, утилита эта не станет набрасываться на вас с железными крюками. Фактически она представляет собою эдакий Haskell-овский <code>make</code>. На самом деле эта утилита весьма мощная и многогранная в своих возможностях, но на данный момент просто воспринимайте её как сборочную утилиту специально для Haskell-проектов.

Создаём папку под названием Haskell и заходим в неё. В ней мы должны создать один файл: main.hs. Это наш файл исходного кода, в нашем простейшем случае других файлов не будет.

Открываем этот файл и пишем в нём, например, это:

```haskell
import System.Environment
 
main :: IO ()
main = getArgs >>= print . haqify . head
 
haqify s = "Haq! " ++ s
```

Сохраняем, закрываем, а потом выполняем в терминале простую команду:

<bash>
cabal init
</bash>

В результате вы попадёте в интерактивный диалог, в ходе которого вам будет предложено ответить на несколько вопросов о вашем проекте:

<bash>
MacBook-Pro-Sevcenko:Haskell dshevchenko$ cabal init
Package name? [default: Haskell] 
Package version? [default: 0.1.0.0] 
Please choose a license:
 * 1) (none)
   2) GPL-2
   3) GPL-3
   4) LGPL-2.1
   5) LGPL-3
   6) BSD3
   7) MIT
   8) Apache-2.0
   9) PublicDomain
  10) AllRightsReserved
  11) Other (specify)
Your choice? [default: (none)] 7
Author name? Denis Shevchenko
Maintainer email? me@dshevchenko.biz
Project homepage URL? 
Project synopsis? Learn project
Project category:
 * 1) (none)
   2) Codec
   3) Concurrency
   4) Control
   5) Data
   6) Database
   7) Development
   8) Distribution
   9) Game
  10) Graphics
  11) Language
  12) Math
  13) Network
  14) Sound
  15) System
  16) Testing
  17) Text
  18) Web
  19) Other (specify)
Your choice? [default: (none)] 
What does the package build:
   1) Library
   2) Executable
Your choice? 2
Include documentation on what each field means (y/n)? [default: n] 

Guessing dependencies...

Generating LICENSE...
Warning: unknown license type, you must put a copy in LICENSE yourself.
Generating Setup.hs...
Generating Haskell.cabal...

You may want to edit the .cabal file and add a Description field.
</bash>

В результате в вашей папке будут созданы несколько дополнительных файлов. Не знаю, почему, но с автоматическим генерированием файла LICENSE произошла проблемка. Как видим, мы получили предупреждение вида:

<bash>
Generating LICENSE...
Warning: unknown license type, you must put a copy in LICENSE yourself.
</bash>

Совершенно непонятно, *что* не понравилось утилите, ведь я ввёл номер лицензии 7 (что соответствует лицензии MIT) и она это проглотила, но в итоге файл LICENSE в корневой папке проекта так и не появился. Но дабы не было противоречий, просто добавьте этот файл вручную (и можете оставить его пустым).

Как уже было упомянуто, в корневой папке проекта появились следующие важные файлы:

<ul>
  <li>Setup.hs</li>
  <li>Haskell.cabal</li>
</ul>

Первый нам не так интересен, потому что (в соответствии с официальным руководством) редактировать этот файл руками нам придётся очень редко. А вот второй файл весьма любопытен. Откроем его и посмотрим:

```haskell
name:                Haskell
version:             0.1.0.0
synopsis:            Learn project
-- description:         
license:             MIT
license-file:        LICENSE
author:              Denis Shevchenko
maintainer:          me@dshevchenko.biz
-- copyright:           
-- category:            
build-type:          Simple
cabal-version:       >=1.8

executable Haskell
  main-is:           main.hs            
  -- other-modules:       
  build-depends:     base ==4.6.*
```

Как видите, этот файл представляет собой некий аналог Makefile для системы cabal. Тут уже сохранены те самые значения, которые мы вводили в процессе вышеупомянутого диалога.

Обращаю ваше внимание на то, что в этом сгенерированном файле изначально строка:

```haskell
  main-is:
```

закомментирована. Вам необходимо раскомментировать её и прописать имя файла, содержащего функцию main, в нашем случае это main.hs. Если вы этого не сделаете, получите ошибку вида:

<bash>
Error: No 'Main-Is' field found for executable Haskell
</bash>

Итак, после этого выполняем следующую простую команду в терминале:

<bash>
cabal configure
</bash>

В результате произойдёт конфигурирование проекта, то есть некая подготовка к фактической сборке. И тут нас ожидает следующая тонкость.

Обратите внимание на последнюю часть файла Haskell.cabal:

```haskell
executable Haskell
  main-is:           main.hs            
  -- other-modules:       
  build-depends:     base ==4.6.*
```

Видите отступ в два пробела перед тремя последними строками? Оказывается, это не просто так. Если мы уберём этот отступ, то получим такую ошибку:

<bash>
cabal: Haskell.cabal:18: Construct not supported at this position: F 18
"main-is" "main.hs\nbuild-depends: base ==4.6.*"
</bash>

Кроме того, отступ должен быть никак не меньше двух пробелов, потому что ради эксперимента я пробовал поставить отступ из одного-единственного пробела, и получил уже такую ошибку:

<bash>
cabal: Haskell.cabal:18: Parse of field 'main-is' failed.
</bash>

В общем, два пробела. В этом случае конфигурирование пройдёт нормально.

<bash>
$ cabal configure
Resolving dependencies...
Configuring Haskell-0.1.0.0...
</bash>

После этого выполняем, собственно, команду сборки:

<bash>
$ cabal build
Building Haskell-0.1.0.0...
Preprocessing executable 'Haskell' for Haskell-0.1.0.0...
</bash>

Всё. После этого в нашей корневой папке проекта появилась папка <code>dist</code>. Вот её содержимое:

<bash>
dist/
├── build
│   ├── Haskell
│   │   ├── Haskell    <<---- Это и есть наш исполняемый файл.
│   │   └── Haskell-tmp
│   │       ├── Main.hi
│   │       └── Main.o
│   └── autogen
│       ├── Paths_Haskell.hs
│       └── cabal_macros.h
├── package.conf.inplace
└── setup-config
</bash>

Ну вот собственно и всё. Можем запускать наше приложение:

<bash>
./dist/build/Haskell/Haskell arg1
</bash>

Вывод будет таким:

<bash>
"Haq! arg1"
</bash>

В дальнейшем я обязательно продолжу рассказы об утилите <code>cabal</code>, но главное, что теперь вы имеете представление о том, как в Haskell создать пусть и простейший, но уже реальный проект.
