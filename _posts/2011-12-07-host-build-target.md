---
layout: post
title: Кросскомпиляция - host/build/target ?
created: 1323215902
categories:
- !binary |-
  0LfQsNC80LXRgtC60Lg=
---
постоянно путаюсь в host/build/target, по этому, запишу :)

<cpp>
build == host           --> нативная компиляция
build != host           --> кросскомпиляция
build == host == target --> нативная компиляция для нативной цели
build == host != target --> нативная компиляция для целевой архитектуры
build != host != target --> Канадский кросс [1]
build != host == target --> кросскомпиляция для нативной цели
</cpp>
где:
<cpp>
build  --> архитектура на которой собираю
host   --> архитектура на которой будет выполняться полученный компилятор
target --> архитектура для которой полученный компилятор будет создавать
</cpp>

пример сборки компилятора в linux, который будет работать на венде, и создавать бинари для мака:
<cpp>
build: i686-pc-linux-gnu
host: i686-pc-mingw32
target: darwin10-osx
</cpp>

и еще несколько примеров найденных с помощью гугления:
<img src="http://clip2net.com/clip/m47996/1330323361-clip-18kb.png" alt="text" />

надеюсь, хоть тут я ничего не попутал %)

[1]
http://airs.com/ian/configure/configure_6.html
http://en.wikipedia.org/wiki/Cross_compiler#Canadian_Cross
