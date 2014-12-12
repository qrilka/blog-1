---
title: Buildbot: CI-инструмент, который мне понравился
tags: Buildbot, CI
description: Buildbot - это CI-инструмент, который мне понравился. Расскажу о том, как его вкусно приготовить. 
---

Приветствую, друзья!

Не так давно понадобилось мне настроить CI. Как вы знаете, на сегодняшний день CI для веб-приложений - это нечто само собою разумеющееся, однако мне предстояло выбрать, ибо CI-инструментов достаточно много. Коммерческие монстры типа TeamCity были отвергнуты сразу, ибо они, во-первых, слишком тяжеловесны, а во-вторых, весьма дороги. После долгих блужданий по Сети я выбрал [Buildbot](http://buildbot.net). И не пожалел.

Я не стану нахваливать эту систему и кричать о том, какая она великолепная, сексуальная и т.п. Более того, по сравнению с той же TeamCity Buildbot выглядит как студенческое поделие (по крайней мере, с точки зрения GUI). Однако я искал максимально простой и ясный в использовании инструмент, и именно своей простотой Buildbot покорил меня.

Итак, к делу.

## Начало

Идём на [официальный сайт](http://buildbot.net), оглядываемся, а затем переходим [сюда](http://trac.buildbot.net/wiki/DownloadInstall). Здесь мы узнаём две вещи: во-первых, Buildbot написан на Python, а во-вторых, установить его предельно просто.

Вначале ставим Python-овский разработческий пакет. Для Ubuntu 14.04 это будет:

```bash
$ sudo apt-get install python-dev
```

Затем устанавливаем сам Buildbot:

```bash
$ sudo easy_install buildbot
```

Немного терпения - и готово. Но прежде чем продолжить, объясню фундаментальную идею Buildbot.

## Идея

Всё предельно просто: есть один хозяин (master) и N-дцать подключенных к мастеру слуг (slave). Хозяин - это центр системы. Именно он подлежит настройке, и именно он связан с веб-фронтендом. Слуги - это отдельные процессы, подключающиеся к хозяину и выполняющие роль сборщиков. По аналогии с TeamCity, слугу можно считать билд-агентом. Таким образом, настроенный Buildbot - это один хозяин и хотя бы один слуга. Вот, собственно, и всё.

## Создаём хозяина

Выполняем:

```bash
$ cd
$ mkdir tmp
$ buildbot create-master tmp/mhelper_buildmaster
```

Не обращайте внимания на то, что каталог называется `tmp`. Это было взято мною из документации.

После выполнения этой команды у нас появился каталог `~/tmp/mhelper_buildmaster`. Далее делаем следующее:

```bash
$ cd tmp/mhelper_buildmaster
$ mv master.cfg.sample master.cfg
```

Файл `master.cfg` - это единственный конфигурационный файл нашего CI-инструмента. К его редактированию мы вернёмся чуток позже.

## Создаём слуг

Моё веб-приложение представлено четырьмя независящими друг от друга стендами. Эти стенды ассоциированы с привычным для программистов жизненным циклом, выраженным в версиях (`alpha`, `beta`, `rc` и `stable`).

В связи с такой схемой я решил, что логично будет создать четыре слуги вместо одного. Итак:

```bash
$ cd
$ sudo easy_install buildbot-slave
$ buildslave create-slave tmp/slave_alpha localhost:9989 slave_alpha slave_alpha!
$ buildslave create-slave tmp/slave_beta localhost:9989 slave_beta slave_beta!
$ buildslave create-slave tmp/slave_rc localhost:9989 slave_rc slave_rc!
$ buildslave create-slave tmp/slave_stable localhost:9989 slave_stable slave_stable!
```

Готово. Теперь у нас есть четыре слуги с именами `slave_alpha`, `slave_beta`, `slave_rc` и `slave_stable`. Разберём параметры этих команд, на примере первой:

```bash
$ buildslave create-slave tmp/slave_alpha localhost:9989 slave_alpha slave_alpha!
```

Первый аргумент - это базовый каталог слуги, в данном случае `~/tmp/slave_alpha/`. Вторым аргументом задаётся точка подключения к (уже созданному нами) хозяину. По умолчанию хозяин доступен на `localhost` через порт `9989` (при желании порт можно будет изменить). Третий аргумент - имя (логин) слуги, `slave_alpha`. Четвёртым аргументом идёт пароль слуги, в данном случае `slave_alpha!`. Как вы уже поняли, имя и пароль слуги используются для того, чтобы аутентифицировать слугу в глазах хозяина.

## Первичная настройка хозяина

Помните файл `~/tmp/mhelper_buildmaster/master.cfg`? Пришла пора взяться за него. Что мне понравилось в Buildbot, так это то, что у него один-единственный конфигурационный файл, в котором задаётся всё, что нам понадобится для работы.

Откроем его и найдём в нём секцию `BUILDSLAVES`. В ней задан список слуг, начинающийся с команды:

```python
c['slaves'] = ...
```

В нашем случае, учитывая четырёх созданных слуг, эта строка будет выглядеть так:

```python
c['slaves'] = [BuildSlave("slave_alpha", "slave_alpha!"),
               BuildSlave("slave_beta", "slave_beta!"),
               BuildSlave("slave_rc", "slave_rc!"),
               BuildSlave("slave_stable", "slave_stable!")]
```

Я думаю, подробных пояснений не нужно: перед нами список из четырёх пар "логин/пароль". Как видите, здесь указаны те самые логины и пароли, которые задавались при создании слуг.

Теперь выполняем:

```bash
$ cd
$ buildbot reconfig tmp/mhelper_buildmaster
```

Поскольку мы изменили конфигурационный файл, нам нужно переконфигурировать нашу систему. И мы будем делать это всякий раз, когда вносим изменения в файл `~/tmp/mhelper_buildmaster/master.cfg`.

## Первый запуск

Запускаем хозяина:

```bash
$ buildbot start tmp/mhelper_buildmaster/
```

Хозяин ожил, пришло время запустить наших слуг:

```bash
$ buildslave start tmp/slave_alpha/
$ buildslave start tmp/slave_beta/
$ buildslave start tmp/slave_rc/
$ buildslave start tmp/slave_stable/
```

Готово. По умолчанию, веб-интерфейс доступна через порт 8010, поэтому набираем в браузере `http://localhost:8010` и любуемся. Да, интерфейс очень прост, но зато понятен.

## Настоящая настройка

Естественно, на данный момент система не представляет для нас никакой ценности. Пришла пора поглужбе разобраться в конфигурационном файле.

### Знакомимся с проектом

Прежде всего познакомим Buildbot с нашим проектом. Откроем файл `~/tmp/mhelper_buildmaster/master.cfg` и найдём в нём секцию `PROJECT IDENTITY`. Пишем:

```python
c['title'] = "My project's real name"
c['titleURL'] = "http://my-project.com"
```

Кстати, если по какой-то причине нас не устраивает умолчальный порт `8010` (например, у меня он был уже занят), то в этой же секции указываем другой:

```python
c['buildbotURL'] = "http://localhost:8020/"
```

### Указываем пользователей

Очевидно, что работать с нашей системой должны лишь те, кому это позволено. Укажем наших будущих пользователей-разработчиков. Находим секцию `STATUS TARGETS` и меняем там следующее:

```python
users = [("denis.shevchenko","super_password")]

authz_cfg = authz.Authz(
    # change any of these to True to enable; see the manual for more
    # options
    auth = auth.BasicAuth(users),
    view = 'auth',
    gracefulShutdown = False,
    forceBuild = 'auth', # use this to test your slave once it is set up
    forceAllBuilds = 'auth',  # ..or this
    pingBuilder = False,
    stopBuild = True,
    stopAllBuilds = False,
    cancelPendingBuild = True,
)
c['status'].append(html.WebStatus(http_port=8020, authz=authz_cfg))
```

Значение `users` - это список пар "логин/пароль" для наших пользователей. Как видите, пока там задан только я. Далее обращаю ваше внимание на строку:

```python
    view = 'auth',
```

Этой строки нет в умолчальном конфиге, но она очень полезна. Без неё веб-интерфейс будет открыть всему миру, и каждый сможет увидеть наши сборки и прочую информацию. Поэтому мы и добавили эту строку: с ней информация о сборках и настройках будет видна только авторизованным пользователям.

Также обращаю ваше внимание на строку:

```python
c['status'].append(html.WebStatus(http_port=8020, authz=authz_cfg))
```

Видите значение `http_port`? Если необходимо, измените его в соответствие со значением порта в параметре `c['buildbotURL']`.

Теперь пользователи, зайдя на главную страницу сервиса, смогут авторизоваться, введя указанные здесь логин и пароль в верхнем правом углу.

### Указываем наши исходники

Используемый мною CI-воркфлоу предельно прост: `git push` в соответствующую ветку, и ассоциированный с нею стенд автоматически обновляется.

Например, для версии `alpha` есть стенд `http://alpha.my-service.com`. Таким образом, чтобы пересобрать этот стенд, нужно запушить изменения в одноимённую ветку `alpha` (далее я подразумеваю, что репозиторий у вас уже есть и упомянутые здесь ветки уже созданы).

Теперь находим секцию `CHANGESOURCES` и пишем там следующее:

```python
from buildbot.changes.gitpoller import GitPoller

poll_interval_in_seconds = 60

git_poller = GitPoller(repourl = 'git@localhost:service/mhelper.git',
                       workdir = 'gitpoller-workdir',
                       branches = ['alpha', 'beta', 'rc', 'stable'],
                       pollinterval = poll_interval_in_seconds)

c['change_source'] = [git_poller]
```

`git_poller` - наш верный сторожевой пёс. Каждый 60 секунд он опрашивает четыре ветки в нашем репозитории.

Обратите внимание на необычный URL репозитория `git@localhost:service/mhelper.git`. Дело в том, что мой репозиторий уютно живёт в [GitLab](https://about.gitlab.com), а этот GitLab физически установлен на той же разработческой виртуалке, где и Buildbot. Поэтому я создал беспарольный ssh-ключик, добавил его в GitLab (через его веб-интерфейс), и теперь пользователь `denis` имеет право выполнять команды для работы с репозиторием от имени пользователя `git`. Напомню, что пользователь `git` автоматически создаётся при установке GitLab.

### Планировщики

Теперь находим секцию `SCHEDULERS` и пишем:

```python
alpha_scheduler = SingleBranchScheduler(name = "alpha_scheduler",
                                        change_filter = filter.ChangeFilter(branch = 'alpha'),
                            		    treeStableTimer = None,
                            	 	    builderNames = ["alpha_builder"])

beta_scheduler = SingleBranchScheduler(name = "beta_scheduler",
                           	           change_filter = filter.ChangeFilter(branch = 'beta'),
                                       treeStableTimer = None,
                            	       builderNames = ["beta_builder"])

rc_scheduler = SingleBranchScheduler(name = "rc_scheduler",
                            	     change_filter = filter.ChangeFilter(branch = 'rc'),
                            	     treeStableTimer = None,
                            	     builderNames = ["rc_builder"])

stable_scheduler = SingleBranchScheduler(name = "stable_scheduler",
                            		 change_filter = filter.ChangeFilter(branch = 'stable'),
                            		 treeStableTimer = None,
                            		 builderNames = ["stable_builder"])

c['schedulers'] = [alpha_scheduler, beta_scheduler, rc_scheduler, stable_scheduler]
```

Здесь мы создаём четыре планировщика. Каждый из них работает со своей версией (веткой), и связан со своим билдером. 

### Сборщики

Итак, мы дошли до самого интересного. Находим секцию `BUILDERS` и пишем:

```python
alpha_factory = BuildFactory()
alpha_factory.addStep(ShellCommand(command = ["mhelper_update.sh", "alpha"]))

beta_factory = BuildFactory()
beta_factory.addStep(ShellCommand(command = ["mhelper_update.sh", "beta"]))

rc_factory = BuildFactory()
rc_factory.addStep(ShellCommand(command = ["mhelper_update.sh", "rc"]))

stable_factory = BuildFactory()
stable_factory.addStep(ShellCommand(command = ["mhelper_update.sh", "stable"]))
```

Мы создали четыре сборочные фабрики. Фабрика - это набор шагов, которые, по нашему замыслу, должны выполниться тогда, когда в одной из наших веток произойдёт некое изменение. Как видите, в каждую из фабрик мы добавили по одному-единственному шагу (с помощью функции `addStep`). В данном случае шаг - это выполнение одной-единственной shell-команды. Buildbot поддерживает достаточно много различных видов шагов, о чём подробнее рассказано [здесь](http://docs.buildbot.net/0.8.9/manual/cfg-buildsteps.html).

Команда, выполняемая каждой из фабрик - это вызов скрипта `mhelper_update.sh`, с передачей оному одного из четырёх аргументов (как видите, это имена версий). Об этом чуть позже.

Теперь создаём четыре сборщика:

```python
alpha_builder = BuilderConfig(name = "alpha_builder",
      			              slavenames = ["slave_alpha"],
                              factory = alpha_factory)

beta_builder = BuilderConfig(name = "beta_builder",
      			             slavenames = ["slave_beta"],
                             factory = beta_factory)

rc_builder = BuilderConfig(name = "rc_builder",
      			           slavenames = ["slave_rc"],
                           factory = rc_factory)

stable_builder = BuilderConfig(name = "stable_builder",
       	       		           slavenames = ["slave_stable"],
                               factory = stable_factory)

c['builders'] = [alpha_builder, beta_builder, rc_builder, stable_builder]
```

Звенья цепи, как видите, наконец-то собираются воедино. Каждый из сборщиков связан, во-первых, со своей сборочной фабрикой, а во-вторых, со своим слугой. Например, сборщик `alpha_builder` связан с фабрикой `alpha_factory` и со слугой `slave_alpha`. Таким образом, shell-команда, добавленная в качестве сборочного шага в фабрику `alpha_factory`, будет выполнена силами слуги `slave_alpha`. Того самого слуги, которого мы создали в самом начале.

### О скрипте

Как вы уже догадались, выполнение скрипта `mhelper_update.sh` как раз и делает всю фактическую работу по обновлению нашего веб-приложения. Разумеется, можно было обойтись и без него, ведь скрипт есть набор shell-команд, и весь этот набор можно было включить в наши сборочные фабрики. Однако я как-то привык работать с отдельным скриптом. Во-первых, так конфигурационный файл сильно упрощается, а во-вторых, в крайнем случае (ну мало ли что) всегда можно обновить приложение и без CI, запустив этот скрипт вручную. Весьма удобно.

Содержимое этого скрипта стандартно: берём исходники приложения из нужной ветки, компилируем (в моём случае речь идёт о Yesod-приложении), затем копируем исполняемый файл вместе со статическими файлами на другую, рабочую виртуалку, после чего перезапускаем там процесс. Всё просто.

### Выводы

Вот и весь наш CI. Пушим в ветку - через минуту получаем обновлённый веб-стенд. В общем, этот Buildbot мне очень понравился. Конечно, веб-интерфейс бы ему покрасивше - цены б ему не было. А возможностей у него реально много. То, что описано в данной статье - это так, Hello World. Подробности ищите [здесь](http://docs.buildbot.net/current/index.html).


