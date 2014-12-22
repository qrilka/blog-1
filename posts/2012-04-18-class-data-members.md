---
title: К вопросу о данных класса...
tags: C++
---

Итак:
```cpp
struct disk {
    disk() :
        m_name( "disk storage" ) {}
private:
    std::string const m_name;
public:
    std::string name() const {
        return m_name;
    }
};
```

Итак, имя доступно через disk::name().

Теперь второй вариант:
```cpp
struct disk {
    std::string name() const {
        return "disk storage";
    }
};
```

Возникает вопрос: какой вариант лучше?

Любопытно, что в абсолютно подавляющем большинстве учебников по C++ рассматривается вариант, подобный первому. Объясняется это императивной привычкой: мол, сначала мы создаём объект, а затем используем его.

Однако я утверждаю, что второй вариант лучше. Значительно лучше. И даже не потому, что кода меньше (хотя это тоже плюс), но главным образом потому, что он логичнее.

Зачем подготавливать имя заранее? Не лучше ли предоставлять его лишь тогда, когда оно необходимо? Ведь функция disk::name() может вообще не вызываться, а следовательно, первый вариант создаёт имя абсолютно напрасно. Второй же вариант предоставляет имя только по необходимости. Если же disk::name() вообще ни разу не вызвали - значит, имя никому не понадобилось. А если так - зачем его было создавать и где-то хранить? 

Такой подход отражает один из принципов функционального программирования: данные предоставляются лишь тогда, когда они действительно необходимы.