---
layout: post
title: О геттерах и сеттерах
category: articles
tags: [C++]
comments: true
share: true
---

Итак, есть у нас следующий код:

{% highlight cpp %}
class worker {
    unsigned int m_id;
public:
    unsigned int get_id() const {
        return m_id;
    }
    
    void set_id( unsigned int new_id ) {
        m_id = new_id;
    }
};
{% endhighlight %}

Перед нами - канонические геттер и сеттер. Первый *просто* возвращает значение идентификатора рабочего, второй - *просто* устанавливает это значение.

И вот совсем недавно я со всею остротой понял, сколь бессмысленны канонические геттеры и сеттеры. От них нет абсолютно никакой пользы. Они только усложняют нашу жизнь.

Сравним:

{% highlight cpp %}
struct worker {
    unsigned int id;
};
{% endhighlight %}

Суть не изменилась, а код стал короче и проще.

Зачем мне прятать поле и при этом предоставлять две функции, открывающие доступ к нему? Это бессмысленно. К тому же на каждое поле мне придётся предоставлять по паре геттер-сеттер, и в большинстве случаев это будет два объявления и две реализации. Сравните это:

{% highlight cpp %}
class worker {
    unsigned int m_id;
    std::string m_name;
    std::string m_section;
public:
    unsigned int get_id() const;
    void set_id( unsigned int new_id );

    std::string get_name() const;
    void set_name( const std::string& new_name );

    std::string get_section() const;
    void set_section( const std::string& new_section );
};

// Где-то в .cpp-шнике

unsigned int worker::get_id() const {
    return m_id;
}
    
void worker::set_id( unsigned int new_id ) {
    m_id = new_id;
}

std::string worker::get_name() const {
    return m_name;
}
    
void worker::set_name( const std::string& new_name ) {
    m_name = new_name;
}

std::string worker::get_section() const {
    return m_section;
}
    
void worker::set_section( const std::string& new_section ) {
    m_section = new_section;
}
{% endhighlight %}

и это:

{% highlight cpp %}
struct worker {
    unsigned int id;
    std::string name;
    std::string section;
};
{% endhighlight %}

Фундаментальная проблема канонических геттеров и сеттеров в том, что они раздувают мой код и привносят в него ненужную сложность, при этом ничего не давая мне взамен. А почему же этих геттеров и сеттеров полно в 99% всех C++-проектов? Виной тому - маниакальная страсть закрывать поля просто потому, что "в соответствии с принципом инкапсуляции данные должны быть сокрыты".

Должны быть сокрыты? Так скрывай их в pimpl, и будет тебе счастье. А то, видите-ли, объявил со словом **<code>private</code>**, но при этом предоставил две функции, сводящие всю эту **<code>private</code>**-ность к нулю. И чем это отличается от открытого поля? Ничем. 

Вывод: выкидывайте канонические геттеры и сеттеры. Окончательно и бесповоротно. А поля, к которым необходим *простой* read-write доступ извне, просто делайте открытыми.

<h3>Уточнение</h3>

Вы заметили, я всё время повторяю слово "канонические". Речь шла только о таких геттерах и сеттерах, которые *просто* возвращают и *просто* изменяют. Но при этом я вполне могу оправдать сеттер, который, помимо установки значения, производит некую проверку (и кричит на всю Ивановскую в случае проблем).
