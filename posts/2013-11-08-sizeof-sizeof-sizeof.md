---
title: sizeof, sizeof и ещё раз sizeof
tags: C++
---

Не так давно решил собрать в одной заметке результаты, возвращаемые оператором **sizeof** применительно к самым разными типам. Иногда это бывает полезным, так что пусть сия заметка будет нам шпаргалкой на будущее.

32-разрядной платформы у меня под рукой нет, так что все приводимые ниже результаты получены на 64-разрядной платформе с использованием компилятора GCC 4.7.0.
<!--break-->
Для простоты используем симпатишное решение для вывода нормального имени типа вместе с его размером:

```cpp
template< typename T >
inline std::string result_for() {
    int status = -1;
    std::string const name( abi::__cxa_demangle( typeid( T ).name(), 
                                                 0, 
                                                 0, 
                                                 &status ) );
    if ( status not_eq 0 ) {
        return "undemangled";
    }

    return name 
           + ": " 
           + boost::lexical_cast<std::string>( sizeof( T ) );
}
```

<h3>Стандартные типы C++</h3>

Ну, куда ж без них-то:

```cpp
int main() {
    std::cout << result_for< char >() << std::endl;
    std::cout << result_for< wchar_t >() << std::endl;
    std::cout << result_for< char16_t >() << std::endl;
    std::cout << result_for< char32_t >() << std::endl;
    std::cout << result_for< short int >() << std::endl;
    std::cout << result_for< int >() << std::endl;
    std::cout << result_for< long int >() << std::endl;
    std::cout << result_for< long long int >() << std::endl;
    std::cout << result_for< float >() << std::endl;
    std::cout << result_for< double >() << std::endl;
    std::cout << result_for< long double >() << std::endl;
    std::cout << result_for< bool >() << std::endl;
    std::cout << result_for< char* >() << std::endl;
}
```

Результаты:

<bash>
char: 1
wchar_t: 4
char16_t: 2
char32_t: 4
short: 2
int: 4
long: 8
long long: 8
float: 4
double: 8
long double: 16
bool: 1
char*: 8
</bash>

Итак, рекордсмен с большим отрывом - **long double**. Нда-с, 128-битное значение - это реально много.

Кстати, вероятно вы заметили отсутствие стандартного типа **void**. Дело в том, что попытка применения к "пустому" типу оператора **sizeof** вызывает гнев компилятора (правда без ошибок, но предупреждение вы получите, по крайней мере при компиляции с флагом **-pedantic**).

Кстати, пусть вас не смущают типы **char16_t** и **char32_t**. Стандарт C++11 ввёл эти новые ключевые слова, так что данные типы уже являются стандартными.

Напоминаю, что размер указателя никак не зависит от типа, на который он указывает. В то время как размер типа "ссылка на..." очень даже зависит, потому что это и есть размер типа, на который мы ссылаемся. :-)

<h3>Стандартные контейнеры</h3>

Вот это уже поинтереснее будет. Обращаю ваше внимание, что здесь включены и контейнеры, добавленные в STL новым стандартом. Для просты будем в качестве наполнения использовать тип **char**, как самый маленький:

```cpp
int main() {
    std::cout << result_for< std::deque<char> >() << std::endl;
    std::cout << result_for< std::forward_list<char> >() << std::endl;
    std::cout << result_for< std::list<char> >() << std::endl;
    std::cout << result_for< std::map<char, char> >() << std::endl;
    std::cout << result_for< std::queue<char> >() << std::endl;
    std::cout << result_for< std::set<char> >() << std::endl;
    std::cout << result_for< std::stack<char> >() << std::endl;
    std::cout << result_for< std::unordered_map<char, char> >() << std::endl;
    std::cout << result_for< std::unordered_set<char> >() << std::endl;
    std::cout << result_for< std::vector<char> >() << std::endl;
}
```

Результаты таковы:

<bash>
std::deque<char, std::allocator<char> >: 80
std::forward_list<char, std::allocator<char> >: 8
std::list<char, std::allocator<char> >: 24
std::map<char, char, std::less<char>, std::allocator<std::pair<char const, char> > >: 48
std::queue<char, std::deque<char, std::allocator<char> > >: 80
std::set<char, std::less<char>, std::allocator<char> >: 48
std::stack<char, std::deque<char, std::allocator<char> > >: 80
std::unordered_map<char, char, std::hash<char>, std::equal_to<char>, std::allocator<std::pair<char const, char> > >: 64
std::unordered_set<char, std::hash<char>, std::equal_to<char>, std::allocator<char> >: 64
std::vector<char, std::allocator<char> >: 24
</bash>

Как видим, от 8 до 80. **deque** самый тяжёлый, **forward_list** - легок как пух.

Разумеется, изменение типа значений, содержащихся в контейнере, никак не повлияет на результат: поставьте **long double** вместо **char**, а **sizeof** контейнера останется прежним.

<h3>Строки</h3>

Что у нас со строками:

```cpp
int main() {
    std::cout << result_for< std::string >() << std::endl;
    std::cout << result_for< std::wstring >() << std::endl;
    std::cout << result_for< std::u16string >() << std::endl;
    std::cout << result_for< std::u32string >() << std::endl;
}
```

Результат:

<bash>
std::string: 8
std::basic_string<wchar_t, std::char_traits<wchar_t>, std::allocator<wchar_t> >: 8
std::basic_string<char16_t, std::char_traits<char16_t>, std::allocator<char16_t> >: 8
std::basic_string<char32_t, std::char_traits<char32_t>, std::allocator<char32_t> >: 8
</bash>

Как и следовало ожидать, результат не зависит от типа символа, хранящегося в строке.

<h3>enum</h3>

С перечислимым типом всё просто:

```cpp
enum Enum {
    a
};

int main() {
    std::cout << result_for< Enum >() << std::endl;
}
```

Результат таков:

<bash>
Enum: 4 
</bash>

Причём размер не зависит от количества "полей" в Enum. Он может быть даже пустым - результат будет тем же.

<h3>"Just for fun" ©</h3>

А это так, для развлечения:

```cpp
int main() {
    std::cout << result_for< std::valarray<char> >() << std::endl;
    std::cout << "---------------------------" << std::endl;
    std::cout << result_for< std::thread >() << std::endl;
    std::cout << "---------------------------" << std::endl;
    std::cout << result_for< std::regex >() << std::endl;
    std::cout << "---------------------------" << std::endl;
    std::cout << result_for< std::ifstream >() << std::endl;
    std::cout << result_for< std::ofstream >() << std::endl;
    std::cout << result_for< std::fstream >() << std::endl;
    std::cout << result_for< std::filebuf >() << std::endl;
    std::cout << "---------------------------" << std::endl;
    std::cout << result_for< std::istream >() << std::endl;
    std::cout << result_for< std::ostream >() << std::endl;
    std::cout << result_for< std::iostream >() << std::endl;
    std::cout << "---------------------------" << std::endl;
    std::cout << result_for< std::istringstream >() << std::endl;
    std::cout << result_for< std::ostringstream >() << std::endl;
    std::cout << result_for< std::stringstream >() << std::endl;
    std::cout << result_for< std::stringbuf >() << std::endl;
}
```

Да, я знаю, размер такого рода типов нужен нам редко. Очень редко. Можно даже сказать, вообще не нужен. И всё же вот результат:

<bash>
std::valarray<char>: 16
---------------------------
std::thread: 8
---------------------------
std::basic_regex<char, std::regex_traits<char> >: 32
---------------------------
std::basic_ifstream<char, std::char_traits<char> >: 904
std::basic_ofstream<char, std::char_traits<char> >: 896
std::basic_fstream<char, std::char_traits<char> >: 912
std::basic_filebuf<char, std::char_traits<char> >: 624
---------------------------
std::istream: 280
std::ostream: 272
std::iostream: 288
---------------------------
std::basic_istringstream<char, std::char_traits<char>, std::allocator<char> >: 360
std::basic_ostringstream<char, std::char_traits<char>, std::allocator<char> >: 352
std::basic_stringstream<char, std::char_traits<char>, std::allocator<char> >: 368
std::basic_stringbuf<char, std::char_traits<char>, std::allocator<char> >: 80
</bash>
