---
title: RTTI+typeid+GCC
tags: gcc
---

Наверняка для многих не секрет то, что GCC манглит(<a href="http://en.wikipedia.org/wiki/Name_mangling#Name_mangling_in_C.2B.2B">name mangling</a>) имена идентификаторов. Но на оборот, наверняка для многих является секретом то, что такие идентификаторы можно расшифровать.

<a href="http://liveworkspace.org/code/b7dff0c2e0f7cd795562734eda8c68f2">Изначальный пример</a>:
```cpp
#include <string>
#include <iostream>
#include <cxxabi.h>
#include <typeinfo>

bool pred(int, int) {return 0;}

template<int L, int R, bool(&P)(int, int)>
struct less {};

int main() {
   std::cout << typeid(char).name() << std::endl;
   std::cout << typeid(unsigned long long).name() << std::endl;
   std::cout << typeid(long double).name() << std::endl;
   std::cout << typeid(std::cout).name() << std::endl;
   
   typedef less<'a', 'b', pred> ls;
   std::cout << typeid(ls).name() << std::endl;
}
```
Вывод:
<blockquote>
c
y
e
So
4lessILi97ELi98EXadL_Z4prediiEEE
</blockquote>
хм.. загадочно...

Сейчас, давайте попытаемся расшифровать "это" в человекоупотребляемую форму.
Изменяем код <a href="http://liveworkspace.org/code/6a3499c487818bfc4f5e063f78f512a2">так</a>:
```cpp
#include <string>
#include <iostream>
#include <cxxabi.h>
#include <typeinfo>

/***************************************************************************/

template<typename T>
std::string demangle(const T& = T()) {
   int stat;
   char* ptr = abi::__cxa_demangle(typeid(T).name(), 0, 0, &stat);
   
   /** if get a nil - return the temporary string */
   if ( !ptr ) return std::string();
   
   /** initialize the string with a buffer */
   std::string str(ptr);
   
   /** freeing the buffer */
   ::free(ptr);
   
   /** return the initialized string */
   return str;
}

/***************************************************************************/

bool pred(int, int) {return 0;}

template<int L, int R, bool(&P)(int, int)>
struct less {};

int main() {
   std::cout << demangle<char>() << std::endl;
   std::cout << demangle<unsigned long long>() << std::endl;
   std::cout << demangle<long double>() << std::endl;
   std::cout << demangle(std::cout) << std::endl;
   
   typedef less<'a', 'b', pred> ls;
   std::cout << demangle<ls>() << std::endl;
}
```
Вывод:
<blockquote>
char
unsigned long long
long double
std::ostream
less<97, 98, pred(int, int)>
</blockquote>
о! человекоупотребляемые имена!

удачи 8) 
