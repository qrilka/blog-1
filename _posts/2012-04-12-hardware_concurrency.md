---
layout: post
title: Э-э-э... Недоделочка, однако...
category: articles
tags: [C++]
comments: true
share: true
---
Столкнулся с таким вот приколом.

В классе std::thread определена статическая функция hardware_concurrency(), возвращающая число потоков в привязке к железу. То есть то количество потоков, которое на текущем железе может (теоретически) выполняться *физически* одновременно.

Ну я пробую:
{% highlight cpp %}
int main() {
   std::cout << std::thread::hardware_concurrency() << std::endl;
}
{% endhighlight %}
Результат - 0. 

Э-э, не понял... Как это ноль? У меня Core i3 с четырьмя ядрами (ну, на самом деле, конечно же, с двумя, но Linux показывает как будто четыре).

В boost::thread такая функция также имеется, пробую её:
{% highlight cpp %}
int main() {
   std::cout << boost::thread::hardware_concurrency() << std::endl;
}
{% endhighlight %}
Результат - 4, как и ожидалось.

Почему же в стандартном потоке такая бяка? Лезу в стандарт, читаю:
<pre>
30.3.1.6   thread static members       
unsigned hardware_concurrency() noexcept;
 
Returns: The number of hardware thread contexts. [ Note: This value 
should only be considered to be a hint. — end note ] If this value 
is not computable or well defined an implementation should return 0.
</pre>

Ага, вроде понятно. Ну, решил глянуть в файл &lt;thread&gt; - а там вообще весело:
{% highlight cpp %}
// Returns a value that hints at the number of hardware thread contexts.
static unsigned int hardware_concurrency() { return 0; }
{% endhighlight %}
Просто и ясно! :)

В общем, недоделочка...
