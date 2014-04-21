---
layout: post
title: 'Objective-C: длительность вызова функции'
created: 1376380345
categories:
- objective-c
---
Итак, понадобилось мне узнать, сколько длится вызов функции. И захотелось мне (раз уж речь шла о **<a href="http://strictcodeapp.com">StrictCode</a>**) сделать это на Objective-C. Заодно и узнал этот способ.

Способ, как выяснилось, оказался чрезвычайно простым (спасибо **<a href="http://stackoverflow.com/questions/2129794/iphone-objective-c-how-to-log-a-methods-execution-time-exactly-in-millisecond">StackOverflow</a>**):
{% highlight cpp %}
    NSDate* methodStart = [NSDate date];
    [self obtainDatesOfLastModificationOfFiles]; // <<< Время вызова этого метода...    
    NSDate* methodFinish = [NSDate date];

    NSTimeInterval executionTime = [methodFinish timeIntervalSinceDate: methodStart];
    NSLog(@"executionTime = %f", executionTime);
{% endhighlight %}

Получаем строчку вида:
{% highlight cpp %}
2013-08-13 11:43:03.407 StrictCode[3293:503] executionTime = 0.000209
{% endhighlight %}

Итак, имеем точность до микросекунд. Да, это не наносекунды, как в C++11, но точность вызова до наносекунд лично мне никогда не была нужна.
