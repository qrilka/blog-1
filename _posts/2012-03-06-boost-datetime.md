---
layout: post
title: 'Boost.DateTime: почему они не добавили эту мелочь??'
created: 1331041040
categories:
- boost
---
Уже второй раз с этим разбираюсь, и удивляюсь...

Итак, открываю документацию к **boost::posix_time**. Читаю:
<pre>
ptime from_time_t(std::time_t)
Creates a ptime from the time_t parameter.
The seconds held in the time_t are added to a time point of 1970-Jan-01.
</pre>
Ну что ж, красиво. Однако мне нужна функция **to_time_t**, делающая как раз обратное. Нету. Искал долго - нету и всё.

Однако в примерах вижу раздельчик **Seconds Since Epoch** - и пример с разницей time_duration.

Ну уж могли бы и сами дописать. Ведь это весьма частая задача, я имею в виду конвертацию *В* time_t, а не только *ИЗ* него.

Ну что ж, если гора не идёт к Магомету... Пишем за них:
{% highlight cpp %}
inline time_t to_time_t( boost::posix_time::ptime const& a_posix_time ) {
    boost::posix_time::ptime const time_since_epoch( 
            boost::gregorian::date( 1970, 1, 1 ) );
    return boost::posix_time::time_duration( a_posix_time - time_since_epoch )
           .total_seconds();
}
{% endhighlight %}
и используем.
