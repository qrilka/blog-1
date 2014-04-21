---
layout: post
title: 'Writing to log: trifle, but nice'
created: 1327247482
categories:
- !binary |-
  0JvQuNGC0LXRgNCw0YLRg9GA0L3QvtC1INC/0YDQvtCz0YDQsNC80LzQuNGA
  0L7QstCw0L3QuNC1
---
So, we have some function for writing to log. For example:
{% highlight cpp %}
report_about <inactive_sessions>();
{% endhighlight %}

We can read this string as text "directly":
{% highlight cpp %}
report about inactive sessions
{% endhighlight %}

And now look at the implementation of this function. So:
{% highlight cpp %}
template <typename What>
inline void report_about() {
    // ...
    What{} .happened() >> log;
}
{% endhighlight %}

Not so important where **log** come from and what type of **log**, it's details. Most likely, we'll have to report not only about inactive sessions, but about other things too. So type of policy (strategy) must implement the function happened() - tada, message about what happened go into log. Now read this as text "directly":
{% highlight cpp %}
What happened >> log
{% endhighlight %}

Trifle, but nice

*P.S. The "mirror form" of stream-operator should be familiar to my readers in previous issues.*
