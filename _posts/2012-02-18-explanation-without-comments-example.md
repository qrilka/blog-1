---
layout: post
title: 'Explanation without comments: example'
created: 1329513681
categories:
- !binary |-
  0JvQuNGC0LXRgNCw0YLRg9GA0L3QvtC1INC/0YDQvtCz0YDQsNC80LzQuNGA
  0L7QstCw0L3QuNC1
- c++11
---
So, we have a function:
{% highlight cpp %}
void set_margins( int left, int top, int right, int bottom );
{% endhighlight %}
However, The most likely example of call of such function will be:
{% highlight cpp %}
placer->set_margins( 2, 2, 2, 2 );
{% endhighlight %}
Hmm... That's problem of "magical numbers". We may forget where is bottom margin and where is top margin. But we don't want to remember declaration of this function. 

So, what we can do?

First solution, canonical:
{% highlight cpp %}
constexpr int left   = 2;
constexpr int top    = 2;
constexpr int right  = 2;
constexpr int bottom = 2;
placer->set_margins( left, top, right, bottom );
{% endhighlight %}
So, it's not bad. But if we have to call this function few times in one scope - well, it's not convenient.

Second solution, not so good:
{% highlight cpp %}
placer->set_margins(   2 // left
                     , 2 // top
                     , 2 // right
                     , 2 // bottom 
                   );
{% endhighlight %}
Comments... Well, I don't recommend this solution.

Third solution, original:
{% highlight cpp %}
inline constexpr int left  ( int const px ) { return px; }
inline constexpr int top   ( int const px ) { return px; }
inline constexpr int right ( int const px ) { return px; }
inline constexpr int bottom( int const px ) { return px; }
{% endhighlight %}
So, we define four functions that just returns argument, nothing more. But now we can write like this:
{% highlight cpp %}
placer->set_margins(   left( 2 )
                     , top( 2 )
                     , right( 2 )
                     , bottom( 2 ) );
// In the same scope...
placer->set_margins(   left( 3 )
                     , top( 5 )
                     , right( 4 )
                     , bottom( 4 ) );
{% endhighlight %}
The result is a short code without variables, without naming conflicts, very easy readable and without comments. In addition, due to use **constexpr** we didn't lose in efficiency in the execution.

In my opinion, beautiful.
