---
layout: post
title: About Boost.UUID again
created: 1330596487
categories:
- boost
---
Well... There is error in documentation. In class **boost::uuids::uuid** we see this predicat:
{% highlight cpp %}
bool is_nil() const;
{% endhighlight %}
It returns **true** only if UUID equal to {00000000-0000-0000-0000-000000000000}.

But in documentation:
<blockquote>
The function, boost::uuids::uuid::is_null() returns true if and only if the uuid is equal to {00000000-0000-0000-0000-000000000000}.
</blockquote>
Hmm... What is **is_null**? There is no such function.

Misprint...
