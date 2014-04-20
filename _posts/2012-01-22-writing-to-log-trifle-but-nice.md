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
<cpp>
report_about <inactive_sessions>();
</cpp>

We can read this string as text "directly":
<cpp>
report about inactive sessions
</cpp>

And now look at the implementation of this function. So:
<cpp>
template <typename What>
inline void report_about() {
    // ...
    What{} .happened() >> log;
}
</cpp>

Not so important where <strong>log</strong> come from and what type of <strong>log</strong>, it's details. Most likely, we'll have to report not only about inactive sessions, but about other things too. So type of policy (strategy) must implement the function happened() - tada, message about what happened go into log. Now read this as text "directly":
<cpp>
What happened >> log
</cpp>

Trifle, but nice

<em>P.S. The "mirror form" of stream-operator should be familiar to my readers in previous issues.</em>
