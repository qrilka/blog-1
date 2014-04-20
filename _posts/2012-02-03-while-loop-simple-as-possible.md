---
layout: post
title: 'while-loop: simple as possible'
created: 1328261448
categories:
- !binary |-
  0JvQuNGC0LXRgNCw0YLRg9GA0L3QvtC1INC/0YDQvtCz0YDQsNC80LzQuNGA
  0L7QstCw0L3QuNC1
---
Well, imagine you see such letter:
<cpp>
while( not_all( small_messages ).was_sended() ) {
    next_from( small_messages ) >> port;
}
</cpp>
So, there is sequential sending of small messages into port, until messages exists. We omit details about port's type and about message format, it doesn't matter. Important that this letter is clear for anyone. Immediately. Without explanation.

However... Suppose <strong>small_messages</strong> is a container of type std::queue. But in this case easier to write like this:
<cpp>
while( !small_messages.empty() ) {
    small_messages.front() >> port;
    small_messages.pop();
}
</cpp>
Yes, it's easier. Easier to write. But little harder to understand.

Oh, I can already hear the screams: "Are you crazy, Denis?? Even novice can understand such simple loop!" Yes, you are right, such loop is <em>not so hard</em> to understand, but my variant is <em>objective easier</em>. 

I'll explain. Take a look at the first line:
<cpp>
while( !small_messages.empty() )
</cpp>
What is this line talking about? "While messages container is not empty..." And what? Unfortunately, this line is not provide us any information about <em>what will be</em> while messages container is not empty. But if we look at my variant:
<cpp>
while( not_all( small_messages ).was_sended() )
</cpp>
we'll read: "While not all messages was sended..." Yeah, if not all sent - the loop will continue to send. Extremely clear.

Implementation of "literary elements" is very simple:
<cpp>
struct not_all {
    explicit not_all( messages const& a_messages ) :
            messages_for_sending( a_messages ) {}
private:
    messages const& messages_for_sending;
public:
    bool was_sended() const {
        return !messages_for_sending.empty();
    }
};

inline message next_from( messages& a_messages ) {
    auto const next_message( a_messages.front() );
    a_messages.pop();
    return next_message;
}
</cpp>
