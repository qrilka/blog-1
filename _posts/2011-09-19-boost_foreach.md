---
layout: post
title: Знаете, во что превращается BOOST_FOREACH?
created: 1316416446
categories:
- boost
---
<!--break-->
Недавно я узнал, во что же разворачивается макрос BOOST_FOREACH.

Итак, берём простейший код:
{% highlight cpp %}
#include <boost/foreach.hpp>
#include <vector>

int main() {
    std::vector<int> v;
    BOOST_FOREACH ( int i, v ) {
        // Оставим тело пустым, оно нам не нужно сейчас...
    }
    return 0;
}
{% endhighlight %}
сохраняем его как main.cpp и скармливаем его препроцессору напрямую:
{% highlight cpp %}
$ cpp main.cpp
{% endhighlight %}

И если вычленить из вывода только развёрнутый BOOST_FOREACH, то мы увидим следующее:
{% highlight cpp %}
if (boost::foreach_detail_::auto_any_t _foreach_col6 = boost::foreach_detail_::contain( (v) , (true ? 0 : boost::foreach_detail_::or_( boost::foreach_detail_::and_( boost::foreach_detail_::not_(boost::foreach_detail_::is_array_(v)) , (true ? 0 : boost::foreach_detail_::is_rvalue_( (true ? boost::foreach_detail_::make_probe(v) : (v)), 0))) , boost::foreach_detail_::and_( boost::foreach_detail_::not_(boost_foreach_is_noncopyable( boost::foreach_detail_::to_ptr(v) , boost_foreach_argument_dependent_lookup_hack_value)) , boost_foreach_is_lightweight_proxy( boost::foreach_detail_::to_ptr(v) , boost_foreach_argument_dependent_lookup_hack_value)))))) {} else if (boost::foreach_detail_::auto_any_t _foreach_cur6 = boost::foreach_detail_::begin( _foreach_col6 , (true ? 0 : boost::foreach_detail_::encode_type(v, boost::foreach_detail_::is_const_(v))) , (true ? 0 : boost::foreach_detail_::or_( boost::foreach_detail_::and_( boost::foreach_detail_::not_(boost::foreach_detail_::is_array_(v)) , (true ? 0 : boost::foreach_detail_::is_rvalue_( (true ? boost::foreach_detail_::make_probe(v) : (v)), 0))) , boost::foreach_detail_::and_( boost::foreach_detail_::not_(boost_foreach_is_noncopyable( boost::foreach_detail_::to_ptr(v) , boost_foreach_argument_dependent_lookup_hack_value)) , boost_foreach_is_lightweight_proxy( boost::foreach_detail_::to_ptr(v) , boost_foreach_argument_dependent_lookup_hack_value)))))) {} else if (boost::foreach_detail_::auto_any_t _foreach_end6 = boost::foreach_detail_::end( _foreach_col6 , (true ? 0 : boost::foreach_detail_::encode_type(v, boost::foreach_detail_::is_const_(v))) , (true ? 0 : boost::foreach_detail_::or_( boost::foreach_detail_::and_( boost::foreach_detail_::not_(boost::foreach_detail_::is_array_(v)) , (true ? 0 : boost::foreach_detail_::is_rvalue_( (true ? boost::foreach_detail_::make_probe(v) : (v)), 0))) , boost::foreach_detail_::and_( boost::foreach_detail_::not_(boost_foreach_is_noncopyable( boost::foreach_detail_::to_ptr(v) , boost_foreach_argument_dependent_lookup_hack_value)) , boost_foreach_is_lightweight_proxy( boost::foreach_detail_::to_ptr(v) , boost_foreach_argument_dependent_lookup_hack_value)))))) {} else for (bool _foreach_continue6 = true; _foreach_continue6 && !boost::foreach_detail_::done( _foreach_cur6 , _foreach_end6 , (true ? 0 : boost::foreach_detail_::encode_type(v, boost::foreach_detail_::is_const_(v)))); _foreach_continue6 ? boost::foreach_detail_::next( _foreach_cur6 , (true ? 0 : boost::foreach_detail_::encode_type(v, boost::foreach_detail_::is_const_(v)))) : (void)0) if (boost::foreach_detail_::set_false(_foreach_continue6)) {} else for (int i = boost::foreach_detail_::deref( _foreach_cur6 , (true ? 0 : boost::foreach_detail_::encode_type(v, boost::foreach_detail_::is_const_(v)))); !_foreach_continue6; _foreach_continue6 = true) {
    // Здесь должно было быть тело цикла, но мы оставили его пустым...
 }
{% endhighlight %}

Выглядит впечатляюще! Но знаете, друзья, я, когда это увидел, ничуть не охладел к BOOST_FOREACH. Да, он разврорачивается в монстроподобную строку. Ну и что? Мне-то что с этого? В моём коде никогда не было проблем с BOOST_FOREACH, он всегда работает именно так, как я ожидаю.

Так что использовал, использую и буду использовать впредь. Ну а вы - решайте сами. :)
