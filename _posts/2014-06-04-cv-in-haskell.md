---
layout: post
title: 'Пишем резюме на Haskell'
category: articles
tags: [Haskell, прикол]
comments: true
share: true
---

Вчера ночью пришло вдохновение, и решил я прикольнуться. Забегая вперёд, скажу, что прикол удался, мне и самому понравилось.

Короче, решил я написать своё резюме на Haskell. Знаете, как это часто бывает? Разработчики пишут резюме, в котором, помимо всего прочего, выкладывают ссылки на примеры своего кода (где-нибудь на GitHub). А я подумал, почему бы не попробовать совместить резюме и код? И вот что из этого вышло:

{% highlight haskell %}
-- Denis Shevchenko, 2014
-- CV, for Haskellers only. ;-)

module Main where

import Control.Monad.Writer.Lazy
import Data.Functor ((<$>))
import Data.List (dropWhileEnd)
import Data.Char (isSpace)

type InfoAboutMe = String
type InfoCloud = Writer InfoAboutMe ()
type Years = (Int, Int)

showYears :: Years -> String
showYears years = show (fst years) ++ " - " ++ show (snd years)

indent = "  "
bigIndent = "    * "
now = 2014 

name :: InfoCloud
name = tell $ indent ++ "Denis Shevchenko" ++ "\n" 

contacts :: InfoCloud
contacts = 
    let rawContactsLines = ["website:  http://dshevchenko.biz",
                            "email:    me@dshevchenko.biz",
                            "GitHub:   /denisshevchenko",
                            "Google+:  +DenisShevchenko",
                            "LinkedIn: /in/dshevchenkobiz"]
        contactsLinesWithNL = (++ "\n") <$> rawContactsLines 
        prettyContactsLines = (bigIndent ++) <$> contactsLinesWithNL
    in
    tell $ concat prettyContactsLines

skills :: InfoCloud
skills = do
    tell $ "\n" ++ indent ++ "Skills: "
    tell $ dropWhileEnd (\char -> char == ',' || isSpace char)
         $ concatMap (++ ", ") ["Haskell",
                                "C++11",
                                "Objective-C",
                                "OS X",
                                "Linux",
                                "Git",
                                "Cocoa",
                                "Network"] 

workAtSCC :: Years -> InfoCloud
workAtSCC years = 
    tell $ bigIndent
           ++ "Satellite Communication Center, " 
           ++ show (fst years) ++ " - " ++ show (snd years) ++ ","
           ++ " C++ developer."
           ++ "\n"

workAtInfrasoft :: Years -> InfoCloud
workAtInfrasoft years = 
    tell $ bigIndent
           ++ "Infrasoft, " 
           ++ showYears years ++ ","
           ++ " C++ developer."
           ++ "\n"

workAtUnicommTelematics :: Years -> InfoCloud
workAtUnicommTelematics years = 
    tell $ bigIndent
           ++ "Unicomm Telematics, " 
           ++ showYears years ++ ","
           ++ " C++ developer."
           ++ "\n"

workAtOVSoft :: Years -> InfoCloud
workAtOVSoft years = 
    tell $ bigIndent
           ++ "OV-Soft, " 
           ++ showYears years ++ ","
           ++ " C++ developer."
           ++ "\n"

workAtParagonSoftware :: Years -> InfoCloud
workAtParagonSoftware years = 
    tell $ bigIndent
           ++ "Paragon Software, " 
           ++ showYears years ++ ","
           ++ " Senior C++ developer."
           ++ "\n"

experience :: InfoCloud
experience = do
    tell $ "\n\n" ++ indent ++ "Experience:\n"
    workAtSCC (2006, 2008) 
    >> workAtInfrasoft (2008, 2009) 
    >> workAtUnicommTelematics (2009, 2011)
    >> workAtOVSoft (2011, 2012)
    >> workAtParagonSoftware (2013, now)

education :: Years -> InfoCloud
education years = 
    tell $ "\n\n" ++ indent ++ "Education: "
           ++ showYears years ++ ", "
           ++ "Moscow State Technological University \"Stankin\""

openProjects :: InfoCloud
openProjects = do
    tell $ "\n" ++ indent ++ "Open projects:\n"
           ++ bigIndent ++ "Thoughts: http://blog.dshevchenko.biz"
           ++ "\n"
           ++ bigIndent ++ "О Haskell по-человечески: http://ohaskell.dshevchenko.biz"
           ++ "\n"
           ++ bigIndent ++ "Haskell cookbook: http://bit.ly/1nMpWhp"
           ++ "\n"

main :: IO ()
main = 
    let infoAboutMe = execWriter $ name
                                   >> contacts 
                                   >> skills
                                   >> education (1999, 2004)
                                   >> experience
                                   >> openProjects
    in
    putStrLn infoAboutMe

unusedTechnologies = "Microsoft"
{% endhighlight %}

Прикольно, не правда ли? Вот результат:

{% highlight bash %}
  Denis Shevchenko
    * website:  http://dshevchenko.biz
    * email:    me@dshevchenko.biz
    * GitHub:   /denisshevchenko
    * Google+:  +DenisShevchenko
    * LinkedIn: /in/dshevchenkobiz

  Skills: Haskell, C++11, Objective-C, OS X, Linux, Git, Cocoa, Network

  Education: 1999 - 2004, Moscow State Technological University "Stankin"

  Experience:
    * Satellite Communication Center, 2006 - 2008, C++ developer.
    * Infrasoft, 2008 - 2009, C++ developer.
    * Unicomm Telematics, 2009 - 2011, C++ developer.
    * OV-Soft, 2011 - 2012, C++ developer.
    * Paragon Software, 2013 - 2014, Senior C++ developer.

  Open projects:
    * Thoughts: http://blog.dshevchenko.biz
    * О Haskell по-человечески: http://ohaskell.dshevchenko.biz
    * Haskell cookbook: http://bit.ly/1nMpWhp
{% endhighlight %}

Идея была в том, чтобы отразить в этом компактном резюме мои нынешние Haskell-навыки. Скажем, можно было спокойно обойтись без `Writer`, не так ли? Но зато сразу видно, что я имею представление о том, как работать с этим трансформером. :-) Аппликативный функтор затесался, частичное применение функции промелькнуло, работа с парой показана, и даже простая монадическая цепочка виднеется.

Кроме того, я стремился к тому, чтобы, используя конструкции Haskell, отразить путь моего профессионального развития. Скажем, вот эта монадическая цепочка:

{% highlight haskell %}
experience = do
    tell $ "\n\n" ++ indent ++ "Experience:\n"
    workAtSCC (2006, 2008) 
    >> workAtInfrasoft (2008, 2009) 
    >> workAtUnicommTelematics (2009, 2011)
    >> workAtOVSoft (2011, 2012)
    >> workAtParagonSoftware (2013, now)
{% endhighlight %}

не просто перечисляет места, где я работал, но показывает их в хронологическом порядке.  

Живёт это хозяйство в моём [GitHub Gist](https://gist.github.com/denisshevchenko/18507de8661a45094a1e). Уверен, в будущем туда добавятся ещё кое-какие вкусности. Скажем, программная генерация итогового текста в PDF, или ещё что-нибудь в этом роде.




