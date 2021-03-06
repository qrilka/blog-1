---
title: Предупреждения, предупреждения, предупреждения...
---

Один мудрый программист сказал *"Компилятор не враг вам! Напротив, он ваш лучший друг!"*. Если бы все программисты думали так же...

Когда я вижу предупреждение (warning) в коде - я чувствую заботу компилятора. Ведь он помогает мне, показывая мои неточности. Ошибки - с ними всё понятно, их не пропустишь. Но вот предупреждения...

Знаете, когда программист не обращает внимание на предупреждения, всплывающие при компиляции его кода - он поступает глупо. Да-да, глупо. Я даже осмелюсь сказать, *очень* глупо.

Каждое предупреждение - это колокольчик от компилятора: *"Дзинь-дзинь, друг мой, обрати внимание вот сюда, тут ты сделал что-то не так"*. Если же программист игнорирует этот колокольчик - значит, он самонадеян и играет с огнём. А играющий с огнём рано или поздно обожжётся (а в некоторых ситуациях можно ведь и дом спалить)...

Единственное оправдание наличия предупреждений при компиляции - чужой код. Вы не ответственны за чужой код, вы ответственный за свой. Если, скажем, в некой используемой вами сторонней библиотеке есть код, порождающий предупреждения - что ж, жаль, но это не ваша забота (тем более что в ряде случаев вы при всём желании не можете изменить этот код). Однако если предупреждения порождает ваш код - вам нет оправданий. Никаких. **НИ-КА-КИХ**!

Лично я уже не первый год придерживаюсь железного правила: мой код должен компилироваться без единого предупреждения. Без единого. И я не отсупаю от этого правила ни при каких обстоятельствах. Могу сказать, что придерживаюсь этого правила *фанатично*. И буду придерживаться впредь. И вам, друзья, искренне советую то же.
