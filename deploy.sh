#!/bin/bash

# Скрипт для для автоматического обновления блога на gh-pages. 

USAGE="Запускаем так: ./deploy.sh \"Сообщение о коммите\" 

Пример:
  ./deploy.sh \"Обновление стиля.\"
" 

# При любой ошибке скрипт вылетает...
set -e

# Проверяем наличие сообщения...  
if [ "$1" = "" ]
then
    echo "А сообщение о коммите где?" 
    echo "$USAGE" 
    exit 1
fi

# Устанавливаем переменные...  
COMMIT_MESSAGE=$1

# Заливаю в мастер...
git commit -a -m $COMMIT_MESSAGE
git push origin master

# Собираем новую версию сайта...
ghc site.hs
./site rebuild

# Копируем во временное место, предварительно удалив старое, если нужно...
rm -rf /tmp/_site/ || true
cp -R _site /tmp/

# Переключаемся на gh-pages...
git checkout gh-pages

# Копируем прямо в корень содержимое подготовленного каталога _site...
cp -R /tmp/_site/* .

# Учитываем новшества...
git add .
git commit -a -m $COMMIT_MESSAGE

# Заливаем...
git push origin gh-pages

echo "Готово"
