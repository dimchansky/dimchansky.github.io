---
title: Создаем блог с помощью Hakyll
description: Подробное описание процесса создания блога на github с помощью Hakyll под Windows
tags: hakyll, haskell, windows
---

[Hakyll](http://jaspervdj.be/hakyll/) является движком для генерации статических сайтов, написанном на 
[Haskell](http://www.haskell.org/). Именно с помощью этого движка были сгенерированы 
страницы этого блога. Вы просто описываете содержимое страниц в удобном формате (обычно используется [markdown](https://ru.wikipedia.org/wiki/Markdown), 
но, при необходимости, можно использовать и TeX - все это достигается благодаря тесной интеграции с 
[pandoc](http://johnmacfarlane.net/pandoc/), тоже написанном на Haskell), затем на Haskell описываете конфигурацию 
сайта, по каким  правилам и как нужно собрать страницы, компилируете конфигурацию и с помощью нее потом собираете 
страницы сайта. Благодаря pandoc в сгенерированных страницах можно использовать подсветку кода и другие полезные плюшки.

С одной стороны мне очень не хочется создавать N+1 описание Hakyll'a для начинающих, но с другой стороны
хочется указать на возможные при компиляции и использовании Hakyll под Windows грабли. В качестве хостинга для нашего
создаваемого сайта мы будем использовать [GitHub](https://github.com/).

Необходимым минимумом для создании своего сайта с помощью Hakyll являются:
 
 * Минимальные знания Haskell или желание его [изучить](http://tryhaskell.org/)
 * Установленную [Haskell Platform for Windows](http://www.haskell.org/platform/windows.html)
 * Установленный пакет [Git Extensions](https://code.google.com/p/gitextensions/). Этот пакет на нужен будет не только 
   для более удобной работы с Git, но и для того, чтобы скомпилировать network пакет в Git bash. Иначе его собрать не получится.


   
Предполагается, что Haskell Platform уже установлен.
Создаем каталог Hakyll и заходим в него.

> cabal sandbox init
Writing a default package environment file to
C:\Hakyll\cabal.sandbox.config
Creating a new sandbox at C:\Hakyll\.cabal-sandbox

Из Git bash

$ cd /c/Hakyll/
$ cabal update
$ cabal install network
$ exit

Из командной строки

https://github.com/jgm/pandoc/issues/1590
> cd C:\Hakyll
> cabal install pandoc -fhttps
> cabal install hakyll

C:\Hakyll\.cabal-sandbox\bin
aeson-pretty.exe
hakyll-init.exe
json2yaml.exe
pandoc-citeproc.exe
pandoc.exe
simple.exe
yaml2json.exe

Создаем https://github.com/dimchansky/dimchansky.github.io

$ cd /c/Hakyll/
$ mkdir -p dimchansky.github.io/master
$ cd dimchansky.github.io/master
$ git init
$ touch .gitignore
$ git add .
$ git commit -m "Initial commit."
$ git remote add origin git@github.com:dimchansky/dimchansky.github.io.git
$ git push origin master

$ cd ..
$ git clone -o unrelated master sources
$ cd sources
$ git checkout --orphan sources
$ git commit -m "Initial commit (sources)."
$ git branch -d master
$ git remote add origin git@github.com:dimchansky/dimchansky.github.io.git
$ git push origin sources
$ cabal sandbox init --sandbox ../../.cabal-sandbox
$ ../../.cabal-sandbox/bin/hakyll-init.exe blog
$ mv -v blog/*
$ rmdir blog

edit .gitignore
_site
_cache
dist
.cabal-sandbox
cabal.sandbox.config

$ git add -A .
$ git commit -m "Initial hakyll site"

[sources def6dd9] Initial hakyll site
 16 files changed, 441 insertions(+)
 create mode 100644 about.rst
 create mode 100644 blog.cabal
 create mode 100644 contact.markdown
 create mode 100644 css/default.css
 create mode 100644 images/haskell-logo.png
 create mode 100644 index.html
 create mode 100644 posts/2012-08-12-spqr.markdown
 create mode 100644 posts/2012-10-07-rosa-rosa-rosam.markdown
 create mode 100644 posts/2012-11-28-carpe-diem.markdown
 create mode 100644 posts/2012-12-07-tu-quoque.markdown
 create mode 100644 site.hs
 create mode 100644 templates/archive.html
 create mode 100644 templates/default.html
 create mode 100644 templates/post-list.html
 create mode 100644 templates/post.html

$ cabal build
$ dist/build/site/site.exe build
$ dist/build/site/site.exe watch

http://127.0.0.1:8000/
