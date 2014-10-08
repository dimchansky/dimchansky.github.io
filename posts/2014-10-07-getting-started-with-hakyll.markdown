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
   для более удобной работы с Git, но и для того, чтобы скомпилировать network пакет в Git bash. В обычной консоли его собрать не получится.

Для начала следует убедиться, что [cabal](https://ru.wikipedia.org/wiki/Cabal) прописан в путях. Для этого открываем консоль и там пишем, к примеру,

    > cabal --version

Если cabal не удалось найти, прописываем в PATH (в зависимости от разрядности установленной Haskell Platform):
	
	C:\Program Files (x86)\Haskell Platform\2014.2.0.0\lib\extralibs\bin
    C:\Program Files (x86)\Haskell Platform\2014.2.0.0\bin

Убеждаемся, что теперь cabal работает и теперь мы переходим к непосредственно сборке Hakyll. Создаем каталог, в котором будем собирать Hakyll.
Я в примерах буду использовать `C:\Hakyll`. Переходим в созданный каталог и там создаем песочницу, в которой будет происходить сборка:

    > cd C:\Hakyll
	> cabal sandbox init

Cabal должен бодро сообщить, что песочница создана:
	
    Writing a default package environment file to
    C:\Hakyll\cabal.sandbox.config
    Creating a new sandbox at C:\Hakyll\.cabal-sandbox

Если не создать песочницы и начать сборку, то все бинарники будут собираться в каталог `%APPDATA%\cabal\bin` и там же рядом будут устанавливаться пакеты.
Чтобы не было конфликтов с версиями пакетов, хорошей практикой считается создание песочницы с последующей установкой в нее всего необходимого.
Причем одну песочницу можно использовать между несколькими проектами. Все дальнейшие вызовы `cabal` необходимо производить исключительно из каталога, 
в котором мы создали песочницу, т.е. из `C:\Hakyll\`.

Теперь нам нужно собрать пакет network. Собирать его нужно в unix-подобной консоли, поэтому запустим Git Extensions и там откроем Git bash 
(либо комбинацией Ctrl+G, либо через меню Git → Git bash). Git bash консоль я буду помечать символом `$` в начале строки. Переходим в наш каталог 
(обращаем внимание на то как указывается путь), обновляем репозиторий cabal и собираем пакет network:
	
    $ cd /c/Hakyll/
    $ cabal update
    $ cabal install network

Никаких сообщений об ошибках быть не должно. Теперь возвращаемся в обычную консоль и в ней собираем pandoc, а затем и hakyll. На данный момент есть 
[известная проблема сборки pandoc](https://github.com/jgm/pandoc/issues/1590), поэтому собирать будем с дополнительным параметром:
	
    > cd C:\Hakyll
    > cabal install pandoc -fhttps
    > cabal install hakyll

После сборки в каталоге `C:\Hakyll\.cabal-sandbox\bin` мы должны увидеть следующие бинарники:
    
	aeson-pretty.exe
    hakyll-init.exe
    json2yaml.exe
    pandoc-citeproc.exe
    pandoc.exe
    simple.exe
    yaml2json.exe

Теперь, когда все готово к созданию сайта, нужно, если этого еще не сделали, создать репозиторий `<имя пользователя>.github.io` на GitHub - это так 
называемый [User page](https://help.github.com/articles/user-organization-and-project-pages/). В моем случае конечный путь до созданного репозитория
выглядел так:

[https://github.com/dimchansky/dimchansky.github.io](https://github.com/dimchansky/dimchansky.github.io)

Все, что будет лежать в ветке **master** этого репозитория, будет доступно по адресу [http://dimchansky.github.io/](http://dimchansky.github.io/) (вместо **dimchansky** здесь и далее везде пишите свое имя).
Понятно, что исходный код нам нужно будет положить в другую ветку, например, **sources** и так как содержимое веток **master** и **sources** будеть сильно 
отличаться и нам нужно будет постоянно копировать новую собранную версию в **master**, то постоянно переключаться между двумя ветками будет создавать сильные неудобства.
Чтобы избежать этого, сделаем небольшой финт ушами, чтобы две ветки одного и того же репозитория лежали в разных папках:

    C:\Hakyll
    └── dimchansky.github.io
        ├── master
        └── sources

Я намеренно сделал дополнительно папку `dimchansky.github.io` внутри `Hakyll` на случай, если я захочу создать еще какой-нибудь сайтик с использованием Hakyll.
Теперь нам снова понадобится Git bash консоль, т.к. мы будем выполнять команды git. Создадим папку для работы с веткой **master** (не забываем вместо **dimchansky** везде писать своем имя):
	
    $ cd /c/Hakyll/
    $ mkdir -p dimchansky.github.io/master
    $ cd dimchansky.github.io/master
    $ git init
    $ touch .gitignore
    $ git add .
    $ git commit -m "Initial commit."
    $ git remote add origin git@github.com:dimchansky/dimchansky.github.io.git
    $ git push origin master

Теперь в той же консоли создадим папку для работы с веткой **sources**:
	
    $ cd ..
    $ git clone -o unrelated master sources
    $ cd sources
    $ git checkout --orphan sources
    $ git commit -m "Initial commit (sources)."
    $ git branch -d master
    $ git remote add origin git@github.com:dimchansky/dimchansky.github.io.git
    $ git push origin sources

Обратите внимание, что ветку **master** мы удалили здесь и хоть git ругнется - это нормально. Теперь дело осталось за малым - создать начальную конфигурацию сайта,
скомпилировать ее и затем с помощью полученного бинарника сгенерировать страницы сайта. Находясь в каталоге **sources** укажем, что мы хотим использовать уже имеющуюся песочницу:
	
    $ cabal sandbox init --sandbox ../../.cabal-sandbox
	
Теперь с помощью Hakyll создадим начальную конфигурацию сайта:
	
    $ ../../.cabal-sandbox/bin/hakyll-init.exe blog
	
Он в нашей папке sources создаст папку blog, нам такой вложенности не надо, поэтому поднимем все на уровень выше:	
	
    $ mv -v blog/* .
    $ rmdir blog

Теперь нужно подредактировать файл `.gitignore` и вписать в него такие строки:
	
    _site
    _cache
    dist
    .cabal-sandbox
    cabal.sandbox.config

Это нужно, чтобы не пихать в ветку **sources** каталоги и файлы:

* **\_site** - каталог, в котором будут сгенерированные статические страницы
* **\_cache** - кеш страниц, который создаст для себя сборщик нашего сайта
* **dist** - каталог, куда будет собран наша конфигуратор сайта
* **.cabal-sandbox** - каталог песочницы
* cabal.sandbox.config - файл конфигурации песочницы

Можно посмотреть, какие же файлы и каталоги были созданы Hakyll'ом:

* about.rst - страница "About"
* blog.cabal - файл для сборки нашего конфигуратора или генератора статических страниц
* contact.markdown - страница "Contact"
* **css** - стили сайта
* **images** - картинки сайта
* index.html - содержимое главнойстраницы сайта
* **posts** - примеры заметок в формате markdown
* site.hs - описание na Haskell того, как нужно генерировать статические страницы
* **templates** - каталог с шаблонами страниц

В дальнейшем все можно в корне поменять, как нам будет удобно. Но пока сделаем одну модификацию в файле `site.hs`, 
чтобы он не спотыкался на страницах с UTF-8 кодировкой. Кстати говоря, набирая страницы в UTF-8 сохраняйте их без BOM, 
т.е. без первых двух байтов, которые любит notepad вставлять, как указатель того, что файл в юникоде. Можно в [notepad++](http://notepad-plus-plus.org/), например, 
указать **Encoding → Encode in UTF-8 without BOM**. Итак, открываем `site.hs` и меняем строки:

```haskell
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
```

на

```haskell
import           GHC.IO.Encoding

--------------------------------------------------------------------------------
main :: IO ()
main = do
    setLocaleEncoding utf8
    setFileSystemEncoding utf8
    setForeignEncoding utf8    
    runHakyll

runHakyll :: IO ()
runHakyll = hakyll $ do
```

Для подсветки синтаксиса кода в папкe **css** создать файл `syntax.css`. Как пример, можно взять дефолтный [Pandoc syntax CSS file](https://github.com/jaspervdj/hakyll/blob/master/web/css/syntax.css), 
а если хватает смелости, то можно его [допилить до нужной радуги](https://benjeffrey.com/pandoc-syntax-highlighting-css). Как оформлять код в markdown можно почитать в [документации по Pandoc](http://johnmacfarlane.net/pandoc/README.html#fenced-code-blocks), 
код можно и [инлайнить](http://johnmacfarlane.net/pandoc/README.html#verbatim).
Также нужно не забыть сослаться на этот CSS файл в файле `templates\default.html`:

```html
    <link rel="stylesheet" type="text/css" href="/css/default.css" />
    <link rel="stylesheet" type="text/css" href="/css/syntax.css" />
```

Теперь можно сохранить все изменения в ветке **sources**:
	
    $ git add -A .
    $ git commit -m "Initial hakyll site"
	$ git push origin sources

Собираем наш генератор сайта:
	
    > cabal build

С его помощью генерируем страницы сайта:	
	
    > dist\build\site\site.exe build

Все сгенерированные страницы окажутся в каталоге **\_site**. Можно теперь локально запустить сервер, который по адресу [http://127.0.0.1:8000/](http://127.0.0.1:8000/) покажет, как выглядит наш сайт
	
    > dist\build\site\site.exe watch

Кстати, при изменении, например, заметок, сервер автоматически перегенерирует измененные страницы. Однако, при изменении `site.hs` нужно сначала пересобрать генератор, а потом сам сайт:

    > cabal build
	> dist\build\site\site.exe rebuild
 
Если порт 8000 уже занят чем-то, можно указать другой порт:

    $ dist/build/site/site.exe watch --port 8080
	
Если нас все устраивает, можно все содержимое каталога **\_site** скопировать в каталог master, перейти в него и отправить на github:

	$ cd ../master
    $ git add -A .
    $ git commit -m "Initial hakyll site"
	$ git push origin master
	
Теперь по адресу [http://dimchansky.github.io/](http://dimchansky.github.io/) можно будет увидеть наш созданный сайт.