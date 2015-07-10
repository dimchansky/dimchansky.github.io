---
title: Примеры применения Haskell в реальной жизни
description: Примеры коммерческого использования, коммерческих сервисов, приложений на Haskell
tags: haskell
---

Часто спрашивают: "A где-нибудь реально в жизни используют Haskell, кроме написания функции факториала?" Здесь небольшая подборка проектов, в которых используется Haskell.
Источником ссылок является неплохая презентация "[Intro to Haskell for Erlangers](http://bob.ippoli.to/haskell-for-erlangers-2014/)".

### Коммерческое применение Haskell

* Проект [Haxl](https://github.com/facebook/Haxl) от [Facebook](https://skillsmatter.com/skillscasts/4429-simon-marlow)
* [Amgen](http://cufp.galois.com/2008/abstracts.html#BalabanDavid) - применение Haskell в биотехнологиях
* Количественное моделирование в инвестиционном банке [Credit Suisse](http://cufp.galois.com/2006/abstracts.html#HowardMansell)
* Создание eDSL для представления и процессинга экзотических вторичных ценных бумаг в [Barclays](http://lambda-the-ultimate.org/node/3331)
* Инфраструктура небольшой торговой группы в [Deutsche Bank](http://cufp.galois.com/2008/abstracts.html#PolakowJeff)
* Платформа для высокочастотных торгов в [Tsuru Capital](https://www.haskell.org/communities/05-2010/html/report.html#sect7.6)
* Генерация отчетов в [McGraw-Hill Financial](https://www.youtube.com/watch?v=o3m2NkusI9k) с помощью собственного языка отчетов [ermine](http://ermine-language.github.io/ermine/)
* Высокоуровневый язык [Bluespec](http://www.slideshare.net/mansu/bluespec-talk) для дизайна микросхем

### Приложения написанные на Haskell

* [Silk](https://www.silk.co/) - платформа для создания коллекций о чем угодно
* [Chordify](http://chordify.net/) - создание аккордов по музыкальным композициям для масс
* [Bump](https://bu.mp/) (куплен Google в 2013) - обмен файлами, фотографиями, контактами между телефоном и PC (прикрыли проект, [архив сайта](http://web.archive.org/web/*/https://bu.mp/))
* [MailRank](http://www.mailrank.com/) (куплен Facebook, ноябрь 2011) - приоритезация входящих сообщений (прикрыли проект, [архив сайта](http://web.archive.org/web/*/http://www.mailrank.com/))
* [Bazqux](https://bazqux.com/) - RSS читалка с комментариями

### Коммерческие сервисы

* [janrain](http://janrain.com/) - управление профилями клиентов
* [Spaceport](http://spaceport.io/) (куплен Facebook, август 2013) - фреймворк для мобильных игр с использованием ActionScript 3, компилятор написан на Haskell
* [scrive](http://scrive.com/en/) - сервис для электронной подписи документов (скандинавский рынок)
* [OpenBrain](http://www.openbrain.co.uk/) - вычислительная платформа для научной и бизнес-аналитики
* [skedge.me](http://skedge.me/) - планирование встреч для бизнеса

### Автономные приложения

* [Pandoc](http://johnmacfarlane.net/pandoc/) - универсальная утилита ("швейцарский нож") для работы с текстовыми форматами.
* [Darcs](http://darcs.net/) - распределённая система управления версиями (типа Git или Mercurial)
* [xmonad](http://xmonad.org/) - фреймовый оконный менеджер для X Window System
* [Gitit](http://gitit.net/) - Wiki с применением Git, Darcs или Mercurial для управления историей изменений
* [git-annex](http://git-annex.branchable.com/) Управление большими файлами с помощью git (похоже на Dropbox)
* [ImplicitCAD](http://www.implicitcad.org/) - аналог OpenScad, программное моделирование твердотельных трехмерных САПР-объектов
* [Diagrams](http://projects.haskell.org/diagrams/) - DSL для создания векторной графики с использованием Haskell
* [Hakyll](http://jaspervdj.be/hakyll/) - движок для генерации статических сайтов (использован при создании этого блога)