---
title: Магия Haskell - вызываем джина
description: Генерирование кода функции по ее типу с помощью джина (djinn)
tags: haskell, djinn, windows
katex: true
toc: true
---

В мире Haskell есть много интересных пакетов, программ, которых не встретишь в мире ООП ввиду их функциональной направленности.
Примером одной из таких программ является [djinn](https://hackage.haskell.org/package/djinn), которую создал Lennart Augustsson 
([исходный код](https://github.com/augustss/djinn)). То, что она делает, похоже на уличную магию и, видимо, производит наибольший
вау-эффект на тех, кто недавно вошел в мир функционального программирования. Программа по заданному типу функции пытается сгенерировать
код этой функции, если это вообще возможно. Лучше наверное сказать, что она, исходя из типов входных параметров, пытается найти
такую трансформацию, которая бы в итоге возвращала значение нужного типа. Лучше всего это объяснить на примерах.

## Как вызывать джина без лампы?

Для начала установим [djinn](https://hackage.haskell.org/package/djinn). Актуальной версией на момент написания является 2014.9.7. 
Проблем с установкой под Windows быть не должно (обычно, если какая-либо программа на Haskell не собирается, то, как правило, не 
собирается именно под Windows, это если авторы использовали что-то из мира Linux, а именно [в том мире в большинстве случаев 
обитают хаскеллисты](https://cdsmith.wordpress.com/2007/07/06/why-dont-haskell-developers-use-windows/)).

Обновляем список пакетов и устанавливаем djinn:

```
> cabal update
> cabal install djinn
```

Все пакеты под Windows устанавливаются в `%APPDATA%\cabal`, устанавливаемые исполняемые файлы оказываются в каталоге `%APPDATA%\cabal\bin`,
поэтому лучше всего добавить этот каталог в `%PATH%`, если этого еще не сделали. Все кто против системы или просто лентяи могут призвать 
джина путем запуска:

```
> %APPDATA%\cabal\bin\djinn.exe
```

## Загадываем желания

Теперь, когда джин вызван, можно загадывать желания. Рассмотрим команды, которые он умеет выполнять.

### Команда `<sym> ? <type>`

Эта команда используется чтобы сгенерировать функцию с именем `<sym>` по ее типу `<type>`. Djinn знает о типах функций,
о кортежах, о Either, Maybe, (), можно также объявить и передать синонимы типов, тип данных. Вот список синонимов типов,
типов данных и классов типов определенных в окружении djinn сразу после запуска (позже можно определить свои):

```
Djinn> :e
```

```{.haskell}
data () = ()
data Either a b = Left a | Right b
data Maybe a = Nothing | Just a
data Bool = False | True
data Void
type Not x = x -> Void
class Monad m where return :: a -> m a; (>>=) :: m a -> (a -> m b) -> m b
class Eq a where (==) :: a -> a -> Bool
```

Если функция может быть найдена, она будет выведена в виде кода на Haskell. Например, сгенерируем функцию, которая 
принимает параметр типа `a`{.haskell} и возвращает результат того же типа `a`{.haskell}:

```
Djinn> f ? a->a
```

```{.haskell}
f :: a -> a
f a = a
```

Это было просто. Пример чуть сложнее, где из внутренних кортежей выберем элементы нужных нам типов.

```
Djinn> sel ? ((a,b),(c,d)) -> (b,c)
```

```{.haskell}
sel :: ((a, b), (c, d)) -> (b, c)
sel ((_, a), (b, _)) = (a, b)
```

А слабо из типа `a`{.haskell} вывести тип `b`{.haskell}?

```
Djinn> convert ? a->b
-- convert cannot be realized.
```

"Ага-aa!!!" — укоризненно сказали суровые сибирские лесорубы.

Стоит также отметить, что если может быть несколько имплементаций, то будет выведена только одна из них. Например,

```
Djinn> f ? a->a->a
```

```{.haskell}
f :: a -> a -> a
f _ a = a
```

Однако, можно включить вывод нескольких решений командой:

```
Djinn> :s +multi
```

Тогда результат команды

```
Djinn> f ? a->a->a
```

будет таким

```{.haskell}
f :: a -> a -> a
f _ a = a
-- or
f a _ = a
```

### Команда `type <sym> <vars> = <type>`

Для того, чтобы добавить в окружение синоним типа можно использовать эту команду. Синонимы типов раскрываются до того, как djinn начинает
поиск реализации функции.

Простой пример:

```
Djinn> type Id a = a->a
Djinn> f ? Id a
```

```{.haskell}
f :: Id a
f a = a
```

Пример посложнее. Попробуем найти реализацию функций `return`, `bind` и `callCC` для [монады продолжения](https://www.haskell.org/haskellwiki/All_About_Monads#The_Continuation_monad).
Несколько описаний на английском этой монады есть в 
[блоге Gabriel Gonzalez](http://www.haskellforall.com/2012/12/the-continuation-monad.html), 
[Wikibooks](http://en.wikibooks.org/wiki/Haskell/Continuation_passing_style#The_Cont_monad),
[Stackoverflow](http://stackoverflow.com/questions/3322540/tutorial-to-disassemble-the-haskell-cont-monad)
[FPComplete](https://www.fpcomplete.com/user/jwiegley/understanding-continuations), 
на русском можно почитать на
[habrahabr](http://habrahabr.ru/post/127040/),
[еще habrahabr](http://habrahabr.ru/post/149174/),
[rsdn](http://rsdn.ru/forum/decl/810346.1).

```
Djinn> type C r a = (a -> r) -> r
Djinn> returnC ? a -> C r a
```

```{.haskell}
returnC :: a -> C r a
returnC a b = b a
```

```
Djinn> bindC ? C r a -> (a -> C r b) -> C r b
```

```{.haskell}
bindC :: C r a -> (a -> C r b) -> C r b
bindC a b c = a (\ d -> b d c)
```

```
Djinn> callCC ? ((a -> C r b) -> C r a) -> C r a
```

```{.haskell}
callCC :: ((a -> C r b) -> C r a) -> C r a
callCC a b = a (\ c _ -> b c) b
```

### Команда `data <sym> <vars> = <type>`

Эта команда добавляет в окружение новый тип данных.

```
Djinn> data Foo a = C a a a
Djinn> f ? a -> Foo a
```

```{.haskell}
f :: a -> Foo a
f a = C a a a
```

Стоит заметить, что djinn не позволяет определять рекурсивных типов. Т.е. если вы попытаетесь определить числа Пеано,
djinn вас разочарует:

```
Djinn> data Peano = Zero | Succ Peano
```

```
Error: Recursive types are not allowed: Peano
```

Хотя djinn и позволяет определять пустые типы командой ```data <sym> <vars>``` (тип данных без конструкторов), но все они 
интерпретируются как одинаковые (из-за хака использованного при определении типов данных в djinn).

Можно теперь завернуть монаду продолжения в наш тип данных и посмотреть на результат генерации функций `return`, `bind` и `callCC`:

```
Djinn> data C r a = C ((a -> r) -> r)
Djinn> returnC ? a -> C r a
```

```{.haskell}
returnC :: a -> C r a
returnC a = C (\ b -> b a)
```

```
Djinn> bindC ? C r a -> (a -> C r b) -> C r b
```

```{.haskell}
bindC :: C r a -> (a -> C r b) -> C r b
bindC a b =
        case a of
        C c -> C (\ d ->
                  c (\ e ->
                     case b e of
                     C f -> f d))
```

```
Djinn> callCC ? ((a -> C r b) -> C r a) -> C r a
```

```{.haskell}
callCC :: ((a -> C r b) -> C r a) -> C r a
callCC a =
         C (\ b ->
            case a (\ c -> C (\ _ -> b c)) of
            C d -> d b)
```

Получилось не так красиво, как с синонимом типов, но дальше можно уже допилить напильником трансформируя `case` в патттерн матчинг 
на уровне параметров функции, например.

### Команда `?instance <sym> <types>`

Для того, чтобы не просить сгенерировать отдельно функции `bind` и `return`, можно попросить сразу сгенерировать инстанс класса типов `Monad`.
Пример монады продолжения:

```
Djinn> data C r a = C ((a -> r) -> r)
Djinn> ?instance Monad (C r)
```

```{.haskell}
instance Monad (C r) where
   return a = C (\ b -> b a)
   (>>=) a b =
        case a of
        C c -> C (\ d ->
                  c (\ e ->
                     case b e of
                     C f -> f d))
```

Пример монады Maybe:

```
Djinn> ?instance Monad Maybe
```

```{.haskell}
instance Monad Maybe where
   return = Just
   (>>=) a b =
        case a of
        Nothing -> Nothing
        Just c -> b c
```

### Команда `class <sym> <vars> where <methods>`

Никто не мешает определить свой класс типов и потом просить сгенерировать для этого класса типов реализацию.

```
Djinn> class Functor f where fmap :: (a -> b) -> f a -> f b; (<$) :: a -> f b -> f a
Djinn> ?instance Functor Maybe
```

```{.haskell}
instance Functor Maybe where
   fmap a b =
       case b of
       Nothing -> Nothing
       Just c -> Just (a c)
   (<$) a b =
       case b of
       Nothing -> Nothing
       Just _ -> Just a
```

```
Djinn> data C r a = C ((a -> r) -> r)
Djinn> ?instance Functor (C r)
```

```{.haskell}
instance Functor (C r) where
   fmap a b =
       case b of
       C c -> C (\ d -> c (\ e -> d (a e)))
   (<$) a b =
       case b of
       C c -> C (\ d -> c (\ _ -> d a))
```

Этих команд джину должно хватить, чтобы почувствовать себя магом в Haskell. Не упомянул я только команд

```
:clear                             Очистить окружение
:delete <sym>                      Удалить определенный символ из окружения
:help                              Вывести список команд
:load <file>                       Загрузить файл с командами
:quit                              Выйти из программы
:set <option>                      Установить опции джина
```

Можно использовать первые буквы команд.
Опции по-умолчанию:

```
    -multi     Вывод нескольких решений
    +sorted    Сортировка решений
    -debug     Отладочный режим
    cutoff=200 Максимальное число генерируемых решений
```

Удачных заклинаний!