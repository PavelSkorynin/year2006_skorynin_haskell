-- Явно выпишем всё, что нам понадобится.
-- Мы об этих штуках ещё не говорили, потому
-- просто игнорируйте эти строчки.
-- В них мы из стандартной библиотеки хапаем
-- себе стандартные типы и функции. Целые числа
-- и операции их сравнения, например.
import Prelude ( Show(..)
               , Bool(..), Int(..), Double(..)
               , (+), (-), (*), (/), mod
               , (<), (==), (>), (<=), (>=))

---------------------------------------------
-- Синтаксис

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x + y
example2' x   = \y -> x + y
example2''    = \x -> \y -> x + y
example2'''   = \x y -> x + y
example2''''  = let z = \x y -> x + y in z
example2''''' = z where
    z x = \y -> x + y

-------------------------------------------
-- Примеры

otherwise = True

abs x | x < 0 = minusx
      | x == 0 = 0
      | otherwise = x
      where minusx = -x

-- Эквивалентны
gcd :: Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b y
    where y = a `mod` b

gcd' a b = if b == 0
    then a
    else gcd' b (a `mod` b)
-- Обратите внимание, что gcd хвосторекурсивный.

-------------------------------------------
-- Операции над функциями

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3   = gcd 121 (gcd 44 11)
example3'  = gcd 121 $ gcd 44 11
example3'' = ($) (gcd 121) (gcd 44 11)

infixr 9 .
(.) f g = \x -> f (g x)

example4 x = (gcd 121 (gcd 44 x))
example4'  = gcd 121 . gcd 44

-------------------------------------------
-- Неподвижные точки
eps = 0.0001

infix 1 ~=
a ~= b = abs (a - b) < eps

average x y = (x + y) / 2
averageDamp f = \ x -> average x (f x)

fixedPoint f g = if g ~= f g
    then g
    else fixedPoint f (f g)

sqrt' x = fixedPoint (\y -> x/y) 1 -- Обычно не сходится.
sqrt x = fixedPoint (averageDamp (\y -> x/y)) 1 -- А этот работает.

-- Взятие производной: нам дали функцию, а мы вернули тоже взяли да
-- и вернули функцию.
deriv :: (Double -> Double) -> (Double -> Double)
deriv f x = (f (x + eps) - f x) / eps

-- Метод Ньютона ищет ноль функции.
newton f x = x - f x / deriv f x

-- Результаты первых двух будут немного отличаться,
sqrt'' x = fixedPoint (newton (\y -> y*y - x)) 1
sqrt''' x = fixedPoint (averageDamp $ newton (\y -> y*y - x)) 1
-- а эта и предыдущая одинаковые.
sqrt'''' x = fixedPoint (averageDamp . newton $ \y -> y*y - x) 1

-------------------------------------------
-- Классические рекурсивные определения

-- Обратите внимание, что вычисление этой
-- функции сначала <<расширяется>>, а потом
-- <<скукоживается>>:
-- fac 10 = 10 * fac 9
--        = 10 * 9 * fac 8
--        ...
--        = 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1
--        = 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2
--        = 10 * 9 * 8 * 7 * 6 * 5 * 4 * 6
--        = 10 * 9 * 8 * 7 * 6 * 5 * 24
--        ...
--        = 3628800
fac 0 = 1
fac n = n * fac (n - 1)

-- а у этой версии ничего не разрастается:
-- fac' 10 = fac'' 10 1
--         = fac'' 9 10
--         = fac'' 9 90
--         ...
--         = fac'' 1 3628800
--         = fac'' 0 3628800
--         = 3628800
fac' n = fac'' n 1 where
    fac'' 0 x = x
    fac'' n a = fac'' (n - 1) (n*a)

-- Мне лень рисовать разрастание такого дерева.
-- Представьте сами.
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- И для этой представьте.
fib' n = fib'' n 1 1 where
    fib'' 0 p pp = pp
    fib'' n p pp = fib'' (n - 1) (pp + p) p

-------------------------------------------
-- Избавляемся от встроенных типов.
-- Мы бы начали с булевых, но мы их уже использовали (оператор if).

-- Однако самостоятельно определить себе булев тип очень просто:
data Bool' = True' | False'
    deriving Show

-- После этого, например, можно определить свой оператор
-- if:
if' True  a b = a
if' False a b = b

-------------------------------------------
-- Избавляемся от встроенных типов.
-- Начинаем с натуральных чисел (в математике -- жирная N)

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- Ноль не является натуральным числом. Правильнее было бы определить
-- data Nat = One | Succ Nat
-- Тогда Integer определяется как
-- data Integer = Zero | Pos Nat | Neg Nat
-- и все счастливы!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

data Nat = Zero | Succ Nat
    deriving Show          -- (1)
--instance Show Nat where  -- (2)
--    show = show . toInt  -- (3)
-- Чёрная магия: если закомментировать строку 1 и раскомментировать 2 и 3,
-- то 
-- > Zero
-- интерпретатор напечатает как 0,
-- а
-- > Succ (Succ Zero)
-- интерпретатор напечатает как 2.

fromInt 0 = Zero                   -- Этот конструктор (Zero) -- просто значение
                                   -- типа Nat.
fromInt a = Succ $ fromInt (a - 1) -- А этот конструктор сильно напоминает функцию.

toInt Zero = 0
toInt (Succ a) = 1 + toInt a

-- Сложение (эквивалентные определения)
plus :: Nat -> Nat -> Nat
plus Zero b     = b
plus (Succ a) b = plus a (Succ b)

plus' a b = case a of
    Zero -> b
    (Succ a) -> plus' a (Succ b)

-- Другое сложение (ведёт себя иначе, но
-- результат такой же)
plus'' Zero b = Zero
plus'' (Succ a) b = Succ $ plus'' a b

-- Реализуйте:
-- * умножение (через сложение)
mul :: Nat -> Nat -> Nat
mul Zero _ = Zero
mul (Succ Zero) b = b
mul (Succ a) b = plus (mul a b) b

-- * вычитание (без использования предыдущих)
-- При этом, sub a b | (a - b), если a >= b
--                   | 0        иначе

sub :: Nat -> Nat -> Nat
sub Zero _ = Zero
sub a Zero = a
sub (Succ a) (Succ b) = sub a b

-- * деление (через вычитание, остаток можно выкинуть)
div :: Nat -> Nat -> Nat
div a b = div' (sub (Succ a) b) b

div' :: Nat -> Nat -> Nat
div' Zero _ = Zero
div' a b = Succ $ div' (sub a b) b



-------------------------------------------
-- Избавляемся от встроенных типов.
-- Продолжаем целыми числами (в математике -- жирная Z)

-- Придумайте тип для целых чисел, обладающий
-- тем свойством, что каждое целое число имеет в нём
-- уникальное представление.

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- Уникальность представления не соблюдена для нуля. Выше было дано
-- объяснение, как сделать правильно.
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

data Integer = Neg Nat | Pos Nat
    deriving Show

zFromInt a | a < 0 = Neg $ fromInt (-a)
           | otherwise = Pos $ fromInt a

zToInt (Neg a) = -(toInt a)
zToInt (Pos a) = toInt a

-- Реализуйте:
-- * сложение
zplus :: Integer -> Integer -> Integer
zplus (Neg a) (Neg b) = Neg $ plus a b
zplus (Pos a) (Pos b) = Pos $ plus a b
zplus (Neg a) (Pos b) = zsub (Pos b) (Pos a)
zplus (Pos a) (Neg b) = zsub (Pos a) (Pos b)

-- * умножение
zmul :: Integer -> Integer -> Integer
zmul (Neg a) (Neg b) = Pos $ mul a b
zmul (Pos a) (Pos b) = Pos $ mul a b
zmul (Neg a) (Pos b) = Neg $ mul a b
zmul (Pos a) (Neg b) = Neg $ mul a b

-- * вычитание
zsub :: Integer -> Integer -> Integer
zsub (Neg a) (Pos b) = Neg $ plus a b
zsub (Pos a) (Neg b) = Pos $ plus a b

zsub (Pos Zero) (Pos b) = Neg b
zsub (Neg Zero) (Neg b) = Pos b
zsub (Pos b) (Pos Zero) = Pos b
zsub (Neg b) (Neg Zero) = Neg b

zsub (Pos (Succ a)) (Pos (Succ b)) = zsub (Pos a) (Pos b)
zsub (Neg (Succ a)) (Neg (Succ b)) = zsub (Neg a) (Neg b)

-- * деление
zdiv :: Integer -> Integer -> Integer
zdiv (Neg a) (Neg b) = Pos $ div a b
zdiv (Pos a) (Pos b) = Pos $ div a b
zdiv (Neg a) (Pos b) = Neg $ div a b
zdiv (Pos a) (Neg b) = Neg $ div a b

-------------------------------------------
-- Избавляемся от встроенных типов.
-- Продолжаем рациональными числами

-- Придумайте тип для рациональных чисел.
-- Уникальность представления каждого числа не обязательна.

data Rational = Ratio Integer Integer
    deriving Show

-- Реализуйте:
-- * сложение
rplus :: Rational -> Rational -> Rational
rplus (Ratio a b) (Ratio c d) = Ratio (zplus (zmul a d) (zmul b c)) (zmul b d)

-- * умножение
rmul :: Rational -> Rational -> Rational
rmul (Ratio a b) (Ratio c d) = Ratio (zmul a c) (zmul b d)

-- * вычитание
rsub :: Rational -> Rational -> Rational
rsub (Ratio a b) (Ratio c d) = Ratio (zsub (zmul a d) (zmul b c)) (zmul b d)

-- * деление
rdiv :: Rational -> Rational -> Rational
rdiv (Ratio a b) (Ratio c d) = Ratio (zmul a d) (zmul b c)

-------------------------------------------
-- Конструируем типы.
-- Пары

-- Предположим, мы хотим уметь определять пары
-- элементов различных типов.
-- Мы могли бы делать это так:
data PairIntInt = PairII Int Int
data PairIntDouble = PairID Int Double
-- но это как-то грустно.

-- Магия Haskell позволяет нам сделать что-то типа
-- data Pair = \a b -> Pair#a#b a b
-- где #a и #b -- значения типовых переменных a и b,
-- или, даже лучше, просто
-- data Pair = \a b -> Pair a b
-- записывают это так:
data Pair a b = Pair a b
    deriving Show

-- Теперь
-- Pair Int Int
-- и
-- Pair Int Double
-- это разные типы, которые как бы генерируются компилятором.
--
-- Почти ничем не отличается от обычных функций (\x y -> y), функция Pair
-- имеет своим результатом тип с одним конструктором, который принимает два
-- аргумента соответствующих типов и возвращает упакованную из них пару.
-- Очень похоже на шаблонные параметры в C++.

-- Теперь мы можем писать функции, возвращающие несколько значений,
-- упакованных в пару.
example5 x = Pair x (Succ Zero)
-- Заметим, что это эквивалентно двум функциям:
example5'1 x = x
example5'2 x = Succ Zero

-- Следите за руками:
example6 x = let (Pair a b) = example5 x in a + (toInt b)
example6' x = let a = example5'1 x
                  b = example5'2 x in a + (toInt b)

-- Смотрите, я раньше сказал, что конструктор -- это почти функция.
-- Так вот он <<почти>> как раз потому, что его можно распаковывать
-- обратно.

-- Заметим также, что:
-- функция с типом
-- Pair a b -> c
-- имеет эквивалентного соседа с типом
-- a -> b -> c

example7 (Pair a b) = example7' a b
example7' a b = b

-- Это преобразование можно автоматизировать:
-- (напоминаю, что стрелка в типах правоассоциативна!)
curry :: (Pair a b -> c) -> a -> b -> c
curry f a b = f (Pair a b)

-- Реализуйте обратную:
uncurry :: (a -> b -> c) -> Pair a b -> c
uncurry f (Pair a b) = f a b

-- Просто какие-то примеры.
example8 (Pair Zero (Succ _)) = Succ Zero
example8 (Pair Zero Zero) = Zero
example8 (Pair (Succ a) _) = a

example9 (Pair (Pair a b) c) = Pair a (Pair b c)

-- Реализуйте функцию pmap с типом
pmap :: (a -> a') -> (b -> b') -> Pair a b -> Pair a' b'
pmap f g (Pair a b) = Pair (f a) (g b)
-- делающую что-то разумное.

-------------------------------------------
-- Конструируем типы.
-- Списки, деревья

data List a = Cons a (List a) -- Элемент и хвост
            | Nil             -- Конец списка
    deriving Show

example10 = Cons (Succ Zero) $ Cons Zero $ Nil

length Nil = 0
length (Cons _ b) = 1 + length b

-- Реализуйте функцию map с типом
map :: (a -> b) -> List a -> List b
map _ Nil = Nil
map f (Cons a tail) = Cons (f a) $ map f tail
-- делающую что-то разумное и такую, что length l == length (map f l)

data Tree a = Node a (Tree a) (Tree a) -- Узел
            | Leaf                     -- Лист
    deriving Show

max a b = if a >= b then a else b

height Leaf = 0
height (Node _ a b) = 1 + max (height a) (height b)

-- Реализуйте функцию
tmap :: (a -> b) -> Tree a -> Tree b
tmap _ Leaf = Leaf
tmap f (Node a t1 t2) = Node (f a) (tmap f t1) (tmap f t2)
-- делающую что-то разумное и такую, что height t == height (tmap f t)

-- Реализуйте функцию
list2tree :: List a -> Tree a
list2tree Nil = Leaf
list2tree (Cons a tail) = Node a Leaf (list2tree tail)
-- делающую что-то разумное и такую, что length l == height (list2tree l)

-------------------------------------------
-- Конструируем типы.
-- Логическое или

data Maybe a = Just a
             | Nothing
    deriving Show

-- Реализуйте функцию
find :: (a -> Bool) -> List a -> Maybe a
find _ Nil = Nothing
find p (Cons a tail) | (p a) = Just a
                     | otherwise = find p tail
-- которая ищет в списке t элемент, удовлетворяющий предикату p (если такой есть).

-- Реализуйте функцию
filter :: (a -> Bool) -> List a -> List a
filter _ Nil = Nil
filter p (Cons a tail) | (p a) = Cons a ftail
                       | otherwise = ftail
                       where ftail = filter p tail
-- которая генерирует список из элементов t, удовлетворяющих предикату f.

isJust Nothing  = False
isJust (Just _) = True

-- При помощи filter, isJust и map реализуйте разумную функцию с типом
maybefilter :: List (Maybe a) -> List a
maybefilter l = map (\(Just a) -> a) (filter isJust l)
--
-- подсказка:
-- map (\(Just a) -> a)
-- Что в этой функции (и подсказке) плохо?

-- Реализуйте разумную функцию
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- Я не придумал "разумную" функцию gfilter и реализовал какую
-- получилось. Более того, я не понял зачем нужны какие-то
-- дополнительные функции для maybefilter. Ниже приведена реализация
-- без isJust, map, filter и gfilter
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter _ Nil = Nil
gfilter f (Cons a tail) | isJust (f a) = Cons (((\(Just a) -> a) . f) a) ftail
                        | otherwise = ftail
                        where ftail = gfilter f tail

-- При помощи неё реализуйте maybefilter':
maybefilter' :: List (Maybe a) -> List a
--maybefilter' l = gfilter (\x -> x) l
maybefilter' Nil = Nil
maybefilter' (Cons Nothing tail) = maybefilter' tail
maybefilter' (Cons (Just a) tail) = Cons a (maybefilter' tail)

-- не обладающую предыдущим недостатком.

data Either a b = Left a
                | Right b
    deriving Show

data Empty --Пустое множество

-- Реализуйте
-- maybe2either :: Maybe a -> Either Empty a
-- maybe2either Nothing = Left 
-- maybe2either (Just a) = Right a
-- 
-- -- Реализуйте
emap :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
emap f _ (Left a) = Left $ f a
emap _ g (Right b) = Right $ g b

