### `Haskell Functor, Applicative and Monad Types`

---

#### Functor
The Functor class is used for types that can be mapped over. In other words, a Functor is useful for applying a normal function over a wrapped value. The `Functor` class is defined in the standard haskell library as below

```haskell
class Functor (f :: * -> *) where
    fmap :: (a -> b) -> f a -> f b    
```

Only concrete types can be made an instance of the `Functor`. i.e., with the kind as `* -> *`. Also all instances of `Functor` should satisfy the following laws:

```haskell
fmap id       ==  id
fmap (f . g)  ==  fmap f . fmap g
```

The infix version of `fmap` is `<$>` and is defined as follows:

```haskell
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x
```

>  **Haskell** **`List`** is made an instance of **`Functor`** as below. It has the kind `[] :: * -> *`

```haskell
instance Functor [] where
    fmap :: (a -> b) -> f a -> f b
    fmap _ [] = []
    fmap f (Just x) = Just (f x)
```

**Haskell** **`(,)`** as an instance of **`Functor`**
Another imteresting functor is the Product Functor, **`(,)`**. It has the the type, `(,) :: a -> b -> (a, b)` and kind `(,) :: * -> * -> *`. To make `(,)` a concrete type we have to supply an additional argument to it.

```haskell
instance Functor ((,) a) where
    fmap :: (b -> c) -> (a, b) -> (a, c)
    fmap f (x, y) = (x, f y)
```

>  **Haskell** **`Maybe`** as an instance of **`Functor`**

```haskell
instance Functor Maybe where
    fmap f Nothing  = Nothing
    fmap f (Just x) = f x
```

>  **Haskell** **`IO`** as an instance of **`Functor`**

```haskell
instance Functor IO where
    fmap f x = do
        y <- x
        return (f y)
```

>  **Haskell** **`(->)`** as an instance of **`Functor`**

```haskell
instance Functor ((->) r) where
    fmap f g = \x -> f (g x)
```
The above version stems from the below definition of fmap, if we
consider `((->) r)` as `(r ->)`

```haskell
fmap :: (a -> b) -> f a -> f b
```
if  we consider `f  =  (->) r` then we can interpret the below
```haskell
fmap :: (a -> b) -> ((->) r) a -> ((->)r) b

-- which can be written as below

fmap :: (a -> b) -> (r -> a) -> (r -> b)
```
which is same as function composition. So, another way of writing the Functor instance of `(->) r` is

```haskell
instance Functor ((->) r) where
    fmap = (.)

-- some examples referenced from LYAH
-- example using fmap and composition
λ> putStrLn $ fmap (show . (++ "efg")) (++ "cd") "ab"
"abcdefg"     
```

>  **Haskell** **`Either`** as an instance of **`Functor`**

The `Either` data type is defined in standard `haskell` library in the module `Data.Either` as below:

```haskell
data Either a b = Left a | Right b
```

`Either` data type is made as an instance of `Functor` as follows

```haskell
instance Functor (Either a) where
    fmap :: (b -> c) -> Either a b -> Either a c
    fmap _ (Left x)  = Left x
    fmap f (Right x) = Right (f x)
```

--
#### Applicative
The `Applicative` class is useful for applying a wrapped function over an already wrapped value to give a wrapped value as result. For a type to act as an Applicative, it has to be an instance of Functor.

> **Class definition of `Applicative`**
```haskell
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

>  **Haskell** **`Maybe`** as an instance of **`Appicative`**

```haskell
instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> x = fmap f x
```

> *Some Interesting facts*

```haskell
pure f <*> x = fmap f x = f<$> x
-- examples
λ> pure (*) <*> Just 4 <*> Just 5
Just 20
λ> Just (*) <*> Just 4 <*> Just 5
Just 20
λ> (*) <$> Just 4 <*> Just 5
Just 20
```

>  **Haskell** **List `[]`** as an instance of an **`Appicative`**

```haskell
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
```

>**An example for the above**

```haskell
λ> [sin, cos, tan] <*> (replicate 3 pi)
[ 1.2246467991473532e-16
, 1.2246467991473532e-16
, 1.2246467991473532e-16
, -1.0
, -1.0
, -1.0
, -1.2246467991473532e-16
, -1.2246467991473532e-16
, -1.2246467991473532e-16
]
```

>  **Haskell** **`IO`** as an instance of **`Appicative`**

```haskell
instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)
```

>**An example using the IO `Applicative`**

```haskell
λ> :t (++) <$> getLine <*> getLine
(++) <$> getLine <*> getLine :: IO [Char]
λ> (++) <$> getLine <*> getLine
abc
def
"abcdef"
```

>  **Haskell** **`(->)`** as an instance of **`Appicative`**

**`(->)`** is made an instance of the `Applicative` as follows

```haskell
instance Applicative ((->) r) where
    pure x = (\_ -> x)
    -- this is same as pure = const
    f <*> g = (\x -> f x (g x))   
```

>**An example for the above**

```haskell
λ> (+) <$> (+ 2) <*> (* 3) $ 4
18
```

>  **Haskell** **`(->)`** as an instance of **`Monad`**
**(->)** is made an instance of the Monad as follows

```haskell
instance Monad ((->) r) where
    return x = (\_ -> x)
    -- this is same as return = const
    g >>= f = \x -> f (g x) x
    -- or
    (g >>= f) x = f (g x) x
```

>**`Applicative` Laws**
---

All `Applicative` functors must satisfy the below laws
- **identity** `pure id <*> v = v`
- **composition** `pure (.) <*> u <*> v <*> <*> w = u <*> (v <*> w)`
- **homomorphism** `pure f <*> pure x = pure (f x)`
- **interchange** `u <*> pure y = pure ($ y) <*> u`

>**`ZipList`**

Another instance of the List type which can act as an `Applicative` is the `ZipList` type. The `ZipList` type is defined as follows:

```haskell
newtype ZipList a = ZipList { getZipList :: [a] }

-- as an instance of Functor
instance Functor ZipList where
    fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)
    -- or
    ZipList fs <*> ZipList xs = ZipList (zipWith id fs xs)
```

**Here, `pure` cannot be defined as just `pure x = ZipList [x]` as it violates the Applicative laws**

- en example with `ZipList`

```haskell
λ> getZipList $ (++) <$> ZipList [ "ha", "heh", "hmm"] <*> ZipList [ "?", "!", "."]
[ "ha?" , "heh!" , "hmm." ]
```
---

#### Application and usage of the `functors` and `applicatives`

Within the package `Control.Applicative` there are some `lifting` functions like `liftA`, `liftA2` etc defined on the lines of the `applicative` and `functor` definitions which are defined above. Here is how they are defined in the standard haskell library.

```haskell
liftA :: (Applicative f) => (a -> b) -> f a -> f b
liftA f x = pure f <*> x 
          = fmap f x    -- same as above

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = pure f <*> x <*> y
             = f <$> x <*> y      -- same as above
             = fmap f x <*> y     -- same as above
```

>**some examples for `lifting` functions**

```haskell
λ> liftA (+5) (Just 4)
Just 9

λ> liftA2 (:) (Just 4) $ fmap (\x -> [x]) (Just 5)
Just [ 4 , 5 ]

-- sequencing operations
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr ((<*>) . fmap (:)) (pure [])

- or

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' []       = pure []
sequenceA' (x : xs) = fmap (:) x <*> sequenceA' xs

-- for IO list sequencing
sequence :: [IO a] -> IO [a]
sequence [] = return []
sequence (x : xs) = do
         y  <- x
         ys <- sequence xs
         return (y : ys)

-- matrix transpose
transpose' :: [[a]] -> [[a]]
transpose' [] = []
transpose' xs = getZipList $ sequenceA $ map ZipList xs
```
