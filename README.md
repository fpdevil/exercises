### `Haskell Functor, Applicative and Monad Types`

#### Functor
The Functor class is used for types that can be mapped over. In other words, a Functor is useful for applying a normal function over a wrapped value.
Instances of Functor should satisfy the following laws:

```haskell
fmap id       ==  id
fmap (f . g)  ==  fmap f . fmap g
```

The infix version of `fmap` is `<$>` and is defined as follows
```haskell
(<$>) :: (Functor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x
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
        result (f y)
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
---
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

> Interesting facts

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

>**An example with the IO**

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

#### Application and usage of the `functors` and `applicatives`

Within the package `Control.Applicative` there are `lifting` functions like `liftA` `liftA2` etc defined using the above `applicative` and `functor` definitions.

```haskell
liftA :: (Applicative f) => (a -> b) -> f a -> f b
liftA f x = pure f <*> x 
          = fmap f x    -- same as above

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = pure f <*> x <*> y
             = f <$> x <*> y      -- same as above
             = fmap f x <*> y     -- same as above
```

>**some examples for `lifting` functions

```haskell
λ> liftA2 (:) (Just 4) $ fmap (\x -> [x]) (Just 5)
Just [ 4 , 5 ]

-- sequencing operations
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr ((<*>) . fmap (:)) (pure [])

- or

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' [] = pure []
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

#### `Monads`

> **Some identities**

(g >>= f) = \x -> f (g x) x

or

(g >>= f) x = f (g x) x

considering  
g :: m a

f :: a -> m b

```haskell
    g :: r -> a
    f :: a -> r -> b
    -- So both g and f are functions, with an argument of type x which
    -- permeates everything.
```
---

>**Following is also an interesting identity**

`Kleisli Arrows`

```haskell
f <=< g = \x -> g x >>= f
f >=> g = \x -> f x >>= g
```
>**or equally**

>`(f <=< g) x = g x >>= f`

>`(f >=> g) x = f x >>= g`


>*Every `Monad` is a `Functor`. So with `>>=` we can deduce `fmap`*
```haskell
fmap f x = x >>= (return . f)
```

Specialized for ((->) r)

```haskell
fmap f x r = (x >>= (const . f)) r
            = (const . f) (x r) r
           = const (f (x r)) r
           = f (x r)
           = (f . x) r
```

>**`join` function**

The join function is the conventional monad join operator. It is used to remove one level of monadic structure, projecting its bound argument into the outer level. 

```haskell
join x r = x r r

join x r = (x >>= id) r
         = (id (x r) r)
         = (x r) r
         = x r r

```
> *The above definition can also be visualized as follows*
```haskell
pure x = const x
pure = const
(<*>) :: f (a->b) -> f a -> f b
(<*>) :: ((->) r (a->b)) -> ((->) r a) -> ((->) r b)
(<*>) :: (r -> (a->b)) -> (r -> a) -> (r -> b)
```

### Applicative Laws
```haskell
Identity: 
pure id <*> v = v
Homomorphism: 
pure f <*> pure x = pure (f x)
Interchange: 
u <*> pure y = pure ($y) <*> u
Composition: 
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
```

#### Identity Law
```haskell
-- LHS
pure (id) <*> v
-- using definition of pure
const id <*> v

-- applying definition of <*>
(\x -> const id x (v x))

-- applying const
(\x -> id (v x))

-- applying id
(\x -> v x)

-- RHS
v
```

#### Homomorphism
```haskell
-- statement
pure f <*> pure x = pure (f x)
-- And the proof
pure f <*> pure x
-- applying definition of pure
const f <*> const x
-- applying definition of <*>
(\y -> const f y (const x y))
-- applying const
(\y -> f (x))
-- As y is unused, replace with _
(\_ -> f x)
-- un-applying definition of pure
pure (f x) -- RHS

```

#### Interchange
```haskell
u <*> pure y = pure ($y) <*> u
-- from LHS
u <*> pure y
-- applying definition of pure
u <*> const y
-- applying definition of <*>
\x -> u x (const y x)
-- applying const
\x -> u x (y)
\x -> u x y

-- from RHS
pure ($y) <*> u
-- applying definition of pure
const ($y) <*> u
-- applying definition of <*>
\x -> const ($y) x (u x)
-- applying const
\x -> ($y) (u x)
-- applying definition of function application operator ($)
\x -> (u x) $ y
\x -> u x y

```
Both the above reduce to a single statement. Hence the Interchange law is proved for ((->) r) type

#### Composition
```haskell
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- from RHS
u <*> (v <*> w)
-- applying definition of <*>
u <*> ( \y -> v y (w y) )
-- applying definition of <*>
\x -> u x ( (\y -> v y (w y)) x )
-- Apply the lambda ( (\y -> v y (w y)) to its 
-- argument x, which results in:
\x -> u x ( v x (w x)) -- (A)

-- from LHS
pure (.) <*> u <*> v <*> w
-- applying definition of pure
const (.) <*> u <*> v <*> w
-- applying definition of <*> to the 1st two terms
(\f -> const (.) f (u f)) <*> v <*> w
-- applying const
(\f -> (.) (u f)) <*> v <*> w
-- applying definition of <*> to the 1st two terms
(\g -> (\f -> (.) (u f)) g (v g)) <*> w
-- applying definition of <*>
\x -> (\g -> (\f -> (.) (u f)) g (v g)) x (w x)
-- Expanding the lambda (\g -> ...) by applying to x
\x -> ((\f -> (.) (u f)) x (v x)) (w x)
-- Expanding the inner lambda (\f -> ...) by applying to x
\x -> (( (.) (u x)) (v x))  (w x)
-- Using definition of function composition (.)
\x -> ((u x) . (v x)) (w x)
-- Using definition of (.), i.e. f.g x = f (g x)
\x -> u x ( v x (w x) ) -- (B)

```
(A) & (B) prove that both the left and right side of the composition law for ((->) r) type reduce to the same statement, and thus the law is proved for the function type.
