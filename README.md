**Haskell Functor, Applicative and Monad Types**

>  **Haskell** **(->)** as an instance of **Functor**

```haskell
instance Functor ((->) r) where
    fmap f g = \x -> f (g x)
```
The above version stems from the below definition of fmap, if we
consider ((->) r) as (r ->)

```haskell
fmap :: (a -> b) -> f a -> f b
```
if  f  =  (->) r then
```haskell
fmap :: (a -> b) -> ((->) r) a -> ((->)r) b

-- which can be written as below

fmap :: (a -> b) -> (r -> a) -> (r -> b)
```
which is same as function composition. So, another way of writing the Functor instance is

```haskell
instance Functor ((->) r) where
    fmap = (.)
```
>  **Haskell** **(->)** as an instance of **Appicative**

**(->)** is made an instance of the Applicative as follows

```haskell
instance Applicative ((->) r) where
    pure x = (\_ -> x)
    -- this is same as pure = const
    f <*> g = (\x -> f x (g x))
```

>  **Haskell** **(->)** as an instance of **Monad**
**(->)** is made an instance of the Monad as follows

```haskell
instance Monad ((->) r) where
    return x = (\_ -> x)
    -- this is same as return = const
    g >>= f = \x -> f (g x) x
    -- or
    (g >>= f) x = f (g x) x
```

---
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

>*Following is also an interesting identity*

Kleisli Arrows

```haskell
f <=< g = \x -> g x >>= f
f >=> g = \x -> f x >>= g

-- or equally

(f <=< g) x = g x >>= f
(f >=> g) x = f x >>= g
```

>*Every Monad is a Functor. So with >>= we can deduce fmap*
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

>*join function*

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
