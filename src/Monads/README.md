#### About `Monads` and `Monoids`

#### `MONOID`

In the abstract mathematical sense, we can define a `Group` `G`  as a set of unordered unique elements associated with a binary operation such that

- for every element x, y in the Group G, `(x . y)` is also an element of G
- for every element x, y, z in the Group G, `x . (y . z) = (x . y) . z`
- there is some element e (referred to as identity element) in Group G such that for any element x in the Group G, `x . e = e . x = x`.
-  for any x in G, there exists some element y which satisfies the relation `x . y = y . x = e`. That is y is an inverse of x.

In haskell such a group can be defined in terms of type class as below

```haskell
class Group g where
    -- identity element
    identity :: g
    -- binary operation
    bin :: g -> g -> g
    -- inverse for an element
    inverse :: g -> g
```

Now whenever we want to create an instance of such a group, we need to verify that the following laws hold good.
>*the below is not a valid haskell code, but just a way of expressing the above laws*

```haskell
-- binary associative operation
forall x y z => x `bin` (y `bin` z) = (x `bin` y) `bin` z
-- an identity
forall x => identity `bin` x == x `bin` identity == x
-- inverse of every element 
forall x => inverse x `bin` x == x `bin` inverse x == identity
```

For the actual real world use in haskell we prefer a much simplicied abstract concept than a group which is a Monoid.

In abstract matchematics a `monoid` is regarded as an algebraic datastructure with a single binary associative operation and an identity element. Monoids are an extension of the Semigroup with identity. 

If `S` is a Set and `.` is a binary operation such that `S X S -> S`, then `S` together with `.` qualifies as a Monoid if it satisfies the below laws.

- Associativity
- Identity element

A `semigroup` together with an `identity` element is called a `monoid`
A `monoid` in which each each element has an inverse is called a `group`.

A monoid type class is defined as below in `haskell`

```haskell
class Monoid m where
    -- an identity element
    mempty :: m
    -- an associative operation
    mappend :: m -> m -> m
    -- an optional mconcat
    mconcat :: [m] -> m
    mconcat = foldr mappend mempty
```

*the infix synonym for the mappend in haskell is defined as below*

```haskell
(<>) :: Mappend m => m -> m -> m
```

#### `MONADS`

`Monad` is a concept from a specific branch of Mathematics called `Category Theory`. From `Haskell's` perspective it can be treated as an abstract datatype of actions. Just like `functors` have the `Functor` type class and `applicative` functors have the `Applicative` type class, `monads` come with their own type class: `Monad`. Also, every Monad is also an Applicative which in turn is a Functor. Here is how its defined.

```haskell
-- m is a type constructor
-- injection (wrapping the type a with a type constructor m)
return a :: a -> m a

-- chaining output of one function to input of another
(>>=) :: m a -> (a -> m b) -> m b

-- performs chaining, but ignores the earlier result
(>>) :: m a -> m b -> m b
x >> y = x >>= \_ -> y

fail :: String -> m a
fail = error
```

From the base definition of the `Monad` class as defined above, the chaining function `(>>=)` is often referred to as `bind`, as it binds the computation result of the left to the parameters on the right.

In it's simplest form a `Monad` can be defined as **Feeding an already wrapped value to a function which takes a normal value but returns a wrapped value**. 
All `Monads` should satisfy the below three laws.

>**Monad Laws**

- Left Identity
    ```haskell
    return x >>= f = f x
    ```
- Right Identity
    ```haskell
    m >>= return = m
    ```
- Associativity
    ```haskell
    m >>= (\x -> f x >>= g) = (m>>= f) >>= g
    ```

>**`Moybe` as an instance of the `Monad`**

```haskell
instance Monad Maybe where
    return :: a -> Maybe a
    return x = Just x

    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing >>= _ = Nothing
    Just x >>= f  = f x 

    fail _ = Nothing
```

>**`Lists` and `maps` as instances of `Monads` **

`Lists` and `maps` are considered as the Bread and Butter of Functional Programming. `Lists` are quite heavily used in `Haskell`. No wonder they can be made instances of the `Monad` type class. As monads, lists are used to model nondeterministic computations which may return arbitrary results.

```haskell
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    fail _ = []
```

> **Some identities**

`(g >>= f) = \x -> f (g x) x`

or

`(g >>= f) x = f (g x) x`

considering `g :: m a` and `f :: a -> m b`

```haskell
    g :: r -> a
    f :: a -> r -> b
    -- So both g and f are functions, with an argument of type x which
    -- permeates everything.
```

>*Following is also an interesting identity*

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
