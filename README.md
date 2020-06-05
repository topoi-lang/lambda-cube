# lambda-cube

![](https://upload.wikimedia.org/wikipedia/commons/thumb/c/cd/Lambda_Cube_img.svg/320px-Lambda_Cube_img.svg.png?1591352282805)

## untyped-lambda
Contains the untyped lambda calculus example, where all the terms are untyped. Only term and evaluated value in the language.
### Reference
* [Write you a Haskell: lambda calculus](http://dev.stephendiehl.com/fun/003_lambda_calculus.html)

#### What is missing:
* Omega Combinator which is a builtin that allows a lambda argument to itself, which allows recursions.

## simply-typed-lambda
Contains the STLC example, which add type annotation and type checking to the language. Now out language has two scopes (one is to store types, and one is for values).
### Reference
* [Write you a Haskell: type systems](http://dev.stephendiehl.com/fun/004_type_systems.html)

#### What is missing / nice to have
* A monad for type checking so that the type checking between expressions/files is possible.

--------
to do.

## lambda 2 (λ2)
Terms depend on types, it has a lot of names (generic, parametric polymorphism, system F...). It is named lambda 2 because of the name "second order typed lambda calculus).

A generic term is a term that works for any data type, if there is an instance for it. So we only write one definition and it will copied the same way, and generally acts the same way for different type.

### Reference
* [Wikepedia lambda cube](https://www.wikiwand.com/en/Lambda_cube#/(%CE%BB2)_System_F)

#### Boxing
We allocating definitions on the heap and pointers for data structures, and we cast the data to and from, depends on its type.

#### Monomorphization
Copy the code in compile time statically for the different types of data we want to store.

## lambda Omega (λω)
Types depend on types, it is extended from lambda 2. Where it allows type constructor that accept another type. Like Rust's `Result<T, E>` and Haskell's `Either Left Right`.

## Lambda P (Pi type)
Types depend on Terms, also dependent type

## Lambda C (Calculus of construction)
It is a lambda that include the feature of λ2 and λω and λΠ.

## Calculus of inductive construction
Higher inductive types.