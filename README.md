# dlmScala

DLMs in Scala


# To Do

- [x] discount factor
- [x] Superposition (define `+` operator for `DLM` objects)
- [ ] `filter` method
- [ ] `forecast` method
- [ ] `smooth` method
- [ ] `backSample` method
- [x] `Gibbs` object (for FFBS)


# Interesting Excursions:
- type unions?
```scala
type ~[A] = A=>Nothing
type V[U,T] = ~[ ~[U] with ~[T] ]
type ~~[A] = ~[~[A]]
type OR[T,U] = { type Lambda[X] = ~~[X] <:< (T V U) }

type OR[T,U] = {
  type ~[A] = A=>Nothing
  type V[U,T] = ~[ ~[U] with ~[T] ]
  type ~~[A] = ~[~[A]]
  type Lambda[X] = ~~[X] <:< (T V U)
}

def bla[T: (Int OR Double)#Lambda](x: T):Double = x match {
  case x:Int => x.toDouble
  case x:Double => x + 1
}

bla(10)
bla(11.1)
```
