# dlmScala

Univariate DLMs in Scala


# To Do

- [x] discount factor
- [x] Superposition (define `+` operator for `DLM` objects)
- [ ] `filter` method
- [ ] `forecast` method
- [ ] `smooth` method
- [ ] `backSample` method
- [x] `Gibbs` object (for FFBS)

- Merge Model class with DLM class
```scala
object DLM {
  abstract class Generic(F:Any,G:Any,V:Any,W:Any) {
    type Obs
    type ObsVar
    type Prior
    // Param is the posterior updates like in Param.scala
    type Param 
    // This is the type of the state parameter (vector, matrix)
    type State

    def filter(y:Obs,init:Param,prior:Prior):List[Param]
    def forecast(y:Obs,filt:List[Param],nAhead:Int=1):List[(Obs,ObsVar)]
    def smooth(y:Obs,filt:List[Param]):List[(Obs,ObsVar)]
    def backSample(y:Obs,filt:List[Param]):List[State]
  }

}
```


# Interesting Excursions for Univariate and Multivariate?
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

Or try [this](http://stackoverflow.com/questions/3508077/how-to-define-type-disjunction-union-types).

