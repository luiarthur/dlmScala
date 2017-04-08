package dlmScala

object Prior {
  trait Generic

  case class Uni(
    n0:Double=0, d0:Double=0, delta:Double=2.0
  ) extends Generic

  // TODO: Implement these
  case class Vec() extends Generic
  case class Mat() extends Generic
}
