package dlmScala

// For use with FFBS
object Prior {

  trait Generic
  class Default() extends Generic
  case class UniDF(delta: Vector[Double]) extends Generic
}
