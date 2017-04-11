package dlmScala

object Forecast {
  import breeze.linalg.{DenseVector,DenseMatrix}

  abstract class Generic(f:Any, Q:Any)
  case class Uni(f:Double,Q:Double) extends Generic(f,Q)
  case class Vec(f:DenseVector[Double],Q:DenseMatrix[Double]) extends Generic(f,Q)
  // TODO: Implement these
  case class Matrix(f:Any,Q:Any) extends Generic(f,Q)
}
