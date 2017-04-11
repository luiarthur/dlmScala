package dlmScala

object Param {
  import breeze.linalg.{DenseVector,DenseMatrix}
  type DVec = DenseVector[Double]
  type DMat = DenseMatrix[Double]

  abstract class Generic(a:Any,R:Any,f:Any,Q:Any,m:Any,C:Any)

  // Univariate
  case class Uni(
    a:DVec,R:DMat,
    f:Double,Q:Double,
    m:DVec,C:DMat) extends Generic(a,R,f,Q,m,C)

  // Univariate with Discount Factors
  case class UniDF(
    a:DVec,R:DMat,
    f:Double,Q:Double,
    m:DVec,C:DMat,
    n:Double,d:Double) extends Generic(a,R,f,Q,m,C)

  // TODO: Implement these:
  case class Vec()
  case class Mat()
}
