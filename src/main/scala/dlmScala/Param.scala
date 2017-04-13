package dlmScala

object Param {
  import breeze.linalg.{DenseVector,DenseMatrix}
  type DVec = DenseVector[Double]
  type DMat = DenseMatrix[Double]

  abstract class Generic(a:Any,R:Any,f:Any,Q:Any,m:Any,C:Any)

  // Univariate with Unknown V
  // S0 = d0/n0
  case class UniDF(
    a:DVec=DenseVector.zeros[Double](0),R:DMat=DenseMatrix.zeros[Double](0,0),
    f:Double=0.0,Q:Double=0.0,
    m:DVec,C:DMat,
    n:Double=1,S:Double=1) extends Generic(a,R,f,Q,m,C)

  // Univariate
  case class Uni(
    a:DVec,R:DMat,
    f:Double,Q:Double,
    m:DVec,C:DMat) extends Generic(a,R,f,Q,m,C)

  /* TODO: Implement these:
   * case class Vec()
   * case class Mat()
   * */
}
