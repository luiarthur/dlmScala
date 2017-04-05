package dlmScala

// Filter: The result of a filter.
object Filter {
  import breeze.linalg.{DenseMatrix, DenseVector}
  import dlmScala.util._
  import dlmScala.DLM

  trait GenericFilter[Obs,OVar,State,SVar]{
    val y:List[Obs];   val dlm:DLM;
    val a:List[State]; val R:List[SVar];
    val f:List[Obs];   val Q:List[OVar];
    val m:List[State]; val C:List[SVar]
  }

  // FIXME!!!
  //case class Univariate (
  //  y:List[Double], dlm:DLM,
  //  a:List[DenseVector[Double]], R:List[DenseMatrix[Double]],
  //  f:List[Double], Q:List[Double],
  //  m:List[DenseVector[Double]], C:List[DenseMatrix[Double]]
  //) extends GenericFilter(y,dlm,a,R,f,Q,m,C)

  //case class Multivariate() { 
  //  ???
  //}


}
