package dlmScala

object util {

  import breeze.linalg.{DenseMatrix, DenseVector}
  import dlmScala.DLM

  /* memoize. For explaination, see: 
   * https://github.com/luiarthur/scala_practice/blob/master/memoize/memoize.md
  */
  import scala.collection.mutable // only for memoize
  def memoize[I,O](f: I=>O): I=>O = new mutable.HashMap[I,O] {
    override def apply(key: I): O = getOrElseUpdate(key, f(key))
  }

  /* blockDiag: Concatenates square matrices into a larger
   *            block-diagonal matrix
   */
  def blockDiag(A:DenseMatrix[Double], B:DenseMatrix[Double]) = {
    val aDim = A.rows
    val bDim = B.rows
    assert(aDim == A.cols && bDim==B.cols)

    val abZeros = DenseMatrix.zeros[Double](aDim,bDim)
    val upper = DenseMatrix.horzcat(A,abZeros)
    val lower = DenseMatrix.horzcat(abZeros.t,B)

    DenseMatrix.vertcat(upper,lower)
  }

  /* Generates a vector of size p with the first element being 1
   * and the other elements being 0
   */
  def E(p: Int) = {
    DenseVector.vertcat(DenseVector(1.0), DenseVector.zeros[Double](p-1))
  }

  /* Generates a Jordan block matrix of dimensions pxp
   */
  def J(p: Int) = {
    DenseMatrix.tabulate(p,p)( (i,j) => if (i==j || j-i==1) 1.0 else 0.0)
  }


  /** times the execution of a block and returns 
   *  the result of the block 
   */
  def timer[R](block: => R): R = {  
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1E9 + "s")
    result
  }

}
