package dlmScala

package object helpers {

  import breeze.linalg.{DenseMatrix, DenseVector}

  def blockDiag(A: DenseMatrix[Double], B: DenseMatrix[Double]) = {
    
    val aDim = A.rows
    val bDim = B.rows
    assert(aDim == A.cols && bDim==B.cols)

    val abZeros = DenseMatrix.zeros[Double](aDim,bDim)
    val upper = DenseMatrix.horzcat(A,abZeros)
    val lower = DenseMatrix.horzcat(abZeros.t,B)

    DenseMatrix.vertcat(upper,lower)
  }


  /** times the execution of a block and returns what the block returns*/
  def timer[R](block: => R): R = {  
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0) / 1E9 + "s")
    result
  }


}
