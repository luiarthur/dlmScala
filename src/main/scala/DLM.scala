package dlmScala

import breeze.linalg.{DenseMatrix, DenseVector}
import dlmScala.util._


/**
 * Univariate Normal DLMs are defined by a 4-tuple {F,G,V,W} for constant evolutions
 */
case class DLM(F:DenseVector[Double], G:DenseMatrix[Double], 
               V:Double, W:DenseMatrix[Double], 
               m0:DenseVector[Double], c0:DenseMatrix[Double],
               delta:Vector[Double], dim:Vector[Int]) {

  override def toString = {
    "F\n" + F.toString + "\n\n" +
    "G\n" + G.toString + "\n\n" +
    "V\n" + V.toString + "\n\n" +
    "W\n" + W.toString + "\n\n" +
    "m0\n" + m0.toString + "\n\n" +
    "c0\n" + c0.toString
  }

  // Adding Two DLMS (Superposition)
  def +(that: DLM):DLM = {
    import DenseVector.vertcat
    val newF = vertcat(this.F,that.F)
    val newG = blockDiag(this.G, that.G)
    val newV = this.V + that.V
    val newW = blockDiag(this.W, that.W)

    val newM0 = vertcat(this.m0, that.m0)
    val newC0 = blockDiag(this.c0, that.c0)

    val newDelta = this.delta ++ that.delta
    val newDim = this.dim ++ that.dim

    DLM(newF, newG, newV, newW, newM0, newC0, newDelta, newDim)
  }

}

object DLM {

  // Basic DLM Definition
  def apply(F:DenseVector[Double], G:DenseMatrix[Double],
            V:Double, W:DenseMatrix[Double],
            m0:DenseVector[Double], c0:DenseMatrix[Double]) = {
    val dim = F.length
    assert(G.rows==dim && G.cols==dim && 
           W.rows==dim && W.cols==dim)
    new DLM(F,G,V,W,m0,c0,delta=Vector(0.0),dim=Vector(dim))
  }

  // DLM Definition with W specified via discount factor
  def apply(F:DenseVector[Double], G:DenseMatrix[Double],
            V:Double, delta:Vector[Double],
            m0:DenseVector[Double], c0:DenseMatrix[Double]) = {
    val dim = F.length
    val W = DenseMatrix.zeros[Double](0,0)
    assert(G.rows==dim && G.cols==dim)
    new DLM(F,G,V,W=W,m0,c0,delta,dim=Vector(dim))
  }
}
