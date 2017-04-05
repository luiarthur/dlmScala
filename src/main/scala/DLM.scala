package dlmScala

import breeze.linalg.{DenseMatrix, DenseVector}
import dlmScala.util._

object DLM {

  // FIXME: Can I bound the types?
  abstract class Generic(
    F:Any, G:Any, V:Any, W:Any
  )

  /** Univariate Normal DLMs are defined by a 4-tuple {F,G,V,W} 
   *  for constant evolutions
   */
  case class Uni(
    F:DenseVector[Double], G:DenseMatrix[Double], 
    V:Double, W:DenseMatrix[Double],
    dim:Vector[Int]
  ) extends Generic(F,G,V,W) {

    override def toString = {
      "F\n" + F.toString + "\n\n" +
      "G\n" + G.toString + "\n\n" +
      "V\n" + V.toString + "\n\n" +
      "W\n" + W.toString + "\n\n" +
      "dim\n" + dim.toString
    }

    // Adding Two DLMS (Superposition)
    def +(that: Uni):Uni = {
      import DenseVector.vertcat
      val newF = vertcat(this.F,that.F)
      val newG = blockDiag(this.G, that.G)
      val newV = this.V + that.V
      val newW = blockDiag(this.W, that.W)

      val newDim = this.dim ++ that.dim

      Uni(newF, newG, newV, newW, newDim)
    }
  }

  // TODO: Implement this (see WH Chapter 16)
  case class Vec(
    F:DenseMatrix[Double], G:DenseMatrix[Double], 
    V:DenseMatrix[Double], W:DenseMatrix[Double]
  ) extends Generic(F,G,V,W) { ??? }

  // TODO: Implement this (See WH Chapter 16)
  //case class Mat()
}
