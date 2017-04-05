package dlmScala

import breeze.linalg.{DenseMatrix, DenseVector}
import dlmScala.util._

object DLM {

  // FIXME: Can I bound the types?
  abstract class Generic(F:Any, G:Any, V:Any, m0:Any, c0:Any)

  /** Univariate Normal DLMs are defined by a 4-tuple {F,G,V,W} 
   *  for constant evolutions
   */
  case class Uni(F:DenseVector[Double], G:DenseMatrix[Double], 
                 V:Double,
                 m0:DenseVector[Double], c0:DenseMatrix[Double],
                 delta:Vector[Double], dim:Vector[Int]) extends
    Generic(F,G,V,m0,c0) {

    override def toString = {
      "F\n" + F.toString + "\n\n" +
      "G\n" + G.toString + "\n\n" +
      "V\n" + V.toString + "\n\n" +
      "m0\n" + m0.toString + "\n\n" +
      "c0\n" + c0.toString + "\n\n" +
      "delta\n" + delta.toString + "\n\n" +
      "dim\n" + dim.toString
    }

    // Adding Two DLMS (Superposition)
    def +(that: Uni):Uni = {
      import DenseVector.vertcat
      val newF = vertcat(this.F,that.F)
      val newG = blockDiag(this.G, that.G)
      val newV = this.V + that.V

      val newM0 = vertcat(this.m0, that.m0)
      val newC0 = blockDiag(this.c0, that.c0)

      val newDelta = this.delta ++ that.delta
      val newDim = this.dim ++ that.dim

      Uni(newF, newG, newV, newM0, newC0, newDelta, newDim)
    }
  }

}
