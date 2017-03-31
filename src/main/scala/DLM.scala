package dlmScala

import breeze.linalg.{DenseMatrix, DenseVector}
import dlmScala.helpers._


/**
 * Univariate Normal DLMs are defined by a 4-tuple {F,G,V,W} for constant evolutions
 */
case class DLM(F:DenseVector[Double], G:DenseMatrix[Double], 
               V:Double, W:DenseMatrix[Double]) {

  // Adding Two DLMS
  def +(that: DLM) {
    val newF = DenseVector.vertcat(this.F,that.F)
    val newG = blockDiag(this.G, that.G)
  }

}
