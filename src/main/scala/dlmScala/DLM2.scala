package dlmScala

import breeze.linalg.{DenseMatrix, DenseVector}
import dlmScala.util._

object DLM2 {

  // FIXME: Can I bound the types?
  abstract class Generic(F:Any, G:Any, V:Any, W:Any) {
    // Type of observations (e.g. Double)
    type Obs
    // Type of observational errors (e.g. Double)
    type ObsVar
    // Class for the prior
    type Prior
    // Class for the parameters in a filter (m,C,a,R,f,Q,etc.)
    type Param
    // Type of the state parameter
    type State

    def filter(y:List[Obs], init:Param, prior:Prior): List[Param]
    def forecast(y:List[Obs], filt:List[Param], nAhead:Int=1): List[(Obs,ObsVar)]
    def smooth(y:List[Obs], filt:List[Param]): List[(Obs,ObsVar)]
    def backSample(y:List[Obs], filt:List[Param]): List[State]
  }

  /** Univariate Normal DLMs are defined by a 4-tuple {F,G,V,W} 
   *  for constant evolutions
   */
  case class UniDF(
    F:DenseVector[Double], G:DenseMatrix[Double], 
    V:Double, delta:Vector[Double], dim:Vector[Int]
  ) extends Generic(F,G,V,DenseMatrix.zeros[Double](0,0)) {

    override def toString = {
      "F\n" + F.toString + "\n\n" +
      "G\n" + G.toString + "\n\n" +
      "V\n" + V.toString + "\n\n" +
      "delta\n" + delta.toString + "\n\n" +
      "dim\n" + dim.toString
    }

    // Adding Two DLMS (Superposition)
    def +(that: UniDF):UniDF = {
      import DenseVector.vertcat
      val newF = vertcat(this.F,that.F)
      val newG = blockDiag(this.G, that.G)
      val newV = this.V + that.V
      val newDelta = this.delta ++ that.delta
      val newDim = this.dim ++ that.dim

      UniDF(newF, newG, newV, newDelta, newDim)
    }

    type Obs = Double
    type ObsVar = Double
    type Prior = Prior.UniDF
    type Param = Param.UniDF
    type State = DenseVector[Double]

    def computeW(prevC:DenseMatrix[Double]) = {

      val n = delta.length
      require(dim.length == n)

      val cumDim = dim.scanLeft(0)(_+_-1)
      val dimLower = cumDim.dropRight(1)
      val dimUpper = cumDim.tail

      val wList = Vector.tabulate(n){ i =>
        val Gi = 
          G(dimLower(i) to dimUpper(i), dimLower(i) to dimUpper(i))
        val prevCi = 
          prevC(dimLower(i) to dimUpper(i), dimLower(i) to dimUpper(i))

        delta(i) * Gi * prevCi * Gi.t
      }

      wList.foldLeft(DenseMatrix.zeros[Double](0,0))(blockDiag)
    }


    def filter(y:List[Obs], init:Param, prior:Prior): List[Param] = {
      def update(prevAndY:(Param,Obs)): Param = {
        val (prev, yi) = prevAndY
        val n = prev.n + 1
        val W = computeW(prev.C)
        val a = G * prev.m
        val R = G * prev.C * G.t + W
        val f = F.t * a
        val Q = F.t * R * F + prev.S
        val e = yi - f
        val S = prev.S + prev.S / n * (e*e/Q-1)
        val A = R * F / Q
        val m = a + A*e
        val C = S / prev.S * (R - A*A.t*Q)
        new Param(a,R,f,Q,m,C,n,S)
      }

      def updateAll(dat:List[Obs], params:List[Param]): List[Param] = {
        if (dat == Nil) params else {
          updateAll(dat.tail, update(params.head,dat.head) :: params)
        }
      }

      updateAll(y, List(init)).reverse
    }

    def forecast(y:List[Obs], filt:List[Param], nAhead:Int=1): List[(Obs,Obs)] = ???
    def smooth(y:List[Obs], filt:List[Param]): List[(Obs,Obs)] = ???
    def backSample(y:List[Obs], filt:List[Param]): List[State] = ???
  }

  // TODO: Implement this (see WH Chapter 16)
  //case class Vec(
  //  F:DenseMatrix[Double], G:DenseMatrix[Double], 
  //  V:DenseMatrix[Double], W:DenseMatrix[Double]
  //) extends Generic(F,G,V,W) { ??? }

  // TODO: Implement this (See WH Chapter 16)
  //case class Mat()
}
