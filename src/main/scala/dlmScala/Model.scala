package dlmScala

object Model {
  import breeze.linalg.{DenseVector,DenseMatrix}
  import util._

  trait Generic {
    type Obs
    type DLM <: DLM.Generic
    type Forecast <: Forecast.Generic
    type Prior <: Prior.Generic
    type Param <: Param.Generic

    def filter(y:Obs,dlm:DLM,init:Param,prior:Prior):List[Param]
    def forecast(y:Obs,dlm:DLM,params:List[Param],nAhead:Int=1):List[Forecast]
    def smooth(y:Obs,dlm:DLM,params:List[Param]):List[Param]
    def backSample(y:Obs,dlm:DLM,params:List[Param]):List[Param]
  }

  trait GenericUni extends Generic {
    type Obs = List[Double]
    type DLM = DLM.Uni
    type Forecast = Forecast.Uni
    type Prior <: Prior.Generic
    type Param <: Param.Generic
  }

  object UniDF extends GenericUni {
    type Prior = Prior.UniDF
    type Param = Param.UniDF

    def computeW(
      prevC:DenseMatrix[Double], G:DenseMatrix[Double],
      delta:Vector[Double], dim:Vector[Int]
    ) = {

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

    def filter(y:Obs,dlm:DLM,init:Param,prior:Prior) = {
      def filtComputeW(prevC:DenseMatrix[Double]) = 
        computeW(prevC,dlm.G,prior.delta,dlm.dim)

      def update(prevAndY:(Param,Double)): Param = {
        val (prev, yi) = prevAndY
        val n = prev.n + 1
        val W = filtComputeW(prev.C)
        val a = dlm.G * prev.m
        val R = dlm.G * prev.C * dlm.G.t + W
        val f = dlm.F.t * a
        val Q = dlm.F.t * R * dlm.F + prev.S
        val e = yi - f
        val S = prev.S + prev.S / n * (e*e/Q-1)
        val A = R * dlm.F / Q
        val m = a + A*e
        val C = S / prev.S * (R - A*A.t*Q)
        new Param(a,R,f,Q,m,C,n,S)
      }

      def updateAll(yList:Obs, params:List[Param]): List[Param] = {
        if (yList == Nil) params else {
          updateAll(yList.tail, update(params.head,yList.head) :: params)
        }
      }
      updateAll(y, List(init)).reverse
    }

    def forecast(y:Obs,dlm:DLM,params:List[Param],nAhead:Int=1) = ???
    def smooth(y:Obs,dlm:DLM,params:List[Param]) = ???
    def backSample(y:Obs,dlm:DLM,params:List[Param]) = ???
  }

  // TODO: Implement this
  object Uni extends GenericUni {
    type Prior = Prior.Default
    type Param = Param.Uni
    def filter(y:Obs,dlm:DLM,init:Param,prior:Prior=new Prior) = ???
    def forecast(y:Obs,dlm:DLM,params:List[Param],nAhead:Int=1) = ???
    def smooth(y:Obs,dlm:DLM,params:List[Param]) = ???
    def backSample(y:Obs,dlm:DLM,params:List[Param]) = ???
  }

  // TODO: Implement these
  //object Vec extends Generic
  //object Mat extends Generic
}
