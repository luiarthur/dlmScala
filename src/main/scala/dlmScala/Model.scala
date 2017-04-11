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
    type Prior = Prior.Default
    type Param <: Param.Generic
  }

  object UniDF extends GenericUni {
    type Param = Param.UniDF

    def computeW(
      prevC:DenseMatrix[Double], G:DenseMatrix[Double],
      delta:Vector[Double], dim:Vector[Int]
    ) = {
      val n = delta.length
      val dimLower = ??? // 0 :: cumsum 
      val dimUpper = ??? // cumsum 
      require(dim.length == n)
      val wList = Vector.tabulate(n){ i =>
        val Gi = ???
        delta(i) * Gi * C(???) * Gi.t
      }
    }

    def filter(y:Obs,dlm:DLM,init:Param,prior:Prior=new Prior) = {

      val N = y.length
      val wList:List[DenseMatrix[Double]] = ???
      val W:DenseMatrix[Double] = 
        wList.foldLeft(DenseMatrix.zeros[Double](0,0))(blockDiag)

      //def update(param:Param): Param = {
      //  val n = param.n + 1
      //  val R = 
      //  val f = ???
      //  val Q = ???
      //  val m = ???
      //  val C = ???
      //  val n = ???
      //  val d = ???
      //  Param(a,R,f,Q,m,C,n,d)
      //}

      ???
    }
    def forecast(y:Obs,dlm:DLM,params:List[Param],nAhead:Int=1) = ???
    def smooth(y:Obs,dlm:DLM,params:List[Param]) = ???
    def backSample(y:Obs,dlm:DLM,params:List[Param]) = ???
  }

  // TODO: Implement this
  object Uni extends GenericUni {
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
