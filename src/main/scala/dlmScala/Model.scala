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
    type Obs = Double
    type DLM = DLM.Uni
    type Forecast = Forecast.Uni
    type Prior = Prior.Default
    type Param <: Param.Generic

    def filter(y:Obs,dlm:DLM,init:Param,prior:Prior=new Prior) = {
      //require(dlm.W.rows>0 || (prior.delta>0 && prior.delta<1))
      //require(dlm.V>0 || (prior.n0>0 && prior.d0>0))
      ???
    }
    def forecast(y:Obs,dlm:DLM,params:List[Param],nAhead:Int=1) = ???
    def smooth(y:Obs,dlm:DLM,params:List[Param]) = ???
    def backSample(y:Obs,dlm:DLM,params:List[Param]) = ???
  }

  object Uni extends GenericUni {
    type Param = Param.Uni
  }
  object UniDF extends GenericUni {
    type Param = Param.UniDF
  }


  // TODO: Implement these
  //object Vec extends Generic
  //object Mat extends Generic
}
