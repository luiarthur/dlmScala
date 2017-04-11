package dlmScala

object Model {
  import breeze.linalg.{DenseVector,DenseMatrix}

  trait Generic {
    type Obs
    type DLM <: DLM.Generic
    type Forecast <: Forecast.Generic
    type Prior <: Prior.Generic
    type Param <: Param.Generic

    def filter(y:Obs,dlm:DLM,prior:Prior):List[Param]
    def forecast(param:Param,nAhead:Int=1):List[Forecast]
    def smooth(param:Param):List[Param]
    def backSample(param:Param):List[Param]
  }

  object Uni extends Generic {
    type Obs = Double
    type DLM = DLM.Uni
    type Forecast = Forecast.Uni
    type Prior = Prior.Default
    type Param = Param.Uni

    def filter(y:Obs, dlm:DLM, prior:Prior=new Prior()) = {
      //require(dlm.W.rows>0 || (prior.delta>0 && prior.delta<1))
      //require(dlm.V>0 || (prior.n0>0 && prior.d0>0))

      ???
    }
    def forecast(param:Param,nAhead:Int=1) = ???
    def smooth(param:Param) = ???
    def backSample(param:Param) = ???
  }


  // TODO: Implement these
  //object Vec extends Generic
  //object Mat extends Generic
}
