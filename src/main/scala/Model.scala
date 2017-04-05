package dlmScala

object Model {
  import breeze.linalg.{DenseVector,DenseMatrix}

  // Filter: The result of a filter.
  case class Filter[Obs,OVar,State,SVar,XDLM<:DLM.Generic](
    y:Obs,         dlm:XDLM,
    a:List[State], R:List[SVar],
    f:Obs,         Q:List[OVar],
    m:List[State], C:List[SVar],
    n:List[Int],   d:List[Int]
  )

  // Forecast: The result of a Forecast
  case class Forecast[Tf,TQ](f:Tf, Q:TQ)

  trait Generic {
    type Obs
    type OVar
    type State
    type SVar
    type XDLM <: DLM.Generic
    type Prior <: Prior.Generic
    type Filt = Filter[Obs,OVar,State,SVar,XDLM]

    def filter(y:Obs,dlm:XDLM,prior:Prior):Filt
    def forecast(filt:Filt,nAhead:Int=1):Forecast[Obs,OVar]
    def smooth(filt:Filt):Filt
    def backSample(filt:Filt):Filt
  }


  object Uni extends Generic {
    type Obs = Double
    type OVar = Double
    type State = DenseVector[Double]
    type SVar = DenseMatrix[Double]
    type Prior = Prior.Uni
    type XDLM = DLM.Uni

    def filter(y:Obs, dlm:XDLM, prior:Prior=new Prior()):Filt = {
      require(dlm.W.rows>0 || (prior.delta>0 && prior.delta<1))
      require(dlm.V>0 || (prior.n0>0 && prior.d0>0))

      ???
    }
    def forecast(filt:Filt,nAhead:Int=1):Forecast[Obs,OVar] = ???
    def smooth(filt:Filt):Filt = ???
    def backSample(filt:Filt):Filt = ???
  }

  object Vec extends Generic {
    type Obs = DenseVector[Double]
    type OVar = DenseMatrix[Double]
    type State = DenseVector[Double]
    type SVar = DenseMatrix[Double]
    type Prior = Prior.Vec
    type XDLM = DLM.Vec

    def filter(y:Obs,dlm:XDLM,prior:Prior):Filt = ???
    def forecast(filt:Filt,nAhead:Int=1):Forecast[Obs,OVar] = ???
    def smooth(filt:Filt):Filt = ???
    def backSample(filt:Filt):Filt = ???
  }


  // TODO: Implement the matrix version
  // object Mat extends Generic
}
