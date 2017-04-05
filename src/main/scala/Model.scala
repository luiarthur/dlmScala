package dlmScala

object Model {
  import breeze.linalg.{DenseVector,DenseMatrix}

  // Filter: The result of a filter.
  case class Filter[Obs,OVar,State,SVar,xDLM<:DLM.Generic](
    y:Obs,   dlm:xDLM,
    a:List[State], R:List[SVar],
    f:Obs,   Q:List[OVar],
    m:List[State], C:List[SVar]
  )

  // Forecast: The result of a Forecast
  case class Forecast[Tf,TQ](f:Tf, Q:TQ)

  trait Generic {
    type Obs
    type OVar
    type State
    type SVar
    type xDLM <: DLM.Generic
    type Filt = Filter[Obs,OVar,State,SVar,xDLM]

    def filter(y:Obs,dlm:xDLM):Filt
    def forecast(filt:Filt,nAhead:Int=1):Forecast[Obs,OVar]
    def smooth(filt:Filt):Filt
    def backSample(filt:Filt):Filt
  }

  object Uni extends Generic {
    type Obs = Double
    type OVar = Double
    type State = DenseVector[Double]
    type SVar = DenseMatrix[Double]
    type xDLM = DLM.Uni

    def filter(y:Obs,dlm:xDLM):Filt = {
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
    type xDLM = DLM.Vec

    def filter(y:Obs,dlm:xDLM):Filt = ???
    def forecast(filt:Filt,nAhead:Int=1):Forecast[Obs,OVar] = ???
    def smooth(filt:Filt):Filt = ???
    def backSample(filt:Filt):Filt = ???
  }


  // TODO: Implement the matrix version
  // object Mat extends Generic
}
