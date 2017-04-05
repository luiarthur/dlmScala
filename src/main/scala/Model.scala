package dlmScala


object Model {
  import breeze.linalg.{DenseVector,DenseMatrix}

  // Filter: The result of a filter.
  case class Filter[Obs,OVar,State,SVar,xDLM<:DLM.Generic](
    y:List[Obs],   dlm:xDLM,
    a:List[State], R:List[SVar],
    f:List[Obs],   Q:List[OVar],
    m:List[State], C:List[SVar]
  )

  // TODO: Implement these Generic Methods
  def filter = ???
  def forecast = ???
  def smooth = ???
  def backSample = ???
}
