package dlmScala

import breeze.linalg.{DenseMatrix, DenseVector}

// Filter: The result of a filter.
case class Filter[Obs,OVar,State,SVar](
  y:List[Obs],   dlm:DLM,
  a:List[State], R:List[SVar],
  f:List[Obs],   Q:List[OVar],
  m:List[State], C:List[SVar]
)
