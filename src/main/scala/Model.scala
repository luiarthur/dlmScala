package dlmScala

object Model {
  import breeze.linalg.{DenseMatrix, DenseVector}
  import dlmScala.Filter

  private[Model] trait GenericModel {
    // TODO: Implement Obs,State types
    type Obs
    type State

    // TODO: Implement these Generic Methods
    def filter(y:Obs, dlm:DLM) = ???
    def forecast(y:Obs) = ???
    def smooth = ???
    def backSample = ???
  }

  // FIXME!!!
  object Univariate extends GenericModel {
    ???  
  }

  object Multivariate extends GenericModel {
    ???
  }

}
