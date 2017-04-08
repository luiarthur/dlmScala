package dlmScala.mcmc

object Metropolis {

  private[Metropolis] trait GenericMetropolis {
    // To Implement:
    type State
    type Cov
    def rnorm(mu:State,sig:Cov): State

    // Pre-implemented:
    def update(curr:State, ll:State=>Double, lp:State=>Double,
               candSig:Cov): State = {
      def logLikePlusLogPrior(x:State) = ll(x) + lp(x)
      val cand = rnorm(curr,candSig)
      val u = math.log(scala.util.Random.nextDouble)
      val p = logLikePlusLogPrior(cand) - 
              logLikePlusLogPrior(curr)

      if (p > u) cand else curr
    }
  }

  object Univariate extends GenericMetropolis {
    type State = Double
    type Cov = Double
    def rnorm(x: State, sig:Cov) =
      breeze.stats.distributions.Gaussian(x,sig).sample
  }

  object Multivariate extends GenericMetropolis {
    import breeze.linalg.{DenseMatrix, DenseVector}
    type State = DenseVector[Double]
    type Cov = DenseMatrix[Double]
    def rnorm(x: State, cov:Cov) =
      breeze.stats.distributions.MultivariateGaussian(x,cov)
        .sample
  }

}
