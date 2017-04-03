package dlmScala.mcmc

object Metropolis {
  import breeze.linalg.{DenseMatrix, DenseVector}

  // Univariate metropolis step with normal random walk
  def update(curr:Double, ll:Double=>Double, lp:Double=>Double, candSig:Double):Double = {
    import breeze.stats.distributions.Gaussian

    def logLikePlusLogPrior(x:Double) = ll(x) + lp(x)
    val cand = Gaussian(curr, candSig).sample
    val u = math.log(scala.util.Random.nextDouble)
    val p = logLikePlusLogPrior(cand) - logLikePlusLogPrior(curr)

    if (p > u) cand else curr
  }

  // Multivariate metropolis step with normal random walk
  def mvUpdate(curr:DenseVector[Double], ll:DenseVector[Double]=>Double, lp:DenseVector[Double]=>Double, candSig:DenseMatrix[Double]):DenseVector[Double] = {
    import breeze.stats.distributions.MultivariateGaussian

    def logLikePlusLogPrior(x:DenseVector[Double]) = ll(x) + lp(x)
    val cand = MultivariateGaussian(curr, candSig).sample
    val u = math.log(scala.util.Random.nextDouble)
    val p = logLikePlusLogPrior(cand) - logLikePlusLogPrior(curr)

    if (p > u) cand else curr
  }


}
