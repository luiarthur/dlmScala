import org.scalatest.FunSuite
import breeze.linalg.{DenseMatrix, DenseVector}
import dlmScala.util._
import dlmScala._

class TestSuite extends FunSuite {

  test("blockDiag") {
    val X = DenseMatrix.tabulate(9,1)( (i,_)=>i+1.0 ).reshape(3,3)
    val Y = DenseMatrix.tabulate(4,1)( (i,_)=>i+1.0 ).reshape(2,2)
    val Z = DenseMatrix( (1.0, 4.0 ,7.0, 0.0, 0.0),
                         (2.0, 5.0, 8.0, 0.0, 0.0),
                         (3.0, 6.0, 9.0, 0.0, 0.0),
                         (0.0, 0.0, 0.0, 1.0, 3.0),
                         (0.0, 0.0, 0.0, 2.0, 4.0))
    assert(blockDiag(X,Y) == Z)
  }

  test("superposition") {
    def genDLM(x: Double, d: Int) = {
      val F = DenseVector.fill(d)(x)
      val G = DenseMatrix.eye[Double](d) * x
      val V = x
      val W = G * 2.0
      val dim = Vector(d)
      DLM.Uni(F,G,V,W,dim)
    }

    val dlm1 = genDLM(1.0, 2)
    val dlm2 = genDLM(2.0, 3)

    val dlm3 = dlm1 + dlm2

    assert(dlm3.F == DenseVector(1.0,1.0,2.0,2.0,2.0))
    assert(dlm3.G == blockDiag(dlm1.G, dlm2.G))
    assert(dlm3.V == 1.0 + 2.0)
    assert(dlm3.W == blockDiag(dlm1.W, dlm2.W))
    assert(dlm3.dim == Vector(2, 3))

    //print(dlm3)
  }

  test ("Metropolis") {
    import dlmScala.mcmc._
    val p = .73
    val n = 5000
    val eps = 0.03
    val x = List.fill(n)(
      if (p>scala.util.Random.nextDouble) 1 else 0)
    val sumx = x.sum
    class State(val p:Double) extends Gibbs.State {
      def update = {
        def ll(p:Double) = {
          if (p > 1 || p < 0) Double.NegativeInfinity else
            sumx * math.log(p) + (n-sumx) * math.log(1-p)
        }
        def lp(p:Double) = 0.0
        def cs = 0.03
        new State(Metropolis.Univariate.update(p,ll,lp,cs))
      }
    }

    val init = new State(0.5)
    val out = init.sample(1000,1000)
    //println("Truth:           "+p)
    //println("Estimate:        "+out.map(_.p).sum / 1000)
    //println("Acceptance Rate: "+out.map(_.p).distinct.length/1000.0)
    assert(math.abs(out.map(_.p).sum / 1000 - p) < eps)
  }

  test("Filter UniDF") {
    val R = org.ddahl.rscala.RClient()

    val n = 30
    val y = List.tabulate(n)(i => i + scala.util.Random.nextGaussian)
    val F = DenseVector(1.0,0.0)
    val G = DenseMatrix( (1.0,1.0), (0.0,1.0) )
    val V = 1.0
    val W = DenseMatrix.zeros[Double](0,0)
    val dim = Vector(2)

    val dlm = new DLM.Uni(F,G,V,W,dim)
    val prior = new Prior.UniDF(Vector(0.95))
    val m0 = DenseVector(1.0,0.0)
    val C0 = DenseMatrix.eye[Double](2)
    val init = new Param.UniDF(m=m0,C=C0)
    val mod = timer{ Model.UniDF.filter(y,dlm,init,prior) }

    R.set("f",mod.map(_.f).toArray)
    R.set("y",y.toArray)
    R eval """
      plot(y,col='grey',pch=20)
      points(f,col='blue',pch=20)
    """

    scala.io.StdIn.readLine()
  }

  test("DLM2 Filter UniDF") {
    val R = org.ddahl.rscala.RClient()

    val n = 30
    val y = List.tabulate(n)(i => i + scala.util.Random.nextGaussian)
    val F = DenseVector(1.0,0.0)
    val G = DenseMatrix( (1.0,1.0), (0.0,1.0) )
    val V = 1.0
    val dim = Vector(2)
    val delta = Vector(0.95)

    val dlm = new DLM2.UniDF(F,G,V,delta,dim)
    val m0 = DenseVector(1.0,0.0)
    val C0 = DenseMatrix.eye[Double](2)
    val init = new Param.UniDF(m=m0,C=C0)
    val filt = timer{ dlm.filter(y,init) }

    R.set("f",filt.map(_.f).toArray)
    R.set("y",y.toArray)
    R eval """
      plot(y,col='grey',pch=20)
      points(f,col='blue',pch=20)
    """

    scala.io.StdIn.readLine()
  }

}
