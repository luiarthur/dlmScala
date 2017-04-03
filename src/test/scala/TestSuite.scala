import org.scalatest.FunSuite
import breeze.linalg.{DenseMatrix, DenseVector}
import dlmScala.util._
import dlmScala.DLM 

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
      val W = DenseMatrix.eye[Double](d) * (x+1)
      val m0 = F * 2.0
      val c0 = G * 2.0
      val delta = Vector(x)
      val dim = Vector(d)
      DLM(F,G,V,W,m0,c0,delta,dim)
    }

    val dlm1 = genDLM(1.0, 2)
    val dlm2 = genDLM(2.0, 3)

    val dlm3 = dlm1 + dlm2

    assert(dlm3.F == DenseVector(1.0,1.0,2.0,2.0,2.0))
    assert(dlm3.G == blockDiag(dlm1.G, dlm2.G))
    assert(dlm3.V == 1.0 + 2.0)
    assert(dlm3.W == blockDiag(dlm1.W, dlm2.W))
    assert(dlm3.m0 == DenseVector(2.0,2.0,4.0,4.0,4.0))
    assert(dlm3.c0 == blockDiag(dlm1.c0, dlm2.c0))
    assert(dlm3.delta == Vector(1.0, 2.0))
    assert(dlm3.dim == Vector(2, 3))

    //print(dlm3)
  }
}
