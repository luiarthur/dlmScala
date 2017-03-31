import org.scalatest.FunSuite
import breeze.linalg.{DenseMatrix, DenseVector}
import dlmScala.helpers._

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
}
