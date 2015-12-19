import org.scalatest._
import OktvSimpleCode._

class OktvSimpleCodeSpec extends FunSpec with Matchers {
  describe("calcIntervals") {
    val N = 14
    val X = List(0, 0, 2, 4, 0, 1, 3, 1, 0, 0, 2, 4, 5, 0)

    it("should returns intervals' length and max: (2,4), (3,3), (3,5)") {
      val intervals = calcIntervals2(X)
      intervals shouldBe List(IntervalLengthAndMax(2,4), IntervalLengthAndMax(3,3), IntervalLengthAndMax(3,5))
    }
  }
}
