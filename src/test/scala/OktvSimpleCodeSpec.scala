import org.scalatest._
import OktvSimpleCode._

class OktvSimpleCodeSpec extends FunSpec with Matchers {
  describe("calcIntervals") {
    val X = List(0, 0, 2, 4, 0, 1, 3, 1, 0, 0, 2, 4, 5, 0)

    it("should returns intervals' length and max: (2,4), (3,3), (3,5)") {
      val intervals = calcIntervals2(X)
      intervals shouldBe List(new Interval(2,4), new Interval(3,3), new Interval(3,5))
    }
  }

  describe("calcIntervals with Option") {
    val X = List(None, None, Some(-2), Some(-4), None, Some(-1), Some(3), Some(1), None, None, Some(2), Some(-4), Some(5), None)

    it("should returns intervals' length and max: (2,-2), (3,3), (3,5)") {
      val intervals = calcIntervals3(X)
      intervals shouldBe List(new Interval(2,-2), new Interval(3,3), new Interval(3,5))
    }
  }
}
