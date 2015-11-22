import org.scalatest._

class SendEmailSpec extends FunSpec with Matchers {
  describe("SpecialOffer") {
    it("should send email") {
      val customer = Customer("Bob", EmailAddress("dummy@dummy.com"), false)

      val result = SpecialOffersService.prepareSpecialOffersEmail(customer)

      result shouldBe (Some(SendEmail(
        EmailAddress("dummy@dummy.com"),
        Email("Hi, Bob! Boy, have we got a deal for you!"))))
    }
  }
}
