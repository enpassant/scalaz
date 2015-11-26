import org.scalatest._

class UserServiceSpec extends FunSpec with Matchers {
  describe("UserService") {
    val validUserId = "bob"
    val invalidUserId = "bill"
    val authServiceException = new Exception("Authentication service is not responding!")
    val dbException = new Exception("Database server is not responding!")

    def authUser(userId: String) = Right(userId == validUserId)
    def faultyAuthUser(userId: String) = Left(authServiceException)

    def userFromDB(userId: String) = Right(Some(User(userId, userId)))
    def emptyDB(userId: String) = Right(None)
    def faultySelecthUser(userId: String) = Left(dbException)

    it("should returns user when authenticated and in database") {
      val user = UserService.getUser(validUserId)(authUser)(userFromDB)
      user shouldBe Right(Some(User(validUserId, validUserId)))
    }

    it("should returns None when user is not authenticated and in database") {
      val user = UserService.getUser(invalidUserId)(authUser)(userFromDB)
      user shouldBe Right(None)
    }

    it("should returns None when user is authenticated and not in database") {
      val user = UserService.getUser(validUserId)(authUser)(emptyDB)
      user shouldBe Right(None)
    }

    it("should returns None when user is not authenticated and not in database") {
      val user = UserService.getUser(invalidUserId)(authUser)(emptyDB)
      user shouldBe Right(None)
    }

    it("should returns authServiceException when authUser service is faulty") {
      val user = UserService.getUser(validUserId)(faultyAuthUser)(userFromDB)
      user shouldBe Left(authServiceException)
    }

    it("should returns dbException when database server is faulty") {
      val user = UserService.getUser(validUserId)(authUser)(faultySelecthUser)
      user shouldBe Left(dbException)
    }
  }
}
