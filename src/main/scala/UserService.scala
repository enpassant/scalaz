import scala.util.Random

case class User(userId: String, name: String)

object AuthService {
  def authUser(userId: String) = {
    val random = new Random
    if (random.nextInt(5) < 1) {
      Left(new Exception("Authentication service is not responding"))
    } else Right(userId.startsWith("bob"))
  }
}

object UserRepo {
  def selectUser(userId: String) = {
    val random = new Random
    if (random.nextInt(5) < 1) {
      Left(new Exception("Database server is not responding"))
    } else if (userId == "bob") {
      Right(Some(User(userId, s"${userId.toUpperCase} Montgomery")))
    } else Right(None)
  }
}

object UserService {
  type AuthUser = String => Either[Exception, Boolean]
  type SelectUser = String => Either[Exception, Option[User]]

  def getUser(userId: String)(authUser: AuthUser)(selectUser: SelectUser):
    Either[Exception, Option[User]] =
  {
    authUser(userId) match {
      case Right(true) => selectUser(userId)
      case Right(_) => Right(None)
      case Left(e) => Left(e)
    }
  }

  def main(args: Array[String]): Unit = {
    val user = UserService.getUser("bob")(AuthService.authUser)(UserRepo.selectUser)
    println(s"bob => $user")
    val userBobby = UserService.getUser("bobby")(AuthService.authUser)(UserRepo.selectUser)
    println(s"bobby => $userBobby")
  }
}

