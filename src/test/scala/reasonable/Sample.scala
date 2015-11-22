package reasonable

import language.higherKinds
import org.scalatest._
import scala.util.{Failure, Success, Try}
import scalaz._

class ReasonableSampleSpec extends FunSpec with Matchers {
  import Algebra._
  import Tester._
  import interact.InteractAlgebra._
  import interact.TestConsole
  import auth.AuthAlgebra._
  import auth.TestAuthenticator
  import error.ErrorAlgebra._
  import error.TestErrorInterpreter

  val KnowSecret = "KnowSecret"

  def prg[F[_]](implicit I: Interacts[F], A: Auths[F], E: ErrorMonads[F]) = {
    import I._; import A._; import E._
    for {
      uid <- ask("What's your user ID?")
      _ <- if (uid.length <= 0)
              error(new Exception("User ID is empty"))
           else
              tell("OK")
      pwd <- ask("Password, please.")
      u <- login(uid, pwd)
      _ <- u.map(u => tell(s"Hello ${u.id}!")) getOrElse tell("Wrong ID or password!")
      b <- u.map(hasPermission(_, KnowSecret)) getOrElse Free.point(false)
      _ <- if (b) tell("UUDDLRLRBA") else tell("Go away!")
    } yield (b)
  }

  type F1[A] = Coproduct[Auth, Interact, A]
  type App[A] = Coproduct[ErrorMonad, F1, A]

  val app: Free[App, Boolean] = prg[App]

  val interpreterTest = or[ErrorMonad, ({type f[x]=Coproduct[Auth,Interact,x]})#f, Tester](
    TestErrorInterpreter, or(TestAuthenticator, TestConsole))

  describe("A program") {
    it("should login and authorize") {
      val m = Map(
        "What's your user ID?" -> "john.snow",
        "Password, please." -> "Ghost")
      val result = app.foldMap(interpreterTest).apply(m)

      result shouldBe (List("OK", "Hello john.snow!", "UUDDLRLRBA"), \/-(true))
    }

    it("should login and not authorize") {
      val m = Map(
        "What's your user ID?" -> "john.rain",
        "Password, please." -> "Ghost")
      val result = app.foldMap(interpreterTest).apply(m)

      result shouldBe (List("OK", "Hello john.rain!", "Go away!"), \/-(false))
    }

    it("should not login if password is wrong") {
      val m = Map(
        "What's your user ID?" -> "john.rain",
        "Password, please." -> "wrong")
      val result = app.foldMap(interpreterTest).apply(m)

      result shouldBe (List("OK", "Wrong ID or password!", "Go away!"), \/-(false))
    }

    it("should not login if user ID is wrong") {
      val m = Map(
        "What's your user ID?" -> "mary.rain",
        "Password, please." -> "wrong")
      val result = app.foldMap(interpreterTest).apply(m)

      result shouldBe (List("OK", "Wrong ID or password!", "Go away!"), \/-(false))
    }

    it("should throw error when user ID is empty") {
      val m = Map(
        "What's your user ID?" -> "",
        "Password, please." -> "wrong")
      val result = app.foldMap(interpreterTest).apply(m)

      result._2.toEither.left.get.getMessage shouldBe "User ID is empty"
    }
  }
}

