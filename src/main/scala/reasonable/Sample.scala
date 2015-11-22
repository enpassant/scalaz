package reasonable

import scalaz._

import language.higherKinds
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}

object ReasonablyPriced
{
  import Algebra._
  import interact.InteractAlgebra._
  import interact.Console
  import auth.AuthAlgebra._
  import auth.Authenticator
  import error.ErrorAlgebra._
  import error.ErrorInterpreter

  val KnowSecret = "KnowSecret"

  def prg[F[_]](implicit I: Interacts[F], A: Auths[F], E: ErrorMonads[F]) = {
    import I._; import A._; import E._
    for {
      uid <- ask("What's your user ID?")
      _ <- if (uid.length <= 0) error(new Exception("User ID is empty")) else tell("OK")
      pwd <- ask("Password, please.")
      u <- login(uid, pwd)
      _ <- u.map(u => tell(s"Hello ${u.id}!")).getOrElse(tell("Wrong ID or password!"))
      b <- u.map(hasPermission(_, KnowSecret)).getOrElse(Free.point(false))
      _ <- if (b) tell("UUDDLRLRBA") else tell("Go away!")
    } yield (b)
  }

  type F1[A] = Coproduct[Auth, Interact, A]
  type App[A] = Coproduct[ErrorMonad, F1, A]

  val app: Free[App, Boolean] = prg[App]

  val m1 = Map(
    "What's your user ID?" -> "john.snow",
    "Password, please." -> "Ghost")
  val m2 = Map(
    "What's your user ID?" -> "john.rain",
    "Password, please." -> "Ghost")

  val interpreter = or[ErrorMonad, ({type f[x]=Coproduct[Auth,Interact,x]})#f, EitherType](
    ErrorInterpreter, or(Authenticator, Console))


  def main(args: Array[String]): Unit = {
    val retValue = app.foldMap(interpreter)
    retValue match {
      case -\/(t) => t.printStackTrace
      case v => println(v)
    }
  }
}
