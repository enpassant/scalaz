package reasonably

import language.higherKinds

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

object ReasonablyPriced
{
  import Algebra._
  import interact.InteractAlgebra._
  import interact.Console
  import interact.FutureConsole
  import auth.AuthAlgebra._
  import auth.Authenticator
  import auth.FutureAuthenticator
  import error.ErrorAlgebra._
  import error.ErrorInterpreter
  import error.FutureErrorInterpreter

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
      b <- u.map(hasPermission(_, KnowSecret)) getOrElse Return(false)
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

  val interpreter = ErrorInterpreter.or[({type f[x]=Coproduct[Auth,Interact,x]})#f](
    Authenticator or Console)
  val interpreterFuture = FutureErrorInterpreter.or[({type f[x]=Coproduct[Auth,Interact,x]})#f](
    FutureAuthenticator or FutureConsole)

  def main(args: Array[String]): Unit = {
    val retValue = app.foldMap(interpreter)
    retValue match {
      //case Left(t: Throwable) => t.printStackTrace
      case Left(t: Throwable) => println(t)
      case v => println(v)
    }

    val futureValue = app.foldMap(interpreterFuture)
    futureValue onComplete {
      //case Failure(t) => t.printStackTrace
      case Failure(t) => println(t)
      case Success(value) => println(value)
    }
    Try(Await.ready(futureValue, 30.seconds))
  }
}
