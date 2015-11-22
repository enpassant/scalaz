package reasonably.auth

import reasonably.Algebra._
import AuthAlgebra._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object FutureAuthenticator extends (Auth ~> Future) {
  def apply[A](a: Auth[A]) = a match {
    case Login(uid, pwd) =>
      if (uid == "john.snow" && pwd == "Ghost")
        Future(Some(User("john.snow")))
      else if (uid == "john.rain" && pwd == "Ghost")
        Future(Some(User("john.rain")))
      else Future(None)
    case HasPermission(u, _) =>
      Future(u.id == "john.snow")
  }
}
