package reasonably.auth

import reasonably.Algebra._
import AuthAlgebra._

object Authenticator extends (Auth ~> EitherType) {
  def apply[A](a: Auth[A]) = a match {
    case Login(uid, pwd) =>
      if (uid == "john.snow" && pwd == "Ghost")
        Right(Some(User("john.snow")))
      else if (uid == "john.rain" && pwd == "Ghost")
        Right(Some(User("john.rain")))
      else Right(None)
    case HasPermission(u, _) =>
      Right(u.id == "john.snow")
  }
}
