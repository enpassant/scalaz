package reasonably.auth

import AuthAlgebra._
import reasonably.Algebra._
import reasonably.Tester._

object TestAuthenticator extends (Auth ~> Tester) {
  def apply[A](a: Auth[A]) = a match {
    case Login(uid, pwd) =>
      if (uid == "john.snow" && pwd == "Ghost")
        m => (Nil, Right(Some(User("john.snow"))))
      else if (uid == "john.rain" && pwd == "Ghost")
        m => (Nil, Right(Some(User("john.rain"))))
      else m => (Nil, Right(None))
    case HasPermission(u, _) =>
      m => (Nil, Right(u.id == "john.snow"))
  }
}
