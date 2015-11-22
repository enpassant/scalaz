package reasonable.auth

import scalaz.{\/, ~>}
import scalaz.Scalaz._

import reasonable.Algebra._
import AuthAlgebra._

object Authenticator extends (Auth ~> EitherType) {
  def apply[A](a: Auth[A]) = a match {
    case Login(uid, pwd) =>
      if (uid == "john.snow" && pwd == "Ghost")
        (Some(User("john.snow"))).right
      else if (uid == "john.rain" && pwd == "Ghost")
        (Some(User("john.rain"))).right
      else (None).right
    case HasPermission(u, _) =>
      (u.id == "john.snow").right
  }
}
