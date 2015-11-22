package reasonable.auth

import scalaz.~>
import scalaz.Scalaz._

import AuthAlgebra._
import reasonable.Tester._

object TestAuthenticator extends (Auth ~> Tester) {
  def apply[A](a: Auth[A]) = a match {
    case Login(uid, pwd) =>
      if (uid == "john.snow" && pwd == "Ghost")
        Some(User("john.snow")).point[Tester]
      else if (uid == "john.rain" && pwd == "Ghost")
        Some(User("john.rain")).point[Tester]
      else None.point[Tester]
    case HasPermission(u, _) =>
      (u.id == "john.snow").point[Tester]
  }
}
