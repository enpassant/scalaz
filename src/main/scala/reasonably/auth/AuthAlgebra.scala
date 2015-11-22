package reasonably.auth

import language.higherKinds

import reasonably.Algebra._

object AuthAlgebra {
  type UserID = String
  type Password = String
  type Permission = String
  case class User(id: String)

  sealed trait Auth[A]
  case class Login(u: UserID, p: Password) extends Auth[Option[User]]
  case class HasPermission(u: User, p: Permission) extends Auth[Boolean]

  class Auths[F[_]](implicit I: Inject[Auth,F]) {
    def login(id: UserID, pwd: Password): Free[F,Option[User]] =
      lift(Login(id, pwd))
    def hasPermission(u: User, p: Permission): Free[F,Boolean] =
      lift(HasPermission(u, p))
  }

  object Auths {
    implicit def instance[F[_]](implicit I: Inject[Auth,F]): Auths[F] = new Auths[F]
  }
}

