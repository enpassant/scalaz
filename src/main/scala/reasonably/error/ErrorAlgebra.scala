package reasonably.error

import language.higherKinds

import reasonably.Algebra._

object ErrorAlgebra {
  sealed trait ErrorMonad[A]
  case class Error(msg: Throwable) extends ErrorMonad[Throwable]

  class ErrorMonads[F[_]](implicit I: Inject[ErrorMonad,F]) {
    def error(t: Throwable): Free[F,Throwable] = lift(Error(t))
  }

  object ErrorMonads {
    implicit def instance[F[_]](implicit I: Inject[ErrorMonad,F]): ErrorMonads[F] = new ErrorMonads[F]
  }
}

