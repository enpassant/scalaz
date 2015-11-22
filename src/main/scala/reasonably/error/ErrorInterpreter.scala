package reasonably.error

import reasonably.Algebra._
import ErrorAlgebra._

object ErrorInterpreter extends (ErrorMonad ~> EitherType) {
  def apply[A](i: ErrorMonad[A]) = i match {
    case Error(t: Throwable) =>
      Left(t)
  }
}
