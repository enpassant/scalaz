package reasonably.error

import ErrorAlgebra._
import reasonably.Algebra._
import reasonably.Tester._

object TestErrorInterpreter extends (ErrorMonad ~> Tester) {
  def apply[A](i: ErrorMonad[A]) = i match {
    case Error(e) => _ => (Nil, Left(e))
  }
}
