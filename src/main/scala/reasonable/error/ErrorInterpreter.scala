package reasonable.error

import scalaz.~>
import scalaz.Scalaz._

import reasonable.Algebra._
import ErrorAlgebra._

object ErrorInterpreter extends (ErrorMonad ~> EitherType) {
  def apply[A](i: ErrorMonad[A]) = i match {
    case Error(t: Throwable) =>
      t.left
  }
}
