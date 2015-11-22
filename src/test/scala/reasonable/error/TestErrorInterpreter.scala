package reasonable.error

import scalaz.~>
import scalaz.{-\/}

import ErrorAlgebra._
import reasonable.Tester._

object TestErrorInterpreter extends (ErrorMonad ~> Tester) {
  def apply[A](i: ErrorMonad[A]) = i match {
    case Error(e) => _ => (Nil, -\/(e))
  }
}
