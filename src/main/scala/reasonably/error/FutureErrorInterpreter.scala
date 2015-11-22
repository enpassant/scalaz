package reasonably.error

import reasonably.Algebra._
import ErrorAlgebra._

import scala.concurrent.{Future, Promise}

object FutureErrorInterpreter extends (ErrorMonad ~> Future) {
  def apply[A](i: ErrorMonad[A]) = i match {
    case Error(t: Throwable) =>
      val p = Promise()
      p.failure(t)
      p.future
  }
}
