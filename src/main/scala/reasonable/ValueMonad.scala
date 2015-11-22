package reasonable

import language.higherKinds
import scala.util.Success
import scalaz.Monad

sealed trait ReturnValue[+A]
case class Value[A, F[A]](a: A, fa: F[A]) extends ReturnValue[A]
case class Error(a: Any) extends ReturnValue[Nothing]

object ValueMonad {
  implicit val valueMonad = new Monad[ReturnValue] {
    def point[A](a: => A) = Value[A, Option](a, Some(a))
    def bind[A, B](a: ReturnValue[A])(f: A => ReturnValue[B]) = a match {
      case Value(a, fa) => f(a) match {
        case Value(b, Some(c)) => Value(b, Some(b))
        case Value(b, Success(c)) => Value(b, Success(b))
        case v => v
      }
      case e @ Error(_) => e
    }
  }
}

