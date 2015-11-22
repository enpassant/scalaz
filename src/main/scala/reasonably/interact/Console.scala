package reasonably.interact

import reasonably.Algebra._
import InteractAlgebra._

object Console extends (Interact ~> EitherType) {
  def apply[A](i: Interact[A]) = i match {
    case Ask(prompt) =>
      println(prompt)
      Right(scala.io.StdIn.readLine)
    case Tell(msg) =>
      Right(println(msg))
    case End =>
      Left(new Exception("End"))
  }
}
