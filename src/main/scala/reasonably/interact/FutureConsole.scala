package reasonably.interact

import reasonably.Algebra._
import InteractAlgebra._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

object FutureConsole extends (Interact ~> Future) {
  def apply[A](i: Interact[A]) = i match {
    case Ask(prompt) =>
      println(prompt)
      Future(scala.io.StdIn.readLine)
    case Tell(msg) =>
      Future(println(msg))
    case End =>
      val p = Promise()
      p.failure(new Exception("End"))
      p.future
  }
}
