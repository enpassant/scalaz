package reasonable.interact

import scalaz.~>
import scalaz.Scalaz._

import reasonable.Algebra._
import InteractAlgebra._

object Console extends (Interact ~> EitherType) {
  def apply[A](i: Interact[A]) = i match {
    case Ask(prompt) =>
      println(prompt)
      (scala.io.StdIn.readLine).right
    case Tell(msg) =>
      (println(msg)).right
    case End =>
      (new Exception("End")).left
  }
}
