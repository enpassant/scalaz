package reasonable.interact

import scalaz.~>
import scalaz.{-\/, \/-}

import InteractAlgebra._
import reasonable.Tester._

object TestConsole extends (Interact ~> Tester) {
  def apply[A](i: Interact[A]) = i match {
    case Ask(prompt) => m => (Nil, \/-(m(prompt)))
    case Tell(msg) => _ => (List(msg), \/-(()))
    case End => _ => (Nil, -\/(new Exception("End")))
  }
}
