package reasonably.interact

import InteractAlgebra._
import reasonably.Algebra._
import reasonably.Tester._

object TestConsole extends (Interact ~> Tester) {
  def apply[A](i: Interact[A]) = i match {
    case Ask(prompt) => m => (Nil, Right(m(prompt)))
    case Tell(msg) => _ => (List(msg), Right(()))
    case End => _ => (Nil, Left(new Exception("End")))
  }
}
