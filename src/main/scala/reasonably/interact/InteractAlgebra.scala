package reasonably.interact

import language.higherKinds

import reasonably.Algebra._

object InteractAlgebra {
  sealed trait Interact[A]
  case class Ask(prompt: String) extends Interact[String]
  case class Tell(msg: String) extends Interact[Unit]
  case object End extends Interact[Unit]

  class Interacts[F[_]](implicit I: Inject[Interact,F]) {
    def tell(msg: String): Free[F,Unit] = lift(Tell(msg))
    def ask(prompt: String): Free[F,String] = lift(Ask(prompt))
    def end(): Free[F,Unit] = lift(End)
  }

  object Interacts {
    implicit def instance[F[_]](implicit I: Inject[Interact,F]): Interacts[F] = new Interacts[F]
  }
}

