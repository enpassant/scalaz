package reasonable

import scalaz.Monad
import scalaz.{-\/, \/-}
import Algebra._

object Tester {
  type Tester[+A] = Map[String, String] => (List[String], EitherType[A])

  implicit val testerMonad = new Monad[Tester] {
    def point[A](a: => A) = _ => (List(), \/-(a))
    def bind[A, B](t: Tester[A])(f: A => Tester[B]) =
      m => {
        t(m) match {
          case (o, -\/(e)) => (o, -\/(e))
          case _ =>
            val (o1, a) = t(m)
            val (o2, b) = f(a.toOption.get)(m)
            (o1 ++ o2, b)
        }
      }
  }
}

