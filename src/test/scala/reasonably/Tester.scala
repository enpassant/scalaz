package reasonably

import reasonably.Algebra._

object Tester {
  type Tester[+A] = Map[String, String] => (List[String], EitherType[A])

  implicit val testerMonad = new Monad[Tester] {
    def pure[A](a: => A): Tester[A] = _ => (List(), Right(a))
    def flatMap[A, B](t: Tester[A])(f: A => Tester[B]): Tester[B] =
      m => {
        t(m) match {
          case (o, Left(e)) => (o, Left(e))
          case _ =>
            val (o1, a) = t(m)
            val (o2, b) = f(a.right.get)(m)
            (o1 ++ o2, b)
        }
      }
  }
}

