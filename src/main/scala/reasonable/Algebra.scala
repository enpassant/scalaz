package reasonable

import scalaz.{Coproduct, Inject, ~>, \/, -\/, \/-}
import scalaz.Free

import language.higherKinds

object Algebra {
  type EitherType[A] = \/[Throwable, A]

  def lift[F[_], G[_], A](fa: F[A])(implicit I: Inject[F, G]): Free[G, A] =
    Free.liftF(I.inj(fa))

  def or[F[_], G[_], H[_]](f: F ~> H, g: G ~> H) = {
    //type Ret[B] = Coproduct[F,G,B]
    //new (Ret ~> H) {
    new (({type f[x] = Coproduct[F,G,x]})#f ~> H) {
      def apply[A](c: Coproduct[F,G,A]): H[A] = c.run match {
        case -\/(fa) => f(fa)
        case \/-(ha) => g(ha)
      }
    }
  }
}

