package reasonably

import language.higherKinds

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Algebra {
  trait Monad[M[_]] {
    def pure[A](a: => A): M[A]
    def flatMap[A,B](a: M[A])(f: A => M[B]): M[B]
  }

  object Monad {
    def apply[F[_]:Monad]: Monad[F] = implicitly[Monad[F]]
  }

  case class Coproduct[F[_],G[_],A](run: Either[F[A],G[A]])

  trait ~>[F[_],G[_]] { self =>
    def apply[A](f: F[A]): G[A]

    def or[H[_]](f: H ~> G): ({ type f[x] = Coproduct[F, H, x]})#f ~> G =
      new (({type f[x] = Coproduct[F,H,x]})#f ~> G) {
        def apply[A](c: Coproduct[F,H,A]): G[A] = c.run match {
          case Left(fa) => self(fa)
          case Right(ha) => f(ha)
        }
      }
  }

  sealed trait Free[F[_],A] {
    def flatMap[B](f: A => Free[F,B]): Free[F,B] =
      this match {
        case Return(a) => f(a)
        case Bind(fx, g) =>
          Bind(fx, g andThen (_ flatMap f))
      }

    def map[B](f: A => B): Free[F,B] =
      flatMap(a => Return(f(a)))

    def foldMap[G[_]:Monad](f: F ~> G): G[A] =
      this match {
        case Return(a) => Monad[G].pure(a)
        case Bind(fx, g) =>
          Monad[G].flatMap(f(fx)) { a =>
                  g(a).foldMap(f)
          }
      }
  }

  case class Return[F[_],A](a: A) extends Free[F,A]

  case class Bind[F[_],I,A](
    a: F[I],
    f: I => Free[F,A]) extends Free[F,A]

  sealed trait Inject[F[_],G[_]] {
    def inj[A](sub: F[A]): G[A]
    def prj[A](sup: G[A]): Option[F[A]]
  }

  object Inject {
    implicit def injRefl[F[_]] = new Inject[F,F] {
      def inj[A](sub: F[A]) = sub
      def prj[A](sup: F[A]) = Some(sup)
    }

    implicit def injLeft[F[_],G[_]] = new Inject[F,({type λ[α] = Coproduct[F,G,α]})#λ] {
      def inj[A](sub: F[A]) = Coproduct(Left(sub))
      def prj[A](sup: Coproduct[F,G,A]) = sup.run match {
        case Left(fa) => Some(fa)
        case Right(_) => None
      }
    }

    implicit def injRight[F[_],G[_],H[_]](implicit I: Inject[F,G]) =
      new Inject[F,({type f[x] = Coproduct[H,G,x]})#f] {
        def inj[A](sub: F[A]) = Coproduct(Right(I.inj(sub)))
        def prj[A](sup: Coproduct[H,G,A]) = sup.run match {
          case Left(_) => None
          case Right(x) => I.prj(x)
      }
    }
  }

  def lift[F[_],G[_],A](f: F[A])(implicit I: Inject[F,G]): Free[G,A] =
    Bind(I.inj(f), Return(_:A))

  type Id[A] = A

  implicit val identityMonad: Monad[Id] = new Monad[Id] {
    def pure[A](a: => A) = a
    def flatMap[A,B](a: A)(f: A => B) = f(a)
  }

  type EitherType[A] = Either[Throwable, A]

  class EitherMonad extends Monad[EitherType] {
    //def left[A](a: => A): Left[Throwable, A]
    def pure[B](b: => B): EitherType[B] = Right[Throwable, B](b)
    def flatMap[B, C](m: EitherType[B])(f: B => EitherType[C]): EitherType[C] =
      m.right.flatMap(f)
  }

  implicit val eitherMonad = new EitherMonad()

  implicit val futureMonad = new Monad[Future]() {
    def pure[A](a: => A): Future[A] = Future(a)
    def flatMap[A, B](m: Future[A])(f: A => Future[B]): Future[B] =
      m.flatMap(f)
  }

}

