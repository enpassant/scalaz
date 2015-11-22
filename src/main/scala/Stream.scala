
trait Stream[+A] {
    def uncons: Option[(A, Stream[A])]
    def isEmpty: Boolean = uncons.isEmpty

    def toList: List[A] = uncons match {
        case None => Nil
        case Some((h, t)) => h :: t.toList
    }

    def take(n: Int): Stream[A] = uncons match {
        case Some((h, t)) if n > 1 => Stream.cons(h, t.take(n - 1))
        case Some((h, _)) => Stream.cons(h, Stream.empty)
        case None => Stream.empty
    }

    def takeWhile(p: A => Boolean): Stream[A] = uncons match {
        case Some((h, t)) if p(h) =>
            Stream.cons(h, t.takeWhile(p))
        case _ => Stream.empty
    }
}

object Stream {
    def empty[A]: Stream[A] =
        new Stream[A] { def uncons = None }
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
        new Stream[A] {
            lazy val uncons = Some((hd, tl))
        }
    def apply[A](as: A*): Stream[A] =
        if (as.isEmpty) empty
        else cons(as.head, apply(as.tail: _*))
}

object StreamTest {

    def main(args: Array[String]) {
        val stream = Stream(3, 8, 2, 23, 1, 6, 7, 2, 3, 2, 11)

        println( stream.toList )
        println( stream.take(5).toList )
        println( stream.takeWhile(_ != 7).toList )
    }
}

// vim: set ts=4 sw=4 et:
