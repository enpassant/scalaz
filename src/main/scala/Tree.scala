
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeTest {
    def size[A](tree: Tree[A]): Int = tree match {
        case Leaf(_) => 1
        case Branch(left, right) => size(left) + size(right)
    }

    def maximum(tree: Tree[Int], maxValue: Int = Int.MinValue): Int = tree match {
        case Leaf(value) => value max maxValue
        case Branch(left, right) => maximum(left, maxValue) max maximum(right, maxValue)
    }

    def depth[A](tree: Tree[A]): Int = tree match {
        case Leaf(value) => 0
        case Branch(left, right) => 1 + (depth(left) max depth(right))
    }

    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
        case Leaf(value) => Leaf( f(value) )
        case Branch(left, right) => Branch( map(left)(f), map(right)(f) )
    }

    def fold[A, B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B = tree match {
        case Leaf(value) => f(value)
        case Branch(left, right) => g( fold(left)(f)(g), fold(right)(f)(g) )
    }

    def main(args: Array[String]) {
        val tree = Branch(Branch(Leaf(5), Branch(Leaf(6), Leaf(2))), Leaf(1))

        println( size(tree) )
        println( maximum(tree) )
        println( depth(tree) )

        println( maximum( map(tree)(_ * 5) ) )

        println( fold(tree)(a => 0)((b1,b2) => 1 + (b1 max b2)) )
    }
}

// vim: set ts=4 sw=4 et:
