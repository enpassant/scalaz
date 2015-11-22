import scalaz._
import Scalaz._
import scalaz.Free._

object StateSample extends App {
    val fib: State[(Int, Int), Int] = State { s => ((s._2, s._1 + s._2), s._1) }
    println( fib.lift[Trampoline].replicateM(5000).eval((1, 1)).run )
    println( fib.lift[Trampoline].iterateWhile(_ < 1800000000).eval((1, 1)).run )

    val fibC: State[(Int, Int, Int), Int] = State { s =>
        ((s._2, s._1 + s._2, s._3 + 1), s._3) }
    println( fibC.lift[Trampoline].iterateWhile(_ < 5000).run((1, 1, 1)).run )
}
// vim: set ts=4 sw=4 et:
