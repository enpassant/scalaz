import scalaz._, Scalaz._, Free._

sealed trait ForthOperators[+A]

final case class Push[A](value: Int, o: A) extends ForthOperators[A]
final case class Add[A](o: A) extends ForthOperators[A]
final case class Mul[A](o: A) extends ForthOperators[A]
final case class Dup[A](o: A) extends ForthOperators[A]
final case class End[A](o: A) extends ForthOperators[A]

object FreeSample {
  //type ForthProg[A] = Free.FreeC[ForthOperators, A]
  type ForthProg[A] = Free[ForthOperators, A]

  implicit val ForthFunctor: Functor[ForthOperators] = new Functor[ForthOperators] {
    def map[A, B](fa: ForthOperators[A])(f: A => B): ForthOperators[B] =
      fa match {
        case Push(value, cont) => Push(value, f(cont))
        case Add(cont) => Add(f(cont))
        case Mul(cont) => Mul(f(cont))
        case Dup(cont) => Dup(f(cont))
        case End(cont) => End(f(cont))
      }
  }

  import scala.language.implicitConversions
  //implicit def liftForth[A](forth: ForthOperators[A]): ForthProg[A] = Free.liftFC(forth)
  def liftForth[A](forth: ForthOperators[A]): ForthProg[A] = Free.liftF(forth)
  def liftPoint[A](forth: A): ForthProg[A] = Free.point(forth)

  def push(value: Int)  = liftForth(Push(value, ()))
  def add = liftForth(Add("Add"))
  def mul = liftForth(Mul(()))
  def dup = liftForth(Dup(()))
  def end = liftForth(End(()))

  type Stack = List[Int]
  type StackState[A] = State[Stack, A]

  def runProgram: ForthOperators ~> StackState = new (ForthOperators ~> StackState) {
    def apply[A](t: ForthOperators[A]) : StackState[A] = t match {
      case Push(value : Int, cont) =>
        State((a: Stack) => (value::a, cont))
      case Add(cont) =>
        State((stack : Stack) => {
          val a :: b :: tail = stack
          ((a + b) :: tail, cont)
        })
      case Mul(cont) =>
        State((stack : Stack) => {
          val a :: b :: tail = stack
          ((a * b) :: tail, cont)
        })
      case Dup(cont) =>
        State((stack : Stack) => {
          val a :: tail = stack
          (a :: a :: tail, cont)
        })
      case End(cont) =>
        // This doesn't work as intended there may not
        // be a way to do this using ~>
        State((a : Stack) => (a, cont))
    }
  }

  def runFn(stack: List[Int], program: ForthOperators[ForthProg[String]]): (List[Int], ForthProg[String]) = program match {
    case Push(value, cont) =>
      (value :: stack, cont)
    case Add(cont) =>
      val a :: b :: tail = stack
      ((a + b) :: tail, cont)
    case Mul(cont) =>
      val a :: b :: tail = stack
      ((a * b) :: tail, cont)
    case Dup(cont) =>
      val a :: tail = stack
      (a :: a :: tail, cont)
    case End(cont) =>
      (stack, Free.point("end..."))
  }

  //def run2Program: ForthOperators ~> Option = new (ForthOperators ~> Option) {
    //def apply[A](t: ForthOperators[A]): Option[A] = t match {
      //case Push(value: Int, cont) =>
        //Some(s"Push($value)\n")
      //case Add(cont) =>
        ////val a :: b :: tail = cont
        ////(a + b) :: tail
        //Some(s"Add\n")
      //case Mul(cont) =>
        ////val a :: b :: tail = cont
        ////(a * b) :: tail
        //Some(s"Mul\n")
      //case Dup(cont) =>
        ////val a :: tail = cont
        ////(a :: cont)
        //Some(s"Dup\n")
      //case End(cont) =>
        ////cont
        //None
    //}
  //}

  def printProgram: ForthOperators ~> Option = new (ForthOperators ~> Option) {
    def apply[A](t: ForthOperators[A]): Option[A] = t match {
      case Push(value: Int, cont) =>
        println(s"Push $value")
        Some(cont)
      case Add(cont) =>
        println("Add")
        Some(cont)
      case Mul(cont) =>
        println("Mul")
        Some(cont)
      case Dup(cont) =>
        println("Dup")
        Some(cont)
      case End(cont) =>
        println("End")
        None
    }
  }

  val testProg = for {
    _ <- push(3)
    _ <- push(6)
    _ <- add
    _ <- push(7)
    _ <- push(2)
    _ <- add
    //_ <- end
    _ <- mul
    _ <- dup
    result <- add
  } yield result

  val square = for {
    _ <- dup
    _ <- mul
  } yield ()

  val testProg2 = for {
    _ <- push(3)
    _ <- square
    _ <- push(4)
    n <- square
    _ <- add
  } yield n

  def main(args: Array[String]): Unit = {
    println(testProg.foldMap(printProgram))
    println(testProg.foldRun(List.empty[Int])(runFn))

    //Free.runFC(testProg)(printProgram)
    //println(Free.runFC(testProg)(runProgram).exec(List[Int]()))
  }
}
