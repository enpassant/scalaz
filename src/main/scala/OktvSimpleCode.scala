import scala.collection.mutable.ArrayBuffer

object OktvSimpleCode {

  def Valami(N: Int, X: List[Int]): (Int, List[Int], List[Int]) = {
    var A = 0
    val B = ArrayBuffer[Int]()
    val C = ArrayBuffer[Int]()
    var D = 0
    var E = 0

    for (i <- 1 until N) {
      if (X(i-1)==0 && X(i)>0) { D = 0; E = 0 }
      if (X(i)>0) { D = D + 1 }
      if (X(i)>E) { E = X(i) }
      if (X(i)==0 && X(i-1)>0) { A=A+1; B.append(D); C.append(E) }
    }

    (A, B.toList, C.toList)
  }

  case class IntervalLengthAndMax(length: Int, max: Int) {
    def add(value: Int) = IntervalLengthAndMax(length + 1, math.max(max, value))
  }
  private val emptyInterval = IntervalLengthAndMax(0, 0)

  def calcIntervals(values: List[Int]): (List[IntervalLengthAndMax]) = {
    val initState = (List.empty[IntervalLengthAndMax], emptyInterval)
    val (intervals, currentInterval) = values.foldLeft(initState) {
      case ((intervals, currentInterval), value) =>
        (currentInterval, value) match {
          case (`emptyInterval`, 0) => (intervals, currentInterval)
          case (_, 0) => (currentInterval :: intervals, emptyInterval)
          case _ => (intervals, currentInterval.add(value))
        }
    }
    if (currentInterval != emptyInterval) (currentInterval :: intervals).reverse
    else intervals.reverse
  }

  private def calcNextState(
    state: (List[IntervalLengthAndMax], IntervalLengthAndMax),
    value: Int) =
  {
    val (intervals, currentInterval) = state
    (currentInterval, value) match {
      case (`emptyInterval`, 0) => state
      case (_, 0) => (currentInterval :: intervals, emptyInterval)
      case _ => (intervals, currentInterval.add(value))
    }
  }

  def calcIntervals2(values: List[Int]): (List[IntervalLengthAndMax]) = {
    val initState = (List.empty[IntervalLengthAndMax], emptyInterval)
    val (intervals, currentInterval) = values.foldLeft(initState)(calcNextState)
    if (currentInterval != emptyInterval) (currentInterval :: intervals).reverse
    else intervals.reverse
  }

  def main(args: Array[String]): Unit = {
    val N = 14
    val X = List(0, 0, 2, 4, 0, 1, 3, 1, 0, 0, 2, 4, 5, 0)

    val (a, b, c) = Valami(N, X)
    println(s"Result 1: ($a, $b, $c)")

    val intervalsLengthAndMax = calcIntervals(X)
    println(s"Result 2: ($intervalsLengthAndMax)")

    val intervalsLengthAndMax2 = calcIntervals2(X)
    println(s"Result 3: ($intervalsLengthAndMax2)")
  }
}
