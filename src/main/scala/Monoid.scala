
trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object WC {
    def apply() = Stub("")

    def apply(str: String) = {
        val indexOf = str.indexOf(' ')
        if (indexOf < 0) Stub(str)
        else {
            val (str1, str2) = str.splitAt(indexOf)
            val lastIndexOf = str2.lastIndexOf(' ') + 1
            val (str3, str4) = str2.splitAt(lastIndexOf)
            Part(str1, str3.count(_ == ' ') - 1, str4)
        }
    }

    def count(wc: WC) = wc match {
        case Stub("") => 0
        case Stub(_) => 1
        case Part("", c, "") => c
        case Part("", c, _) => c + 1
        case Part(_, c, "") => c + 1
        case Part(_, c, _) => c + 2
    }
}

object MonoidTest {

    val wcMonoid = new Monoid[WC] {
        def op(a1: WC, a2: WC): WC = a1 match {
            case Stub(s1) => a2 match {
                case Stub(s2) => Stub(s1 + s2)
                case Part(ls1, w, rs1) => Part(s1 + ls1, w, rs1)
            }
            case Part(ls1, w1, rs1) => a2 match {
                case Stub(s2) => Part(ls1, w1, rs1 + s2)
                case Part(ls2, w2, rs2) =>
                    Part(ls1,
                        w1 + w2 + (if (rs1 == "" && ls2 == "") 0 else 1),
                        rs2)
            }
        }
        def zero: WC = WC()
    }

    def counter(str: String): WC = {
        val length = str.length
        if (length < 10) {
            WC(str)
        } else {
            val (str1, str2) = str.splitAt(length / 2)
            wcMonoid.op(counter(str1), counter(str2))
        }
    }

    def main(args: Array[String]) {
        val longText = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."

        val splitLength = 5
        val slideList = longText.sliding(splitLength, splitLength).toList
        val splits: List[WC] = slideList.map( text => WC(text) )

        println(longText)
        val result = splits.foldLeft(wcMonoid.zero)(wcMonoid.op)
        println(result)
        val result2 = splits.foldRight(wcMonoid.zero)(wcMonoid.op)
        println(result2)
        val result3 = counter(longText)
        println(result3)
        println(WC.count(result))
    }
}
// vim: set ts=4 sw=4 et:
