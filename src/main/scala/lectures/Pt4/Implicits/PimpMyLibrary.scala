package lectures.Pt4.Implicits

object PimpMyLibrary extends App {

  //  2.isPrime
  implicit class RichInt(val value: Int) extends AnyVal {
    def isEven: Boolean = value % 2 == 0

    def sqrt: Double = Math.sqrt(value)

    def times(f: () => Unit): Unit = {
      def timesAux(n: Int): Unit = {
        if (n <= 0) ()
        else {
          f()
          timesAux(n - 1)
        }
      }

      timesAux(value)
    }

    def *[T](list: List[T]): List[T] = {
      def concatenate(n: Int): List[T] =
        if (n <= 0) List()
        else concatenate(n - 1) ++ list

      concatenate(value)
    }

  }

  implicit class RicherInt(value: RichInt) {
    def isOdd: Boolean = !value.isEven
  }


  new RichInt(42).sqrt
  42.sqrt

  //  ^^^ type enrichment (pimping library)

  1 to 10

  import scala.concurrent.duration._

  3.second

  //  compiler does not do multiple implicit searches
  //  42.isOdd // does not work


  /* EXERCISES
  1. Enrich string class
    - asInt
    - encrypt +2 chars
        john -> lnjp

  2. Further enrich Int class
    - times(function
   */

  implicit class RichString(value: String) {
    def asInt: Int = Integer.valueOf(value) // java.lang.integer -> Int

    def encrypt(cypherDistance: Int): String = value.map(c => (c + cypherDistance).asInstanceOf[Char])
  }

  println("3".asInt + 4)
  println("John".encrypt(2))

  3.times(() => println("Scala rocks"))
  println(3 * List(1, 2, 3))

  //  "3" / 4
  implicit def stringToInt(s: String): Int = Integer.valueOf(s)

  println("6" / 2) //  stringToInt("6") / 2

  /* TIPS
  - Keep type enrichment to implicit classes and type classes
  - Avoid implicit defs as much as possible
  - Package implicit clearly, bring to scope only what's necessary
  - If you need conversions, make them specific

  */
}
