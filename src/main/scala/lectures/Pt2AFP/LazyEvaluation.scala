package lectures.Pt2AFP

object LazyEvaluation extends App {

  // val x: Int = throw new RuntimeException()
  // lazy val y: Int = throw new RuntimeException()
  // Lazy delays the evaluation of values

  // expression is evaluated just once, then the value is stored
  lazy val x: Int = {
    println("hello")
    42
  }

  println(x)
  println(x)

  // Implications
  // 1. Side effects can be shortcicuited
  def sideEffectCondition: Boolean = {
    println("boo")
    true
  }

  def simpleCondition: Boolean = false

  lazy val lazyCondition = sideEffectCondition

  println(if (simpleCondition && lazyCondition) "yes" else "no")

  // 2 In conjunction with call by name
  def byNameMethod(n: => Int): Int =
    // call by need
    lazy val t = n // only evaluated once
    t + t + t + t + 1

  def retrieveMagicValue = {
    // side effect after long computation
    println("waiting")
    Thread.sleep(1000)
    42
  }

  println(byNameMethod(retrieveMagicValue))

  //3. Filtering with lazy vals
  def lessThan30(i: Int): Boolean = {
    println(s"$i is less than 30? ")
    i < 30
  }

  def greaterThan20(i: Int): Boolean = {
    println(s"$i is greater than 20? ")
    i > 20
  }

  val numbers = List(1, 25, 40, 5, 23)
  val lt30 = numbers.filter(lessThan30)
  val gt20 = lt30.filter(greaterThan20)
  println(gt20)

  val lt30Lazy = numbers.withFilter(lessThan30) //lazy vals under the hood
  val gt20Lazy = lt30Lazy.withFilter(greaterThan20)
  println("----------------")
  println(gt20Lazy)
  gt20Lazy.foreach(println)

  // for-comprehensions use withFilter for guards (lazy)
}
