package lectures.Pt1AS

import scala.annotation.targetName
import scala.util.Try

object DarkSugars extends App {

  //  #1: Methods with single param
  def singleArgMethod(arg: Int): String = s"$arg little ducks..."

  val description = singleArgMethod {
    // write some complex code
    42 // this is the param arg being sent to the function
  }

  val aTryInstance = Try { // like java's try {..}
    throw new RuntimeException()
  }

  //  for anonymous functions
  List(1, 2, 3).map { x =>
    x + 1
  }

  //  #2: single abstract method pattern
  trait Action {
    def act(x: Int): Int
  }

  val anInstance: Action = new Action {
    override def act(x: Int): Int = x + 1
  }

  val aFunkyInstance: Action = (x: Int) => x + 1 // compiler magic

  //  example: Runnable
  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("hello Scala")
  })

  val aSweeterThread = new Thread(() => println("sweet scala"))

  abstract class AnAbstractType {
    def implemented: Int = 23

    def f(a: Int): Unit //unimplemented
  }

  val anAbstractInstance: AnAbstractType = (a: Int) => println("sweet")

  //  #3: the :: and #:: methods (special)
  //  #:: prepend for streams

  val prependedList = 2 :: List(3, 4)
  println(prependedList)
  //  2.::(List(3, 4))  not actually what's happening
  //  List(3, 4).::(2)  what's actually happening

  //  scala spec states that operators associativity is dictated by last character of operator
  //  if it ends in : it's right associative
  //  else, left associative (what we consider normal)

  class MyStream[T] {
    //    function name is -->:
    @targetName("Some weird function")
    def -->:(value: T): MyStream[T] = this // some actual implementation
  }

  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]


  //  4: multi-word method naming
  class TeenGirl(name: String) {
    def `and then said`(gossip: String): Unit = println(s"$name said $gossip")
  }

  val lilly = new TeenGirl("Lily")
  lilly `and then said` "Scala is so sweet"

  //  5: infix types
  class Composite[A, B]

  //  val compose: Composite[Int, String] = ???
  val compose: Int Composite String = ???

  class -->[A, B]

  val towards: Int --> String = ???

  //  6: update() method, special like apply
  val anArray = Array(1, 2, 3)
  anArray(2) = 7
  //  ^^ this is rewritten to anArray.update(2, 7) by the compiler
  //  used in mutable collection

  //  7: setters for mutable containers
  class Mutable {
    private var internalMember: Int = 0 //private for OO encapsulation

    def member = internalMember //"getter"

    def member_=(value: Int): Unit =
      internalMember = value // "setter"
  }

  def aMutableContainer = new Mutable

  aMutableContainer.member = 42
  //    ^^ rewritten to aMutableContainer.member_=(42)

}
