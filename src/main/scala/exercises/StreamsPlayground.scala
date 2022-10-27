package exercises

import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.List

/**
 * Exercise: implement a lazily evaluated, singly linked STREAM of elements.
 * naturals = MyStream.from(1)(x => x + 1) = stream of natural numbers (potentially infinite!)
 * naturals.take(100).foreach(println) // lazily evaluated stream of the first 100 naturals (finite stream)
 * naturals.foreach(println) // will crash - infinite!
 * naturals.map(_ * 2) // stream of all even numbers (potentially infinite)
 */
abstract class MyStream[+A] {
  def isEmpty: Boolean

  def head: A

  def tail: MyStream[A]

  @targetName("Prepend")
  def #::[B >: A](element: B): MyStream[B]

  @targetName("Concatenate")
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B]

  def foreach(f: A => Unit): Unit

  def map[B](f: A => B): MyStream[B]

  def flatMap[B](f: A => MyStream[B]): MyStream[B]

  def filter(predicate: A => Boolean): MyStream[A]

  // Take first n elements
  def take(n: Int): MyStream[A]

  def takeAsList(n: Int): List[A] = take(n).toList()

  @tailrec
  final def toList[B >: A](acc: List[B] = Nil): List[B] =
    if (isEmpty) acc.reverse
    else tail.toList(head :: acc)
}

object EmptyStream extends MyStream[Nothing] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException()

  def tail: MyStream[Nothing] = throw new NoSuchElementException()

  @targetName("Prepend")
  def #::[B >: Nothing](element: B): MyStream[B] = new Cons(element, this)

  @targetName("Concatenate")
  def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream

  def foreach(f: Nothing => Unit): Unit = ()

  def map[B](f: Nothing => B): MyStream[B] = this

  def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this

  def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this

  // Take first n elements
  def take(n: Int): MyStream[Nothing] = this

}

class Cons[+A](hd: A, tl: => MyStream[A]) extends MyStream[A] {
  def isEmpty: Boolean = false

  override val head: A = hd

  override lazy val tail: MyStream[A] = tl // call by need

  @targetName("Prepend")
  def #::[B >: A](element: B): MyStream[B] = new Cons[B](element, this)

  @targetName("Concatenate")
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = new Cons[B](head, tail ++ anotherStream)

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  def map[B](f: A => B): MyStream[B] = new Cons[B](f(head), tail.map(f))

  def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)


  def filter(predicate: A => Boolean): MyStream[A] =
    if (predicate(head)) new Cons[A](head, tail.filter(predicate))
    else tail.filter(predicate)

  // Take first n elements
  def take(n: Int): MyStream[A] =
    if (n <= 0) EmptyStream
    else if (n == 1) new Cons(head, EmptyStream)
    else new Cons(head, tail.take(n - 1))

}

object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] =
    new Cons[A](start, MyStream.from(generator(start))(generator))
}

object StreamsPlayground extends App {
  val naturals = MyStream.from(1)(_ + 1)
  println(naturals.head)
  println(naturals.tail.head)
  println(naturals.tail.tail.head)

  val startFrom0 = 0 #:: naturals
  println(startFrom0.head)

  startFrom0.take(10000).foreach(println)

  // functional
  println(startFrom0.map(_ * 2).take(100).toList())
  println(startFrom0.flatMap(x => new Cons(x, new Cons(x + 1, EmptyStream))).take(10).toList())


  // Exercises on streams
  // 1 - stream of fibonacci exercise
  // 2 - stream of prime numbers with Eratosthenes' sieve
  /*
   * [2,3,4...]
   * filter out all numbers divisible by 2
   * [2,3,5,7,9,11 ..]
   * filter out all numbers divisible by 3
   * [2, 3,5,7,11,13,17 ...]
   * filter out all numbers diviseble by 5
   * ...
   */

  //@tailrec
  def fibonacci(first: BigInt, second: BigInt): MyStream[BigInt] =
    new Cons(first, fibonacci(second, first + second))

  println(fibonacci(1, 1).take(100).toList())

  def erathosthenes(numbers: MyStream[Int]): MyStream[Int] =
    if (numbers.isEmpty) numbers
    else new Cons[Int](numbers.head, erathosthenes(numbers.tail.filter(_ % numbers.head != 0)))

  println(erathosthenes(MyStream.from(2)(_ + 1)).take(100).toList())
}
