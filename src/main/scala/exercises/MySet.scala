package exercises

import lectures.Pt1AS.AdvancedPatternMatching.MyList

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {
  /* Exercise: implement a functional set
   1. Contains method
   2. Add element to set
   3. concatenate sets
   4. map
   5. flatMap
   6. filter
   7. foreach
   */
  override def apply(elem: A): Boolean =
    contains(elem)

  def contains(elem: A): Boolean

  def +(elem: A): MySet[A]

  def ++(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]

  def flatMap[B](f: A => MySet[B]): MySet[B]

  def filter(predicate: A => Boolean): MySet[A]

  def foreach[B](f: A => Unit): Unit
}

object MySet {
  def apply[A](values: A*): MySet[A] =
    @tailrec
    def go(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if (valSeq.isEmpty) acc
      else go(valSeq.tail, acc + valSeq.head)

    go(values.toSeq, new EmptySet[A])
}

class EmptySet[A] extends MySet[A] {
  def contains(elem: A): Boolean = false

  def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)

  def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  def map[B](f: A => B): MySet[B] = new EmptySet[B]

  def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]

  def filter(predicate: A => Boolean): MySet[A] = this

  def foreach[B](f: A => Unit): Unit = ()
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  def contains(elem: A): Boolean =
    (head == elem) || tail.contains(elem)

  def +(elem: A): MySet[A] =
    if (this.contains(elem)) this
    else new NonEmptySet[A](elem, this)

  def ++(anotherSet: MySet[A]): MySet[A] =
    tail ++ anotherSet + head


  def map[B](f: A => B): MySet[B] =
    tail.map(f) + f(head)

  def flatMap[B](f: A => MySet[B]): MySet[B] =
    tail.flatMap(f) ++ f(head)


  def filter(predicate: A => Boolean): MySet[A] =
    val filteredTail = tail.filter(predicate)
    if (predicate(head)) filteredTail + head
    else filteredTail

  def foreach[B](f: A => Unit): Unit =
    f(head)
    tail.foreach(f)
}

object MySetPlayground extends App {
  val s = MySet(1, 2, 3, 4)
  println("Testing MySet")
  s.foreach(println)
  println("Testing MySet")
  (s + 5).foreach(println)
  println("Testing MySet")
  (s + 5 ++ MySet(-1, -2) + 3).foreach(println)
}
