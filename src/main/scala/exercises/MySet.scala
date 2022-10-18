package exercises

import lectures.Pt1AS.AdvancedPatternMatching.MyList

import scala.annotation.{tailrec, targetName}

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

  @targetName("Add")
  def +(elem: A): MySet[A]

  @targetName("Concatenate")
  def ++(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]

  def flatMap[B](f: A => MySet[B]): MySet[B]

  def filter(predicate: A => Boolean): MySet[A]

  def foreach[B](f: A => Unit): Unit

  /* Exercise 2
  1. remove an element
  3. difference with another set
  2. intersection with another set
   */
  @targetName("Remove")
  def -(elem: A): MySet[A]

  @targetName("Difference")
  def --(anotherSet: MySet[A]): MySet[A]

  @targetName("Intersection")
  def &(anotherSet: MySet[A]): MySet[A]

  // Exercise 3 - implement unary_! => negation of a set
  //  set[1,2,3] => ???
  @targetName("Not")
  def unary_! : MySet[A]
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

  @targetName("Add")
  def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)

  @targetName("Concatenate")
  def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  def map[B](f: A => B): MySet[B] = new EmptySet[B]

  def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]

  def filter(predicate: A => Boolean): MySet[A] = this

  def foreach[B](f: A => Unit): Unit = ()

  //  Part 2
  @targetName("Remove")
  def -(elem: A): MySet[A] = this

  @targetName("Difference")
  def --(anotherSet: MySet[A]): MySet[A] = this

  @targetName("Intersection")
  def &(anotherSet: MySet[A]): MySet[A] = this

  //  Part 3
  @targetName("Not")
  def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)
}

//  all elements of type A which satisfy a property
//  {x in A | property(x)
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
  override def contains(elem: A): Boolean = property(elem)

  //{ x in A | property(x) } + element ==> { x in A | property(x) || x== element }
  @targetName("Add")
  override def +(elem: A): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || x == elem)

  //{ x in A | property(x) } +| element ==> { x in A | property(x) ||  set contains x }
  @targetName("Concatenate")
  override def ++(anotherSet: MySet[A]): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || anotherSet(x))

  override def map[B](f: A => B): MySet[B] = politelyFail

  override def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail

  override def filter(predicate: A => Boolean): MySet[A] =
    new PropertyBasedSet[A](x => property(x) && predicate(x))

  override def foreach[B](f: A => Unit): Unit = politelyFail

  @targetName("Remove")
  override def -(elem: A): MySet[A] =
    filter(x => x != elem)

  @targetName("Difference")
  override def --(anotherSet: MySet[A]): MySet[A] =
    filter(!anotherSet)

  @targetName("Intersection")
  override def &(anotherSet: MySet[A]): MySet[A] =
    filter(anotherSet)

  @targetName("Not")
  override def unary_! : MySet[A] =
    new PropertyBasedSet[A](x => !property(x))

  def politelyFail = throw new IllegalArgumentException("Really deep rabbit hole")
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  def contains(elem: A): Boolean =
    (head == elem) || tail.contains(elem)

  @targetName("Add")
  def +(elem: A): MySet[A] =
    if (this.contains(elem)) this
    else new NonEmptySet[A](elem, this)

  @targetName("Concatenate")
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

  @targetName("Remove")
  def -(elem: A): MySet[A] =
    if (head == elem) tail
    else tail - elem + head

  @targetName("Difference")
  def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet.contains(_))

  @targetName("Intersection")
  def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet) // intersection == filter

  //  new operator
  @targetName("Not")
  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !this.contains(x))
}

object MySetPlayground extends App {
  val s = MySet(1, 2, 3, 4)
  println("Testing MySet")
  s.foreach(println)
  println("Testing MySet")
  (s + 5).foreach(println)
  println("Testing MySet")
  (s + 5 ++ MySet(-1, -2) + 3).foreach(println)

  //  Negated test
  val negative = !s
  println(negative(2))
  println(negative(5))

  val negativeEven = negative.filter(_ % 2 == 0)
  println(negativeEven(5))

  val negativeEven5 = negativeEven + 5
  println(negativeEven5(5))

}
