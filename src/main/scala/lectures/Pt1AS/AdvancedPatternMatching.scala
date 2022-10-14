package lectures.Pt1AS

object AdvancedPatternMatching extends App {

  val numbers = List(1)

  val description = numbers match
    case head :: Nil => println(s"The only element is $head")
    case _ =>

  /*
    -constants
    -wildcards
    -case classes
    -tuples
    -some special magic like above
  */

  class Person(val name: String, val age: Int)
  //  make class compatible with PM without making it a case class

  object Person {
    def unapply(person: Person): Option[(String, Int)] = Some((person.name, person.age))

    //    unapply can be overloaded
    def unapply(age: Int): Option[String] =
      Some(if (age < 21) "minor" else "major")
  }

  val bob = new Person("Bob", 25)
  val greeting = bob match
    //  This pattern is related to the object, not the class
    case Person(n, a) => s"Hi, my name is $n and I'm $a years old"

  val legalStatus = bob.age match
    case Person(status) => s"My legal status is $status"
    case _ => "I don't know"

  println(greeting)
  println(legalStatus)

  /* EXERCISES
  1. pattern match for ints using conditions instead of values
  */

  // My solution
  object GetProperty {
    def unapply(arg: Int): Option[String] =
      if (arg < 10) Some("single digit")
      else if (arg % 2 == 0) Some("An even number")
      else None
  }
  //  Daniel's solution involved creating objects for each property with their own unapply(): Boolean
  //  then match n to each object

  val n = 2
  val mathProperty = n match
    case GetProperty(property) => property
    case _ => "Unknown"

  println(mathProperty)

  //  Infix patterns
  //  In case head :: Nil, :: is an infix pattern
  case class Or[A, B](a: A, b: B) // built in as either

  val either = Or(2, "two")

  val humanDescription = either match
    // case Or(number, string) => s"$number is written as $string"
    case number Or string => s"$number is written as $string" //equivalent

  println(humanDescription)

  //  decomposing sequences
  val vararg = numbers match {
    case List(1, _*) => "starting with 1" // _* is a vararg pattern
  }
  println(vararg)

  abstract class MyList[+A] {
    def head: A = ???

    def tail: MyList[A] = ???
  }

  case object Empty extends MyList[Nothing]

  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object MyListPatterns {
    def unapplySeq[A](list: MyList[A]): Option[Seq[A]] =
      if (list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
  }

  val myList: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val decomposed = myList match
    case MyListPatterns(1, 2, _*) => "Starts with 1, 2" // pattern is run on Option[Seq[A]] returned by unapplySeq
    case _ => "something else"
  println(decomposed)

  //  custom return types for unapply
  //  return type should have
  //  -isEmpty: Boolean
  //  -get: T
  abstract class Wrapper[T] {
    def isEmpty: Boolean

    def get: T
  }

  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      override def isEmpty: Boolean = false

      override def get: String = person.name
    }
  }

  println(bob match
    case PersonWrapper(n) => s"This person's name is $n"
    case _ => "An alien"
  )
}
