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

}
