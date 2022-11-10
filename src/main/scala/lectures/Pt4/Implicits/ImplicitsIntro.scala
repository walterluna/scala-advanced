package lectures.Pt4.Implicits


object ImplicitsIntro extends App {

  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name"
  }

  implicit def stringToPerson(name: String): Person = Person(name);

  println("Peter".greet)
  //  Method greet does not exist on String
  //  Compiler will look for an implicit value, class or method that can turn this string
  //  into something that has a greet method
  //  If the compiler finds more than one suitable match, code will not compile

  def increment(a: Int)(implicit b: Int) = a + b

  implicit val defaultB: Int = 42

  println(increment(5))
  //  Second param is not necessary as the compiler will find the implicit value
  //  If required, we can pass another value for the second param and ignore the implicit one


}
