package lectures.Pt2AFP

object PartialFunctions extends App {
  val aFunction = (x: Int) => x + 1 // Int => Int

  val aFussyFunction = (x: Int) =>
    if (x == 1) 42
    else if (x == 2) 56
    else if (x == 5) 999
    else throw new RuntimeException("Function not applicable")

  val aNicerFussyFunction = (x: Int) => x match
    case 1 => 42
    case 2 => 56
    case 5 => 999
  //  {1,2,5} => Int (PartialFunction)

  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  } // equal to aNicerFussyFunction

  println(aPartialFunction(2))
  //  println(aPartialFunction(5342)) results in MatchError

  //  PF utilities
  println(aPartialFunction isDefinedAt 67)

  //  can be lifted to total functions
  val lifted = aPartialFunction.lift // Int => Option(Int)
  println(lifted(2))
  println(lifted(98))

  //  OrElse
  val pfChain = aPartialFunction.orElse[Int, Int] {
    case 45 => 67
  }

  println(pfChain(2))
  println(pfChain(45))

  //  Pf extend normal (total) functions
  val aTotalFunction: Int => Int = {
    case 1 => 99
  }

  //  HOFs accept partial functions as well
  val aMappedList = List(1, 2, 3).map {
    case 1 => 42
    case 2 => 78
    case 3 => 1000
  }
  println(aMappedList)

  /* Note: PF can only have ONE parameter type  */

  /* Exercises
  1. Construct a PF instance by using the PF trait
  2. dumb hat-bot as a PF
   */

  val exPartialFunction = new PartialFunction[Int, Int] {
    override def apply(v1: Int): Int =
      v1 match
        case 1 => 42
        case 2 => 78
        case 5 => 1000

    override def isDefinedAt(x: Int): Boolean =
      x == 1 || x == 2 || x == 5
  }

  val getBotResponse: PartialFunction[String, Unit] = {
    case "Hello" => println("Hi! I'm dumbot")
    case "how are you" => println("Great and you?")
    case "Goodbye" => println("Ok, see you latter")
  }

  scala.io.Source.stdin.getLines().foreach(getBotResponse(_))


}
