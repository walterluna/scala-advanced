package lectures.Pt2AFP

object Monads extends App {
  // Implement our own Try monad
  trait Attempt[+A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }

  object Attempt {
    def apply[A](a: => A): Attempt[A] =
      try {
        Success(a)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Success[A](value: A) extends Attempt[A] {
    override def flatMap[B](f: A => Attempt[B]): Attempt[B] =
      try {
        f(value)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Fail(e: Throwable) extends Attempt[Nothing] {
    override def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }

  val attempt = Attempt {
    throw new RuntimeException("My failed monad")
  }

  println(attempt)

  /* Exercise:
  1) Implement a Lazy[T] monad
  computation will only be executed when it's needed
  unit/apply
  flatMap

  2) Monads = unit + flatMap
     Monads = unit + map + flatten

    Monad[T] {
      def flatMap[B](f: T => B): Monad[B] = ...(implemented)

      def map[B](f: T => B): Monad[B] = ???
      def flatten(m: Monad[Monad[T]]): Monad[T] = ???
   }
   */

}
