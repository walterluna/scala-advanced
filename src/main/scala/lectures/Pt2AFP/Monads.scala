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

  //  1) Lazy monad

  class Lazy[+A](value: => A) {
    //    call by need
    private lazy val internalValue = value

    def use: A = internalValue

    def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] =
      f(internalValue)
  }

  object Lazy {
    def apply[A](value: => A): Lazy[A] = new Lazy[A](value)
  }

  val lazyInstance = Lazy {
    println("Today I don't feel like doing anything")
    42
  }

  //  println(lazyInstance.use)

  val flatMappedInstance = lazyInstance.flatMap(x => Lazy {
    10 + x
  })
  val flatMappedInstance2 = lazyInstance.flatMap(x => Lazy {
    10 + x
  })

  flatMappedInstance.use
  flatMappedInstance2.use

  /*
  // 2) Map and flatten in terms of flatMap
  Monad[T] {
    def flatMap[B](f: T => B): Monad[B] = ...(implemented)

    // unit refers to the apply function (constructor)
    def map[B](f: T => B): Monad[B] = flatMap( x => unit(f(x)))
    def flatten(m: Monad[Monad[T]]): Monad[T] = m.flatMap((x: Monad[T]) => x)
 }
   */

}
