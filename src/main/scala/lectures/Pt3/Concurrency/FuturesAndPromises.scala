package lectures.Pt3.Concurrency

import scala.concurrent.Future
import scala.util.{Failure, Success}

//important for futures
import scala.concurrent.ExecutionContext.Implicits.global

object FuturesAndPromises extends App {
  //  functional way to calculate something in parallel or on another thread

  def calculateMeaningOfLife: Int = {
    Thread.sleep(2000)
    42
  }

  val aFuture = Future {
    calculateMeaningOfLife // calculates in another thread
  } // (global) which is passed by the compiler

  println(aFuture.value)

  println("Waiting on the future")
  aFuture.onComplete {
    case Success(v) => println(s"The meaning of life is $v")
    case Failure(e) => println(" I have failed with")
  }


  Thread.sleep(3000)
}
