package lectures.Pt3.Concurrency

import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Random, Success, Try}
import scala.concurrent.duration.*

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

  //  Mini social network

  case class Profile(id: String, name: String) {
    def poke(anotherProfile: Profile): Unit =
      println(s"${this.name} poking ${anotherProfile.name}")
  }

  object SocialNetwork {
    //  "database"
    val names = Map(
      "fb.id.1-zuck" -> "Mark",
      "fb.id.2-bill" -> "Billd",
      "fb.id.0-dummy" -> "dummy",
    )

    val friends = Map(
      "fb.id.1-zuck" -> "fb.id.2-bill"
    )

    val random = new Random()

    //  API
    def fetchProfile(id: String): Future[Profile] = Future {
      Thread.sleep(random.nextInt(300))
      Profile(id, names(id))
    }

    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
      Thread.sleep(random.nextInt(400))
      val bfId = friends(profile.id)
      Profile(bfId, names(bfId))
    }
  }

  val mark = SocialNetwork.fetchProfile("fb.id.1-zuck")
  //  mark.onComplete {
  //    case Success(markProfile) => {
  //      val bill = SocialNetwork.fetchBestFriend(markProfile)
  //      bill.onComplete {
  //        case Success(billProfile) => markProfile.poke(billProfile)
  //        case Failure(e) => e.printStackTrace()
  //      }
  //    }
  //    case Failure(e) => e.printStackTrace()
  //  }

  //  functional composition of futures
  //  map, flatmap, filter

  val nameOnTheWall = mark.map(_.name)
  val marksBestFriend = mark.flatMap(SocialNetwork.fetchBestFriend)
  val zucksBestFriend = marksBestFriend.filter(_.name.startsWith("Z"))

  //  for-comprehensions (recommended)
  for {
    mark <- SocialNetwork.fetchProfile("fb.id.1-zuck")
    bill <- SocialNetwork.fetchBestFriend(mark)
  } mark.poke(bill)

  Thread.sleep(1000)

  //  fallbacks
  val aProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id ").recover {
    case e: Throwable => Profile("fb.id.0-dummy", "dummy name")
  }

  val aFetchedProfileNoMatterWhat = SocialNetwork.fetchProfile("unknown id ").recoverWith {
    case e: Throwable => SocialNetwork.fetchProfile("fb.id.5-real-id")
  }

  val fallbackResult = SocialNetwork.fetchProfile("unknown id ")
    .fallbackTo(SocialNetwork.fetchProfile("fb.id.5-real-id"))

  //  online banking app
  case class User(name: String)

  case class Transaction(sender: String, receiver: String, amount: Double, status: String)

  object BankingApp {
    val name = "Rock the jvm"

    def fetchUser(name: String): Future[User] = Future {
      //      simulate fetching from the db
      Thread.sleep(500)
      User(name)
    }

    def createTransaction(user: User, merchantName: String, amount: Double): Future[Transaction] = Future {
      Thread.sleep(500)
      Transaction(user.name, merchantName, amount, "Success")
    }

    def purchase(username: String, item: String, merchantName: String, cost: Double): String = {
      // fetch the user from the DB
      // create a transaction
      // WAIT for the transaction to finish
      val transactionStatusFuture = for {
        user <- fetchUser(username)
        transaction <- createTransaction(user, merchantName, cost)
      } yield transaction.status

      Await.result(transactionStatusFuture, 2.seconds) // implicit conversions -> pimp my library
    }
  }

  println(BankingApp.purchase("Daniel", "iPhone 12", "rock the jvm store", 3000))

  // promises

  val promise = Promise[Int]() // "controller" over a future
  val future = promise.future

  // thread 1 - "consumer"
  future.onComplete {
    case Success(r) => println("[consumer] I've received " + r)
  }

  // thread 2 - "producer"
  val producer = new Thread(() => {
    println("[producer] crunching numbers...")
    Thread.sleep(500)
    // "fulfilling" the promise
    promise.success(42)
    println("[producer] done")
  })

  producer.start()
  Thread.sleep(1000)

  /* EXERCISES
    1) fulfill a future IMMEDIATELY with a value
    2) inSequence(fa, fb)
    3) first(fa, fb) => new future with the first value of the two futures
    4) last(fa, fb) => new future with the last value
    5) retryUntil[T](action: () => Future[T], condition: T => Boolean): Future[T]
   */

  //  1)
  def fulfillImmediately[T](value: T): Future[T] = Future(value)

  // 2)
  def inSequence[A, B](first: Future[A], second: Future[B]) =
    first.flatMap(_ => second)

  // 3)
  def first[A](fa: Future[A], fb: Future[A]): Future[A] = {
    val prom = Promise[A]
    fa.onComplete(prom.tryComplete)
    fb.onComplete(prom.tryComplete)

    prom.future
  }

  def last[A](fa: Future[A], fb: Future[A]): Future[A] = {
    //  1st promise which both futures will try to complete
    //  2nd promise which will be completed by last future
    val bothPromise = Promise[A]
    val lastPromise = Promise[A]

    def checkAndComplete = (result: Try[A]) => {
      if (bothPromise.tryComplete(result))
        lastPromise.tryComplete(result)
    }

    fa.onComplete(checkAndComplete)
    fb.onComplete(checkAndComplete)

    lastPromise.future
  }

  println("Testing exercise 2 and 3")
  val fast = Future {
    Thread.sleep(100)
    42
  }
  val slow = Future {
    Thread.sleep(200)
    24
  }
  first(fast, slow).foreach(x => println(s"First: $x"))
  last(fast, slow).foreach(x => println(s"Last: $x"))
  Thread.sleep(1000)

  // retry until
  def retryUntil[A](action: () => Future[A], condition: A => Boolean): Future[A] =
    action()
      .filter(condition)
      .recoverWith {
        case _ => retryUntil(action, condition)
      }

  val random = new Random()
  val action = () => Future {
    Thread.sleep(100)
    val nextValue = random.nextInt(100)
    println("generated " + nextValue)
    nextValue
  }

  retryUntil(action, (x: Int) => x < 10)
    .foreach(result => println("settled at " + result))
  Thread.sleep(10000)
}
