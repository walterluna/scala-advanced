package lectures.Pt3.Concurrency

import scala.concurrent.Future
import scala.util.{Failure, Random, Success}

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
  
}
