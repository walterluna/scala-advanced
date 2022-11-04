package lectures.Pt3.Concurrency

object JvmConcurrencyProblems extends App {

  def runInParallel(): Unit = {
    var x = 0

    val thread1 = new Thread(() => {
      x = 1
    })
    val thread2 = new Thread(() => {
      x = 2
    })

    thread1.start()
    thread2.start()
    println(x)
  }

  case class BankAccount(var amount: Int)


  def buy(account: BankAccount, thing: String, price: Int) = {
    account.amount -= price
  }

  def buySafe(account: BankAccount, thing: String, price: Int) =
    account.synchronized {
      account.amount -= price
    }

  def demoBankingProblem(): Unit = {
    (1 to 10000).foreach(_ => {
      val account = new BankAccount(50000)
      val t1 = new Thread(() => buy(account, "shoes", 3000))
      val t2 = new Thread(() => buy(account, "iHpne", 4000))
      t1.start()
      t2.start()
      t1.join()
      t2.join()
      if (account.amount != 43000) println(s"I've broken the bank: ${account.amount}")
    })
  }

  demoBankingProblem()

  /* Exercises
   1) create "inception threads"
    t1
      -> t2
        -> t3
          -> t4
  each thread prints "hello from thread $i"
  print all messages in reverse order

  2) What's the mix/max value of x

  3) "sleep fallacy"
   */

  //  1) create "inception threads"
  def inceptionThreads(maxThreads: Int, i: Int = 1): Thread = {
    new Thread(() => {
      if (i < maxThreads) {
        val childThread = inceptionThreads(maxThreads, i + 1)
        childThread.start()
        childThread.join()
      }
      println(s"Hello from thread $i")
    })
  }

  inceptionThreads(50).start()

  def minMax(): Unit = {
    var x = 0
    val threads = (1 to 100).map(_ => new Thread(() => x += 1))
    threads.foreach(_.start())
  }
  //  min value is 1
  // max value is 100

  def demoSleepFallacy(): Unit = {
    var message = ""
    val awesomeThread = new Thread(() => {
      Thread.sleep(1000)
      message = "scala is awesome"
    })

    message = "scala sucks"
    awesomeThread.start()
    awesomeThread.start()
    Thread.sleep(1001)
    //  solution: join the worker thread
    println(message)
  }
  //  Almost always "Scala is awesome
  //  not guaranteed
  //  On some JVMs, processors, OS, Thread.sleep yields execution
}
