package lectures.Pt3.Concurrency

import java.util.concurrent.Executors

object Intro extends App {
  //  JVM threads
  //  threads are instances of a class
  //  Java has an interface Runnable with a public void run() methods
  val aRunnable = new Runnable {
    override def run(): Unit = println("Running in parallel")
  }
  val aThread = new Thread(aRunnable)

  aThread.start() //gives signal to the JVM to start a JVM thread
  //  create a JVM thread which runs on a OS thread
  aRunnable.run() // Code is NOT run on parallel
  //  thread instance !== jvm thread

  aThread.join() //blocks until aThread finished running

  val threadHello = new Thread(() => (1 to 5).foreach(_ => println("hello")))
  val threadGoodbye = new Thread(() => (1 to 5).foreach(_ => println("goodbye")))
  threadHello.start()
  threadGoodbye.start()
  //  different runs produce different results

  //  executors
  //  threads are expensive to restart and kill
  val pool = Executors.newFixedThreadPool(10)

  pool.execute(() => println("Something in the thread pool"))
  pool.execute(() => {
    Thread.sleep((1000))
    println("done after 1 second")
  })
  pool.execute(() => {
    Thread.sleep((1000))
    println("almost done ")
    Thread.sleep((1000))
    println("done after 2 seconds")
  })

  pool.shutdown()
  //  pool.execute(() => println("should now appear")) //throws an exception in main thread
  //  pool.shutdownNow() //interrups all running threads
  println(pool.isShutdown)
}
