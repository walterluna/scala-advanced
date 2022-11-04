package lectures.Pt3.Concurrency

object ThreadCommunication extends App {

  /* Producer-Consumer problem
  producer -> [ ? ] -> consumer
  producer and consumer run in parallel, consumer should wait for producer
   */
  class SimpleContainer {
    private var value: Int = 0

    def isEmpty: Boolean = value == 0

    def set(newValue: Int): Unit =
      value = newValue

    def get: Int = {
      val result = value
      value = 0
      result
    }
  }

  def naiveProdCons() = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting...")
      while (container.isEmpty) { // Busy waiting
        println("[consumer] actively waiting")
      }

      println("[consumer] I have consumed" + container.get)
    })

    val producer = new Thread(() => {
      println("[producer] cmputing...")
      Thread.sleep(500)
      val value = 42
      println("[producer] I have produced the value: " + value)
      container.set(value)
    })

    consumer.start()
    producer.start()
  }


  //  wait and notify
  def smartProdCons(): Unit = {
    val container = new SimpleContainer

    val consumer = new Thread(() => {
      println("[consumer] waiting")
      container.synchronized {
        container.wait()
      }

      //  container must have some value
      println("[consumer] I have consumed: " + container.get)
    })

    val producer = new Thread(() => {
      println("[producer] hard at work...")
      Thread.sleep(2000)
      val value = 42

      container.synchronized {
        println("[producer] I'm producing " + value)
        container.set(value)
        container.notify()
      }
    })

    consumer.start()
    producer.start()
  }

  //  naiveProdCons()
  smartProdCons()
}
