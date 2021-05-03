package lectures.part3concurrency

import lectures.part3concurrency.Intro.pool

import java.util.concurrent.Executors

object Intro extends App {
  // JVM threads
  val runnable = new Runnable {
    override def run(): Unit = println("Running in parallel")
  }
  val aThread  = new Thread(runnable)

  aThread.start() // gives the signal to the JVM to start a JVM thread
  runnable.run() // does not do anything in parallel
  aThread.join() // blocks until aTread finishes running

  val threadHello = new Thread(() => (1 to 5).foreach(_ => println("hello")))
  val threadGoodbye = new Thread(() => (1 to 5).foreach(_ => println("goodbye")))
  threadHello.start()
  threadGoodbye.start()
  // different runs produce different results


  // executors
  val pool = Executors.newFixedThreadPool(10)
  pool.execute(() => println("something in the thread pool"))

  pool.execute(() => {
    Thread.sleep(1000)
    println("done after 1 second")
  })
  pool.execute(() => {
    Thread.sleep(1000)
    println("almost done")
    Thread.sleep(2000)
    println("done after 2 seconds")
  })

  pool.shutdown()
//  pool.execute(() => println("should not appear"))
//    pool.shutdownNow()
  println(pool.isShutdown)

  def runInParalell = {
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
//  for (_ <- 1 to 10000) runInParalell

  class BankAccount(var amount: Int) {
    override def toString: String = "" + amount
  }

  def buy(account: BankAccount, thing: String, price: Int) = {
    account.amount -= price
//    println("I've bought " + thing)
//    println("my account is now " + account)
  }

  for (_ <- 1 to 1000) {
    val account = new BankAccount(50000)
    val thread1 = new Thread(() => {
      buy(account, "shoes", 3000)
    })
    val thread2 = new Thread(() => {
      buy(account, "iPhone12", 4000)
    })
    thread1.start()
    thread2.start()
    Thread.sleep(10)
    if (account.amount != 43000) println("AHA: " + account.amount)
    println()
  }

  def buySafe(account: BankAccount, thing: String, price: Int) = {
    account.synchronized({
      // no two threads can evaluate this at same time
      account.amount -= price
      println("I've bought " + thing)
      println("my account is now " + account)
    })
  }

  def inceptionThreads(maxThreads: Int, i: Int = 1): Thread = new Thread(() => {
    if (i < maxThreads) {
      val newThread = inceptionThreads(maxThreads, i + 1)
      newThread.start()
      newThread.join()
    }
    println(s"Hello from thread $i")
  })

  inceptionThreads(50).start()
}

