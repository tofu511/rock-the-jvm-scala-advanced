package lectures.part3concurrency

import java.util.concurrent.ForkJoinPool
import java.util.concurrent.atomic.AtomicReference
import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.parallel.immutable.ParVector

object ParallelUtil extends App {

  val parList = List(1,2,3).par
  val aParVector = ParVector[Int](1,2,3)

  def measure[T](operation: => T): Long = {
    val time = System.currentTimeMillis()
    operation
    System.currentTimeMillis() - time
  }

  val list = (1 to 10000).toList
  val serialTime = measure {
    list.map(_ + 1)
  }
  println(serialTime)
  val parallelTime = measure {
    list.par.map(_ + 1)
  }
  println(parallelTime)

  // fold, reduce with non-associative operators
  println(List(1,2,3).reduce(_ - _))
  println(List(1,2,3).par.reduce(_ - _))

  // synchronization
  var sum = 0
  List(1,2,3).par.foreach(sum += _)
  println(sum) // race conditions!

  // configuring
  aParVector.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(2))

  // atomic ops and references
  val atomic = new AtomicReference[Int](2)
  val currentValue = atomic.get() // thread-safe read
  atomic.set(4) // thread-safe write

  atomic.getAndSet(5) // thread-safe combo
  atomic.compareAndSet(38, 50)

  atomic.updateAndGet(_ + 1) // thread-safe function run
  atomic.getAndUpdate(_ + 1)

  atomic.accumulateAndGet(12, _ + _) // thread-safe accumulation
}
