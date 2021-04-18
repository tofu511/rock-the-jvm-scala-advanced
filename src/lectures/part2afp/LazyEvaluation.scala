package lectures.part2afp

import scala.annotation.tailrec

object LazyEvaluation extends App {

  lazy val x: Int = {
    println("hello")
    42
  }
  println(x)
  println(x)

  def sideEffectCondition: Boolean = {
    println("boo")
    true
  }
  def simpleCondition: Boolean = false

  lazy val lazyCondition = sideEffectCondition
  println(if(simpleCondition && lazyCondition) "yes" else "no")

  // in conjunction with call by name
  def byNameMethod(n: => Int): Int = {
    lazy val t = n // only evaluated once
    t + t + t + 1
  }
  def retrieveMagicValue = {
    println("waiting")
    Thread.sleep(1000)
    42
  }
  println(byNameMethod(retrieveMagicValue))

  // filtering with lazy vals
  def lessThan30(i: Int): Boolean = {
    println(s"$i is less than 30?")
    i < 30
  }

  def greaterThan20(i: Int): Boolean = {
    println(s"$i is greater than 20?")
    i > 20
  }

  val numbers = List(1, 25, 40, 5, 23)
  val lt30 = numbers.filter(lessThan30)
  val gt20 = lt30.filter(greaterThan20)
  println(gt20)

  val lt30lazy = numbers.withFilter(lessThan30)
  val gt20lazy = lt30lazy.withFilter(greaterThan20)
  println
  gt20lazy.foreach(println)

  abstract class MyStream[+A] {
    def isEmpty: Boolean
    def head: A
    def tail: MyStream[A]

    def #::[B >: A](element: B): MyStream[B]
    def ++[B >: A](anotherStream: MyStream[B]): MyStream[B]

    def foreach(f: A => Unit): Unit
    def map[B](f: A => B): MyStream[B]
    def flatMap[B](f: A => MyStream[B]): MyStream[B]
    def filter(predicate: A => Boolean): MyStream[A]

    def take(n: Int): MyStream[A]
    def takeAsList(n: Int): List[A] = take(n).toList()

    @tailrec
    final def toList[B >: A](acc: List[B] = Nil): List[B] =
      if (isEmpty) acc.reverse
      else tail.toList(head :: acc)
  }

  object MyStream {
    def from[A](start: A)(generator: A => A): MyStream[A] = new Cons[A](start, MyStream.from(generator(start))(generator))
  }

  object EmptyStream extends MyStream[Nothing] {
    def isEmpty: Boolean = true
    def head: Nothing = throw new NoSuchElementException
    def tail: MyStream[Nothing] = throw new NoSuchElementException

    def #::[B >: Nothing](element: B): MyStream[B] = new Cons[B](element, this)
    def ++[B >: Nothing](anotherStream: MyStream[B]): MyStream[B] = anotherStream

    def foreach(f: Nothing => Unit): Unit  = ()
    def map[B](f: Nothing => B): MyStream[B]  = this
    def flatMap[B](f: Nothing => MyStream[B]): MyStream[B]  = this
    def filter(predicate: Nothing => Boolean): MyStream[Nothing]  = this

    def take(n: Int): MyStream[Nothing]  = this
  }

  // : => にしないとダメ(call by need)
  class Cons[+A](hd: A, tl: => MyStream[A]) extends MyStream[A] {
    def isEmpty: Boolean = false

    // val, lazy valとしてoverride
    override val head: A = hd
    override lazy val tail: MyStream[A] = tl // call by need

    def #::[B >: A](element: B): MyStream[B] = new Cons[B](element, this)
    def ++[B >: A](anotherStream: MyStream[B]): MyStream[B] = new Cons[B](head, tail ++ anotherStream)

    def foreach(f: A => Unit): Unit = {
      f(head)
      tail.foreach(f)
    }
    def map[B](f: A => B): MyStream[B] = new Cons[B](f(head), tail.map(f))
    def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)
    def filter(predicate: A => Boolean): MyStream[A] = {
      if (predicate(head)) new Cons(head, tail.filter(predicate))
      else tail.filter(predicate)
    }

    def take(n: Int): MyStream[A] = {
      if (n <= 0) EmptyStream
      else if (n == 1) new Cons(head, EmptyStream)
      else new Cons(head, tail.take(n - 1))
    }
  }

  println("=========")
  val naturals = MyStream.from(1)(_ + 1)
  println(naturals.head)
  println(naturals.tail.head)
  println(naturals.tail.tail.head)

  val startFrom0 = 0 #:: naturals
  println(startFrom0.head)

  startFrom0.take(10000).foreach(println)

  println(startFrom0.map(_ * 2).take(100).toList())
  println(startFrom0.flatMap(x => new Cons(x, new Cons(x + 1, EmptyStream))).take(10).toList())
}
