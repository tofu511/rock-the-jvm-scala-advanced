package lectures.part2afp

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {
  override def apply(v1: A): Boolean = contains(v1)
  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  // Exercise
  // removing an element
  def -(elem: A): MySet[A]
  // intersection with another set
  def &(anotherSet: MySet[A]): MySet[A]
  // difference with another set
  def --(anotherSet: MySet[A]): MySet[A]
  def unary_! : MySet[A]
}

class EmptySet[A] extends MySet[A] {
  def contains(elem: A): Boolean = false
  def +(elem: A): MySet[A] = new NonEmptySet[A](elem, this)
  def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  def map[B](f: A => B): MySet[B] = new EmptySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]
  def filter(predicate: A => Boolean): MySet[A] = this
  def foreach(f: A => Unit): Unit = ()
  def -(elem: A): MySet[A] = this
  def &(anotherSet: MySet[A]): MySet[A] = this
  def --(anotherSet: MySet[A]): MySet[A] = this
  def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)
}
//class AllInclusiveSet[A] extends MySet[A] {
//  override def contains(elem: A): Boolean = true
//  override def +(elem: A): MySet[A] = this
//  override def ++(anotherSet: MySet[A]): MySet[A] = this
//  override def map[B](f: A => B): MySet[B] = ???
//  override def flatMap[B](f: A => MySet[B]): MySet[B] = ???
//  override def filter(predicate: A => Boolean): MySet[A] = ???
//  override def foreach(f: A => Unit): Unit = ???
//  override def -(elem: A): MySet[A] = ???
//  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)
//  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)
//  override def unary_! : MySet[A] = new EmptySet[A]
//}
// all elements of type A which satisfy a property
// { x in A | property(x) }
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
  override def contains(elem: A): Boolean = property(elem)
  // { x in A | property(x) } + element = { x in A | property(x) || x == element }
  override def +(elem: A): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || x == elem)
  override def ++(anotherSet: MySet[A]): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || anotherSet(x))

  override def map[B](f: A => B): MySet[B] = politelyFail
  override def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail
  override def filter(predicate: A => Boolean): MySet[A] = new PropertyBasedSet[A](x => property(x) && predicate(x))
  override def foreach(f: A => Unit): Unit = politelyFail
  override def -(elem: A): MySet[A] = filter(x => x != elem)
  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)
  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)
  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  def politelyFail = throw new IllegalArgumentException("Really deep rabbit hole!")
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  def contains(elem: A): Boolean = head == elem || tail.contains(elem)
  def +(elem: A): MySet[A] =
    if (this.contains(elem)) this
    else new NonEmptySet[A](elem, this)
  def ++(anotherSet: MySet[A]): MySet[A] =
    tail ++ anotherSet + head

  def map[B](f: A => B): MySet[B] = tail.map(f) + f(head)
  def flatMap[B](f: A => MySet[B]): MySet[B] = tail.flatMap(f) ++ f(head)
  def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail.filter(predicate)
    if (predicate(head)) filteredTail + head
    else filteredTail
  }
  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }

  def -(elem: A): MySet[A] = {
    if (this.contains(elem)) this.filter(_ != elem)
    else this
//     if (head == elem) tail
//     else tail - elem + head
  }

  def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !this.contains(x))
}

object MySet {
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)

    buildSet(values.toSeq, new EmptySet[A])
  }
}

object MySetPlayground extends App {
  val s = MySet(1,2,3,4)
//  (s + 5 ++ MySet(6,7,8)).flatMap(elem => MySet(elem * 2)).filter(_ % 2 == 0).foreach(println)
  val negative = !s
  println(negative(2))
  println(negative(5))

  val negativeEven = negative.filter(_ % 2 == 0)
  println(negativeEven(5))

  val negativeEven5 = negativeEven + 5
  println(negativeEven5(5))

  println((s - 1)(1))
  println((s - 1)(2))
}
