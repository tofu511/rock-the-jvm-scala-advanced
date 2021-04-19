package lectures.part2afp

import scala.annotation.tailrec

object Monads extends App {

  trait Attempt[+A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }
  object Attempt {
    def apply[A](a: => A): Attempt[A] =
      try {
        Success(a)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Success[+A](value: A) extends Attempt[A] {
    override def flatMap[B](f: A => Attempt[B]): Attempt[B] =
      try {
        f(value)
      } catch {
        case e: Throwable => Fail(e)
      }
  }
  case class Fail(e: Throwable) extends Attempt[Nothing] {
    override def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }

  /*
    left-identity
    unit.flatMap(f) = f(x)
    Success(x).flatMap(f) = f(x)

    right-identity
    attempt.flatMap(unit) = attempt
    Success(x).flatMap(x => Attempt(x)) = Attempt(x) = Success(x)
    Fail(_).flatMap(...) = Fail(e)

    associativity
    attempt.flatMap(f).flatMap(g) == attempt.flatMap(x => f(x).flatMap(g))
    Fail(e).flatMap(f).flatMap(g) == Fail(e)
    Fail(e).flatMap(x => f(x).flatMap(g)) = Fail(e)

    Success(v).flatMap(f).flatMap(g) =
      f(v).flatMap(g) OR Fail(e)

    Success(v).flatMap(x => f(x).flatMap(g)) =
      f(v).flatMap(g) OR Fail(e)
   */

  val fail = Attempt {
    throw new RuntimeException("my monad!")
  }
  println(fail)

  /*
    Exercise:
      1. implement a Lazy[T] monad = computation which will only be executed when it's needed
      2. Monad[T] with map & flatten
   */

  class Lazy[+A](value: => A) {
    // call by need
    private lazy val internalValue = value
    def use: A = internalValue
    // function call by need
    def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(internalValue)
  }
  object Lazy {
    def apply[A](value: => A): Lazy[A] = new Lazy(value)
  }

  val lazyInstance = Lazy {
    println("I'm Lazy")
    42
  }
  val flatMappedLazy = lazyInstance.flatMap(x => Lazy {
    10 * x
  })
  val flatMappedLazy2 = lazyInstance.flatMap(x => Lazy {
    10 * x
  })
  flatMappedLazy.use
  flatMappedLazy2.use

  trait Monad[+A] {
    def flatMap[B](f: A => Monad[B]): Monad[B]
    def map[B](f: A => B): Monad[B]
    def flatten[A](m: Monad[Monad[A]]): Monad[A]
    def ::[B >: A](element: B): Monad[B]
    def ++[B >: A](anotherMonad: Monad[B]): Monad[B]
  }
  object Monad {
    def apply[A](a: A*): Monad[A] = {
      @tailrec
      def build(seq: Seq[A], acc: Monad[A]): Monad[A] =
        if (seq.isEmpty) acc
        else build(seq.tail, seq.head :: acc)

      build(a.toSeq.reverse, new EmptyList)
    }
  }

  class EmptyList extends Monad[Nothing] {
    def flatMap[B](f: Nothing => Monad[B]): Monad[B] = this
    def map[B](f: Nothing => B): Monad[B] = this
    def flatten[Nothing](m: Monad[Monad[Nothing]]): Monad[Nothing] = this
    def ::[B >: Nothing](element: B): Monad[B] = NonEmptyList(element, this)
    def ++[B >: Nothing](anotherMonad: Monad[B]): Monad[B] = anotherMonad
  }

  case class NonEmptyList[+A](head: A, tail: Monad[A]) extends Monad[A] {
    def flatMap[B](f: A => Monad[B]): Monad[B] = f(head) ++ tail.flatMap(f)
    def map[B](f: A => B): Monad[B] = f(head) :: tail.map(f)
    def flatten[A](m: Monad[Monad[A]]): Monad[A] = m.flatMap(identity)
    def ::[B >: A](element: B): Monad[B] = NonEmptyList(element, this)
    def ++[B >: A](anotherMonad: Monad[B]): Monad[B] = NonEmptyList(head, tail ++ anotherMonad)
  }

  val myList = Monad.apply(1,2,3)
  val mm = Monad.apply(Monad.apply(1,2,3))
  println(mm.flatten(mm))
}
