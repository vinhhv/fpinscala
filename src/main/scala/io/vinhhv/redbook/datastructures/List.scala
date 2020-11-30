package io.vinhhv.redbook.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil                             extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil         => 0           // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def tail[A](l: List[A]): List[A] = l match {
    case Nil         => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil         => Nil
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n > 0) {
      l match {
        case Nil         => Nil
        case Cons(_, xs) => drop(xs, n - 1)
      }
    } else l

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil         => Nil
    case Cons(x, xs) =>
      if (f(x)) dropWhile(xs, f)
      else l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil          => Nil
    case Cons(_, Nil) => Nil
    case Cons(x, xs)  => Cons(x, init(xs))
  }

  // Exercise 3.7
  // No, not possible to short-circuit using foldRight; all arguments are resolved before calculation.

  // Exercise 3.8
  // We get back the same list. It shows that the foldRight and data constructor for list is the same if given
  // the proper arguments

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil         => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length2[A](l: List[A]): Int =
    foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  def foldRightByFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def appendFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    reverse(foldLeft(reverse(a1), a2)((acc, a) => Cons(a, acc)))

  def appendFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((a, acc) => Cons(a, acc))

  def appendAll[A](as: List[List[A]]): List[A] =
    foldRight(as, List[A]())(append)

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil: List[Int])((h, t) => Cons(h + 1, t))

  def allToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def filterOutOdd(as: List[Int]): List[Int]                =
    filter(as)(_ % 2 == 0)

  def flatMap1[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((h, t) => append(f(h), t))

  def flatMap2[A, B](as: List[A])(f: A => List[B]): List[B] =
    appendAll(map(as)(f))

  def filterByFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap2(as)(h => if (f(h)) List(h) else Nil)

  def zipAdd(l: List[Int], r: List[Int]): List[Int] = (l, r) match {
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipAdd(t1, t2))
  }

  def zipWith[A, B](l: List[A], r: List[A], f: (A, A) => B): List[B] = (l, r) match {
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2, f))
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    (sup, sub) match {
      case (Nil, Nil)                   => true
      case (_, Nil)                     => true
      case (Nil, _)                     => false
      case (Cons(h1, t1), Cons(h2, t2)) =>
        if (h1 == h2) hasSubsequence(t1, t2)
        else hasSubsequence(t1, sub)
    }
}
