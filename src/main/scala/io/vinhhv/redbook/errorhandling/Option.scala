package fpinscala.errorhandling

import scala.{
  Option => _, Some => _, Either => _, _
} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] { self =>
  def map[B](f: A => B): Option[B] =
    self match {
      case Some(value) => Some(f(value))
      case None        => None
    }

  def getOrElse[B >: A](default: => B): B =
    self match {
      case Some(value) => value
      case None        => default
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    self.map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    self.map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    self.flatMap(a => if (f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int =
      throw new Exception(
        "fail!"
      ) // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    } catch {
      case e: Exception => 43
    }   // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int               =
    try {
      val x = 42 + 5
      x + ((throw new Exception(
        "fail!"
      )): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    } catch { case e: Exception => 43 }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (None, _)          => None
      case (_, None)          => None
      case (Some(a), Some(b)) => Some(f(a, b))
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil): Option[List[A]])((op, acc) => map2(op, acc)(_ :: _))

  def traverse1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sequence(a map f)
  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]]) { (a, acc) =>
      f(a) match {
        case None        => None
        case Some(value) => acc.flatMap(bs => Some(value :: bs))
      }
    }
  def traverse3[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight(Some(Nil): Option[List[B]])((h, t) => map2(f(h), t)(_ :: _))
}
