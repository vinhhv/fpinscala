package io.vinhhv.redbook

object Main {
  def main(args: Array[String]): Unit =
    println("Hello red-book!")

  object Chapter2 {
    // Exercise 1
    def fib(n: Int): Int = {
      @annotation.tailrec
      def go(l: Int, r: Int, n: Int): Int =
        if (n > 0) {
          go(r, l + r, n - 1)
        } else l
      go(0, 1, n)
    }

    // Exercise 2
    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
      @annotation.tailrec
      def go(l: Int, r: Int, acc: Boolean): Boolean =
        if (l < as.length && r < as.length) {
          go(l + 1, r + 1, acc && ordered(as(l), as(r)))
        } else {
          acc
        }
      go(0, 1, true)
    }

    // Exercise 3
    def curry[A, B, C](f: (A, B) => C): A => (B => C) = { (a: A) => (b: B) =>
      f(a, b)
    }

    // Exercise 4
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = { (a: A, b: B) =>
      f(a)(b)
    }

    // Exercise 5
    def compose[A, B, C](f: B => C, g: A => B): A => C = { (a: A) =>
      f(g(a))
    }
  }
}
