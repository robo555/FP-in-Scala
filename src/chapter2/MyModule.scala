import scala.annotation.tailrec

/**
 * Created by robo on 15/11/2016.
 */
object MyModule {
  // exercise 2.1
  def fib(n: Int): Int = {
    @tailrec
    def go(step: Int, prev1: Int, prev2: Int): Int = {
      if (step == n) {
        prev1
      } else {
        go(step + 1, prev2, prev1 + prev2)
      }
    }
    go(0, 0, 1)
  }

  // exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length) {
        true
      } else if (ordered(as(n - 1), as(n))) {
        loop(n + 1)
      } else {
        false
      }
    }
    loop(1)
  }

  // exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  // exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }
    go(n, 1)
  }

  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  private def formatAbs(x: Int) = {
    formatResult("absolute value", x, abs)
  }

  private def formatFactorial(n: Int) = {
    formatResult("factorial", n, factorial)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(7))
  }
}
