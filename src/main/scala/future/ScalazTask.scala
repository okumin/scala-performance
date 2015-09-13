package future

import scalaz.concurrent.Task
import scalaz.concurrent.Task._

object ScalazTask {
  def fibonacci(n: Int): Task[Int] = n match {
    case 0 => now(0)
    case 1 => now(1)
    case i =>
      for {
        x <- fibonacci(i - 2)
        y <- fibonacci(i - 1)
      } yield x + y
  }

  def fibonacciF(n: Int): Task[Int] = n match {
    case 0 => now(0)
    case 1 => now(1)
    case i =>
      for {
        x <- fork(fibonacciF(i - 2))
        y <- fork(fibonacciF(i - 1))
      } yield x + y
  }

  def fibonacciT(n: Int): Task[Int] = n match {
    case 0 => now(0)
    case 1 => now(1)
    case i =>
      for {
        x <- suspend(fibonacciT(i - 2))
        y <- suspend(fibonacciT(i - 1))
      } yield x + y
  }

  private[this] val maxStack = 512

  def fibonacciO(n: Int): Task[Int] = {
    def step(i: Int, stack: Int): Task[Int] = {
      if (stack >= maxStack) suspend(fibonacciO(i)) else go(i, stack + 1)
    }
    def go(n: Int, stack: Int): Task[Int] = n match {
      case 0 => now(0)
      case 1 => now(1)
      case i =>
        for {
          x <- step(i - 2, stack)
          y <- step(i - 1, stack)
        } yield x + y
    }
    go(n, 0)
  }
}
