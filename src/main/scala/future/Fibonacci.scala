package future

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec
import scala.util.{Success, Try}
import scalaz.Free.Trampoline
import scalaz.Trampoline
import scalaz.concurrent.Task

object Fibonacci {
  def await[A](future: Future[A]): A = {
    Await.result(future, 1000.seconds)
  }
  def main(args: Array[String]): Unit = {
    val n = args(1).toInt
    args(0) match {
      case "fibonacci" => println(fibonacci(n))
      case "try" => println(fibonacciTry(n).get)
      case "future_global" =>
        import scala.concurrent.ExecutionContext.Implicits.global
        println(await(fibonacciFuture(n)))
      case "future_thread_pool" =>
        import ThreadPoolContext.Implicits.context
        println(await(fibonacciFuture(n)))
      case "future_blocking" =>
        import BlockContext.Implicits.context
        println(await(fibonacciFuture(n)))
      case "future_trampoline" =>
        import play.api.libs.iteratee.Execution.Implicits.trampoline
        println(await(fibonacciFuture(n)))
      case "task" =>
        println(fibonacciTask(n).run)
      case "task_fork" =>
        println(fibonacciTaskWithFork(n).run)
      case "task_optimized" =>
        println(fibonacciTaskOptimized(n).run)
    }
  }

  def fibonacci(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case i => fibonacci(i - 2) + fibonacci(i - 1)
  }

  def fibonacciTry(n: Int): Try[Int] = n match {
    case 0 => Success(0)
    case 1 => Success(1)
    case i =>
      for {
        x <- fibonacciTry(i - 2)
        y <- fibonacciTry(i - 1)
      } yield x + y
  }

  def fibonacciTailCalls(n: Int): TailRec[Int] = n match {
    case 0 => TailCalls.done(0)
    case 1 => TailCalls.done(1)
    case i =>
      for {
        x <- TailCalls.tailcall(fibonacciTailCalls(i - 2))
        y <- TailCalls.tailcall(fibonacciTailCalls(i - 1))
      } yield x + y
  }

  def fibonacciTrampoline(n: Int): Trampoline[Int] = n match {
    case 0 => Trampoline.done(0)
    case 1 => Trampoline.done(1)
    case i =>
      for {
        x <- Trampoline.suspend(fibonacciTrampoline(i - 2))
        y <- Trampoline.suspend(fibonacciTrampoline(i - 1))
      } yield x + y
  }

  def fibonacciFuture(n: Int)(implicit executor: ExecutionContext): Future[Int] = n match {
    case 0 => Future.successful(0)
    case 1 => Future.successful(1)
    case i =>
      for {
        x <- fibonacciFuture(i - 2)
        y <- fibonacciFuture(i - 1)
      } yield x + y
  }

  def fibonacciTask(n: Int): Task[Int] = n match {
    case 0 => Task.now(0)
    case 1 => Task.now(1)
    case i =>
      for {
        x <- fibonacciTask(i - 2)
        y <- fibonacciTask(i - 1)
      } yield x + y
  }

  def fibonacciTaskWithFork(n: Int): Task[Int] = n match {
    case 0 => Task.now(0)
    case 1 => Task.now(1)
    case i =>
      for {
        x <- fibonacciTaskWithFork(i - 2)
        y <- Task.fork(fibonacciTaskWithFork(i - 1))
      } yield x + y
  }

  private[this] val MaxStack = 512

  def fibonacciTaskOptimized(n: Int): Task[Int] = {
    def step(i: Int, stack: Int): Task[Int] = {
      if (stack >= MaxStack) Task.suspend(fibonacciTaskOptimized(i)) else go(i, stack + 1)
    }
    def go(n: Int, stack: Int): Task[Int] = n match {
      case 0 => Task.now(0)
      case 1 => Task.now(1)
      case i =>
        for {
          x <- step(i - 2, stack)
          y <- step(i - 1, stack)
        } yield x + y
    }
    go(n, 0)
  }
}
