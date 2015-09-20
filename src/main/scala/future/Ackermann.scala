package future

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec
import scala.util.{Success, Try}
import scalaz.Free.Trampoline
import scalaz.Trampoline
import scalaz.concurrent.Task

object Ackermann {
  def await[A](future: Future[A]): A = {
    Await.result(future, 1000.seconds)
  }
  def main(args: Array[String]): Unit = {
    val m = 3
    val n = args(1).toInt
    args(0) match {
      case "ackermann" => println(ackermann(m, n))
      case "try" => println(ackermannTry(m, n).get)
      case "future_global" =>
        import scala.concurrent.ExecutionContext.Implicits.global
        println(await(ackermannFuture(m, n)))
      case "future_thread_pool" =>
        import ThreadPoolContext.Implicits.context
        println(await(ackermannFuture(m, n)))
      case "future_blocking" =>
        import BlockContext.Implicits.context
        println(await(ackermannFuture(m, n)))
      case "future_trampoline" =>
        import play.api.libs.iteratee.Execution.Implicits.trampoline
        println(await(ackermannFuture(m, n)))
      case "task" =>
        println(ackermannTask(m, n).run)
      case "task_fork" =>
        println(ackermannTaskWithFork(m, n).run)
      case "task_optimized" =>
        println(ackermannTaskOptimized(m, n).run)

    }
  }

  def ackermann(m: Int, n: Int): Int = (m, n) match {
    case (0, _) => n + 1
    case (_, 0) => ackermann(m - 1, 1)
    case (_, _) => ackermann(m - 1, ackermann(m, n - 1))
  }

  def ackermannTry(m: Int, n: Int): Try[Int] = (m, n) match {
    case (0, _) => Success(n + 1)
    case (_, 0) => ackermannTry(m - 1, 1)
    case (_, _) =>
      ackermannTry(m, n - 1).flatMap { x =>
        ackermannTry(m - 1, x)
      }
  }

  def ackermannTailCalls(m: Int, n: Int): TailRec[Int] = (m, n) match {
    case (0, _) => TailCalls.done(n + 1)
    case (_, 0) => TailCalls.tailcall(ackermannTailCalls(m - 1, 1))
    case (_, _) =>
      TailCalls.tailcall(ackermannTailCalls(m, n - 1)).flatMap { x =>
        TailCalls.tailcall(ackermannTailCalls(m - 1, x))
      }
  }

  def ackermannTrampoline(m: Int, n: Int): Trampoline[Int] = (m, n) match {
    case (0, _) => Trampoline.done(n + 1)
    case (_, 0) => Trampoline.suspend(ackermannTrampoline(m - 1, 1))
    case (_, _) =>
      Trampoline.suspend(ackermannTrampoline(m, n - 1)).flatMap { x =>
        Trampoline.suspend(ackermannTrampoline(m - 1, x))
      }
  }

  def ackermannFuture(m: Int, n: Int)(implicit executor: ExecutionContext): Future[Int] = {
    (m, n) match {
      case (0, _) => Future.successful(n + 1)
      case (_, 0) => Future(ackermannFuture(m - 1, 1)).flatMap(identity)
      case (_, _) =>
        Future(ackermannFuture(m, n - 1)).flatMap(identity).flatMap { x =>
          ackermannFuture(m - 1, x)
        }
    }
  }

  def ackermannTask(m: Int, n: Int): Task[Int] = (m, n) match {
    case (0, _) => Task.now(n + 1)
    case (_, 0) => Task.suspend(ackermannTask(m - 1, 1))
    case (_, _) =>
      Task.suspend(ackermannTask(m, n - 1)).flatMap { x =>
        ackermannTask(m - 1, x)
      }
  }

  def ackermannTaskWithFork(m: Int, n: Int): Task[Int] = (m, n) match {
    case (0, _) => Task.now(n + 1)
    case (_, 0) => Task.fork(ackermannTaskWithFork(m - 1, 1))
    case (_, _) =>
      Task.fork(ackermannTaskWithFork(m, n - 1)).flatMap { x =>
        Task.fork(ackermannTaskWithFork(m - 1, x))
      }
  }

  private[this] val MaxStack = 512

  def ackermannTaskOptimized(m: Int, n: Int): Task[Int] = {
    def step(m: Int, n: Int, stack: Int): Task[Int] = {
      if (stack >= MaxStack) Task.suspend(ackermannTaskOptimized(m, n)) else go(m, n, stack + 1)
    }
    def go(m: Int, n: Int, stack: Int): Task[Int] = (m, n) match {
      case (0, _) => Task.now(n + 1)
      case (_, 0) => step(m - 1, 1, stack)
      case (_, _) =>
        step(m, n - 1, stack).flatMap { x =>
          step(m - 1, x, stack)
        }
    }
    go(m, n, 0)
  }
}
