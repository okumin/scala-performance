package future

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.TailCalls
import scala.util.control.TailCalls.TailRec
import scala.util.{Success, Try}
import scalaz.Free.Trampoline
import scalaz.Trampoline
import scalaz.concurrent.Task

object Count {
  def count(x: Int): Int = x match {
    case 0 => 0
    case _ => count(x - 1) + 1
  }

  def countLoop(x: Int): Int = {
    var i = 0
    while (i < x) {
      i += 1
    }
    i
  }

  def countTry(x: Int): Try[Int] = x match {
    case 0 => Success(0)
    case _ => countTry(x - 1).map(_ + 1)
  }

  def countTailCalls(x: Int): TailRec[Int] = x match {
    case 0 => TailCalls.done(0)
    case _ => TailCalls.tailcall(countTailCalls(x - 1)).map(_ + 1)
  }

  def countTrampoline(x: Int): Trampoline[Int] = x match {
    case 0 => Trampoline.done(0)
    case _ => Trampoline.suspend(countTrampoline(x - 1)).map(_ + 1)
  }

  def countFuture(x: Int)(implicit executor: ExecutionContext): Future[Int] = x match {
    case 0 => Future.successful(0)
    case _ => Future(countFuture(x - 1)).flatMap(identity).map(_ + 1)
  }
  
  def countTask(x: Int): Task[Int] = x match {
    case 0 => Task.now(0)
    case _ => Task.suspend(countTask(x - 1)).map(_ + 1)
  }

  def countTaskWithFork(x: Int): Task[Int] = x match {
    case 0 => Task.now(0)
    case _ => Task.fork(countTaskWithFork(x - 1)).flatMap { i => Task.delay(i + 1) }
  }

  private[this] val MaxStack = 512

  def countTaskOptimized(x: Int): Task[Int] = {
    def step(i: Int, stack: Int): Task[Int] = {
      if (stack >= MaxStack) Task.suspend(countTaskOptimized(i)) else go(i, stack + 1)
    }
    def go(n: Int, stack: Int): Task[Int] = n match {
      case 0 => Task.now(0)
      case i => step(i - 1, stack).map(_ + 1)
    }
    go(x, 0)
  }
}
