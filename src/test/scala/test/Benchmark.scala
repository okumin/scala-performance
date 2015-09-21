package test

import org.scalameter.Bench
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

trait Benchmark extends Bench.LocalTime {
  val timeout = 1000.seconds
  val testCase = Option(System.getProperty("test.case"))
  implicit class RichScope(scope: Scope) {
    def when(tag: String)(f: => Unit): Unit = testCase match {
      case Some(`tag`) | None => scope.in(f)
      case Some(_) =>
    }
  }

  def await[A](future: Future[A]): A = {
    Await.result(future, timeout)
  }
}
