package test

import org.scalameter.Bench
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

trait Benchmark extends Bench.LocalTime {
  val timeout = 1000.seconds
  val testCase = Option(System.getProperty("test.case"))
  implicit class RichScope(scope: Scope) {
    def when(tags: String*)(f: => Unit): Unit = testCase match {
      case Some(x) if !tags.contains(x) =>
      case _ => scope.in(f)
    }
  }

  def await[A](future: Future[A]): A = {
    Await.result(future, timeout)
  }
}
