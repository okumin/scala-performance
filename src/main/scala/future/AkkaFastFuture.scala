package future

import akka.http.scaladsl.util.FastFuture._
import scala.concurrent.{ExecutionContext, Future}

object AkkaFastFuture {
  def fibonacci(n: Int)(implicit executor: ExecutionContext): Future[Int] = n match {
    case 0 => Future.successful(0)
    case 1 => Future.successful(1)
    case i =>
      for {
        x <- fibonacci(i - 2).fast
        y <- fibonacci(i - 1).fast
      } yield x + y
  }
}
