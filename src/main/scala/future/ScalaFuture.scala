package future

import scala.concurrent.{ExecutionContext, Future}

object ScalaFuture {
  def fibonacci(n: Int)(implicit executor: ExecutionContext): Future[Int] = n match {
    case 0 => Future.successful(0)
    case 1 => Future.successful(1)
    case i =>
      for {
        x <- fibonacci(i - 2)
        y <- fibonacci(i - 1)
      } yield x + y
  }

  def fibonaccip(n: Int)(implicit executor: ExecutionContext): Future[Int] = n match {
    case 0 => Future.successful(0)
    case 1 => Future.successful(1)
    case i =>
      val a = fibonaccip(i - 2)
      val b = fibonaccip(i - 1)
      for {
        x <- a
        y <- b
      } yield x + y
  }
}
