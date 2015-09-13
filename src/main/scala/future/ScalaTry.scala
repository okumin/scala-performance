package future

import scala.util.{Success, Try}

object ScalaTry {
  def fibonacci(n: Int): Try[Int] = n match {
    case 0 => Success(0)
    case 1 => Success(1)
    case i =>
      for {
        x <- fibonacci(i - 2)
        y <- fibonacci(i - 1)
      } yield x + y
  }
}
