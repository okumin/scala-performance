package future

object Scala {
  def fibonacci(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case i => fibonacci(i - 2) + fibonacci(i - 1)
  }
}
