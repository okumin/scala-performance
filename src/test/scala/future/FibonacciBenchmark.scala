package future

import org.scalameter.Bench
import org.scalameter.api._
import scala.concurrent.Await
import scala.concurrent.duration._

object FibonacciBenchmark extends Bench.LocalTime {
  val max = 30
  val sizes: Gen[Int] = Gen.range("size")(10, max, 5)
  val timeout = 100.seconds

  val expected: Map[Int, Int] = {
    (0 to max).map { x =>
      x -> Fibonacci.fibonacci(x)
    }.toMap
  }

  performance of "fibonacci" in {
    measure method "fibonacci" in {
      using(sizes) in { x =>
        assert(Fibonacci.fibonacci(x) == expected(x))
      }
    }

    measure method "scala.util.Try" in {
      using(sizes) in { x =>
        assert(Fibonacci.fibonacciTry(x).get == expected(x))
      }
    }

    measure method "scala.util.control.TailCalls" in {
      using(sizes) in { x =>
        assert(Fibonacci.fibonacciTailCalls(x).result == expected(x))
      }
    }

    measure method "scalaz.Trampoline" in {
      using(sizes) in { x =>
        assert(Fibonacci.fibonacciTrampoline(x).run == expected(x))
      }
    }

    measure method "scala.concurrent.Future with global context" in {
      using(sizes) in { x =>
        import scala.concurrent.ExecutionContext.Implicits.global
        assert(Await.result(Fibonacci.fibonacciFuture(x), timeout) == expected(x))
      }
    }

    measure method "scala.concurrent.Future with thread pool context" in {
      using(sizes) in { x =>
        import future.ThreadPoolContext.Implicits.context
        assert(Await.result(Fibonacci.fibonacciFuture(x), timeout) == expected(x))
      }
    }

    measure method "scala.concurrent.Future with BlockContext" in {
      using(sizes) in { x =>
        import future.BlockContext.Implicits.context
        assert(Await.result(Fibonacci.fibonacciFuture(x), timeout) == expected(x))
      }
    }

    measure method "scala.concurrent.Future with trampoline context" in {
      using(sizes) in { x =>
        import play.api.libs.iteratee.Execution.Implicits.trampoline
        assert(Await.result(Fibonacci.fibonacciFuture(x), timeout) == expected(x))
      }
    }

    measure method "scalaz.concurrent.Task" in {
      using(sizes) in { x =>
        assert(Fibonacci.fibonacciTask(x).run == expected(x))
      }
    }

    measure method "scalaz.concurrent.Task with fork" in {
      using(sizes) in { x =>
        assert(Fibonacci.fibonacciTaskWithFork(x).run == expected(x))
      }
    }

    measure method "scalaz.concurrent.Task with optimized" in {
      using(sizes) in { x =>
        assert(Fibonacci.fibonacciTaskOptimized(x).run == expected(x))
      }
    }
  }
}
