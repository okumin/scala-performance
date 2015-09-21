package future

import org.scalameter.api._
import test.Benchmark

object FibonacciBenchmark extends Benchmark {
  val max = System.getProperty("fibonacci.max").toInt
  val sizes: Gen[Int] = Gen.range("size")(10, max, 5)

  val expected: Map[Int, Int] = {
    (0 to max).map { x =>
      x -> Fibonacci.fibonacci(x)
    }.toMap
  }

  performance of "fibonacci" in {
    measure.method("fibonacci").when("fibonacci") {
      using(sizes) in { x =>
        assert(Fibonacci.fibonacci(x) == expected(x))
      }
    }

    measure.method("scala.util.Try").when("try") {
      using(sizes) in { x =>
        assert(Fibonacci.fibonacciTry(x).get == expected(x))
      }
    }

    measure.method("scala.util.control.TailCalls").when("tail-calls") {
      using(sizes) in { x =>
        assert(Fibonacci.fibonacciTailCalls(x).result == expected(x))
      }
    }

    measure.method("scalaz.Trampoline").when("trampoline") {
      using(sizes) in { x =>
        assert(Fibonacci.fibonacciTrampoline(x).run == expected(x))
      }
    }

    measure.method("scala.concurrent.Future with global context").when("future-global") {
      using(sizes) in { x =>
        import scala.concurrent.ExecutionContext.Implicits.global
        assert(await(Fibonacci.fibonacciFuture(x)) == expected(x))
      }
    }

    measure.method("scala.concurrent.Future with thread pool context").when("future-thread-pool") {
      using(sizes) in { x =>
        import future.ThreadPoolContext.Implicits.context
        assert(await(Fibonacci.fibonacciFuture(x)) == expected(x))
      }
    }

    measure.method("scala.concurrent.Future with BlockContext").when("future-block") {
      using(sizes) in { x =>
        import future.BlockContext.Implicits.context
        assert(await(Fibonacci.fibonacciFuture(x)) == expected(x))
      }
    }

    measure.method("scala.concurrent.Future with trampoline context").when("future-trampoline") {
      using(sizes) in { x =>
        import play.api.libs.iteratee.Execution.Implicits.trampoline
        assert(await(Fibonacci.fibonacciFuture(x)) == expected(x))
      }
    }

    measure.method("scalaz.concurrent.Task").when("task") {
      using(sizes) in { x =>
        assert(Fibonacci.fibonacciTask(x).run == expected(x))
      }
    }

    measure.method("scalaz.concurrent.Task with fork").when("task-fork") {
      using(sizes) in { x =>
        assert(Fibonacci.fibonacciTaskWithFork(x).run == expected(x))
      }
    }

    measure.method("scalaz.concurrent.Task with optimized").when("task-optimized") {
      using(sizes) in { x =>
        assert(Fibonacci.fibonacciTaskOptimized(x).run == expected(x))
      }
    }
  }
}
