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
      x -> Scala.fibonacci(x)
    }.toMap
  }

  performance of "fibonacci" in {
    measure method "raw" in {
      using(sizes) in { x =>
        assert(Scala.fibonacci(x) == expected(x))
      }
    }

    measure method "ScalaTry" in {
      using(sizes) in { x =>
        assert(ScalaTry.fibonacci(x).get == expected(x))
      }
    }

    measure method "ScalaFuture with global context" in {
      using(sizes) in { x =>
        import scala.concurrent.ExecutionContext.Implicits.global
        assert(Await.result(ScalaFuture.fibonacci(x), timeout) == expected(x))
      }
    }

    measure method "ScalaFuture with parallel running" in {
      using(sizes) in { x =>
        import scala.concurrent.ExecutionContext.Implicits.global
        assert(Await.result(ScalaFuture.fibonaccip(x), timeout) == expected(x))
      }
    }

    measure method "ScalaFuture with BlockContext" in {
      using(sizes) in { x =>
        import future.BlockContext.Implicits.context
        assert(Await.result(ScalaFuture.fibonacci(x), timeout) == expected(x))
      }
    }

    measure method "ScalaFuture with trampoline" in {
      using(sizes) in { x =>
        import play.api.libs.iteratee.Execution.Implicits.trampoline
        assert(Await.result(ScalaFuture.fibonacci(x), timeout) == expected(x))
      }
    }

    measure method "AkkaFastFuture with global context" in {
      using(sizes) in { x =>
        import scala.concurrent.ExecutionContext.Implicits.global
        assert(Await.result(AkkaFastFuture.fibonacci(x), timeout) == expected(x))
      }
    }

    measure method "ScalazTask" in {
      using(sizes) in { x =>
        assert(ScalazTask.fibonacci(x).run == expected(x))
      }
    }

    measure method "ScalazTask with fork" in {
      using(sizes) in { x =>
        assert(ScalazTask.fibonacciF(x).run == expected(x))
      }
    }

    measure method "ScalazTask with trampoline" in {
      using(sizes) in { x =>
        assert(ScalazTask.fibonacciT(x).run == expected(x))
      }
    }

    measure method "ScalazTask with optimized" in {
      using(sizes) in { x =>
        assert(ScalazTask.fibonacciO(x).run == expected(x))
      }
    }
  }
}
