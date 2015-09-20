package future

import org.scalameter.Bench
import org.scalameter.api._
import scala.concurrent.Await
import scala.concurrent.duration._

object AckermannBenchmark extends Bench.LocalTime {
  val m = 3
  val min = 8
  val max = 12
  val sizes: Gen[Int] = Gen.range("size")(min, max, 1)
  val timeout = 1000.seconds
  val expected: Map[Int, Int] = {
    (0 to max).map { x =>
      x -> (math.pow(2, x + 3).toInt - 3)
    }.toMap
  }

  performance of "ackermann" in {
    measure method "ackermann" in {
      using(sizes) in { x =>
        if (max < 12) {
          assert(Ackermann.ackermann(m, x) == expected(x))
        }
      }
    }

    measure method "scala.util.Try" in {
      using(sizes) in { x =>
        if (max < 12) {
          assert(Ackermann.ackermannTry(m, x).get == expected(x))
        }
      }
    }

    measure method "scala.util.control.TailCalls" in {
      using(sizes) in { x =>
        assert(Ackermann.ackermannTailCalls(m, x).result == expected(x))
      }
    }

    measure method "scalaz.Trampoline" in {
      using(sizes) in { x =>
        assert(Ackermann.ackermannTrampoline(m, x).run == expected(x))
      }
    }

    measure method "scala.concurrent.Future with global context" in {
      using(sizes) in { x =>
        import scala.concurrent.ExecutionContext.Implicits.global
        assert(Await.result(Ackermann.ackermannFuture(m, x), timeout) == expected(x))
      }
    }

    measure method "scala.concurrent.Future with thread pool context" in {
      using(sizes) in { x =>
        import future.ThreadPoolContext.Implicits.context
        assert(Await.result(Ackermann.ackermannFuture(m, x), timeout) == expected(x))
      }
    }

    measure method "scala.concurrent.Future with BlockContext" in {
      using(sizes) in { x =>
        if (x < 11) {
          import future.BlockContext.Implicits.context
          assert(Await.result(Ackermann.ackermannFuture(m, x), timeout) == expected(x))
        }
      }
    }

    measure method "scala.concurrent.Future with trampoline context" in {
      using(sizes) in { x =>
        import play.api.libs.iteratee.Execution.Implicits.trampoline
        assert(Await.result(Ackermann.ackermannFuture(m, x), timeout) == expected(x))
      }
    }

    measure method "scalaz.concurrent.Task" in {
      using(sizes) in { x =>
        assert(Ackermann.ackermannTask(m, x).run == expected(x))
      }
    }

    measure method "scalaz.concurrent.Task with fork" in {
      using(sizes) in { x =>
        assert(Ackermann.ackermannTaskWithFork(m, x).run == expected(x))
      }
    }

    measure method "scalaz.concurrent.Task with optimized" in {
      using(sizes) in { x =>
        assert(Ackermann.ackermannTaskOptimized(m, x).run == expected(x))
      }
    }
  }
}
