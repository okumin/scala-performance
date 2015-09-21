package future

import org.scalameter.api._
import test.Benchmark

object AckermannBenchmark extends Benchmark {
  val m = 3
  val min = 8
  val max = System.getProperty("ackermann.max").toInt
  val sizes: Gen[Int] = Gen.range("size")(min, max, 1)
  val expected: Map[Int, Int] = {
    (0 to max).map { x =>
      x -> (math.pow(2, x + 3).toInt - 3)
    }.toMap
  }

  performance of "ackermann" in {
    measure.method("ackermann").when("ackermann") {
      using(sizes) in { x =>
        assert(Ackermann.ackermann(m, x) == expected(x))
      }
    }

    measure.method("scala.util.Try").when("try") {
      using(sizes) in { x =>
        assert(Ackermann.ackermannTry(m, x).get == expected(x))
      }
    }

    measure.method("scala.util.control.TailCalls").when("tail-calls") {
      using(sizes) in { x =>
        assert(Ackermann.ackermannTailCalls(m, x).result == expected(x))
      }
    }

    measure.method("scalaz.Trampoline").when("trampoline") {
      using(sizes) in { x =>
        assert(Ackermann.ackermannTrampoline(m, x).run == expected(x))
      }
    }

    measure.method("scala.concurrent.Future with global context").when("future-global") {
      using(sizes) in { x =>
        import scala.concurrent.ExecutionContext.Implicits.global
        assert(await(Ackermann.ackermannFuture(m, x)) == expected(x))
      }
    }

    measure.method("scala.concurrent.Future with thread pool context").when("future-thread-pool") {
      using(sizes) in { x =>
        import future.ThreadPoolContext.Implicits.context
        assert(await(Ackermann.ackermannFuture(m, x)) == expected(x))
      }
    }

    measure.method("scala.concurrent.Future with BlockContext").when("future-block") {
      using(sizes) in { x =>
        import future.BlockContext.Implicits.context
        assert(await(Ackermann.ackermannFuture(m, x)) == expected(x))
      }
    }

    measure.method("scala.concurrent.Future with trampoline context").when("future-trampoline") {
      using(sizes) in { x =>
        import play.api.libs.iteratee.Execution.Implicits.trampoline
        assert(await(Ackermann.ackermannFuture(m, x)) == expected(x))
      }
    }

    measure.method("scalaz.concurrent.Task").when("task") {
      using(sizes) in { x =>
        assert(Ackermann.ackermannTask(m, x).run == expected(x))
      }
    }

    measure.method("scalaz.concurrent.Task with fork").when("task-fork") {
      using(sizes) in { x =>
        assert(Ackermann.ackermannTaskWithFork(m, x).run == expected(x))
      }
    }

    measure.method("scalaz.concurrent.Task with optimized").when("task-optimized") {
      using(sizes) in { x =>
        assert(Ackermann.ackermannTaskOptimized(m, x).run == expected(x))
      }
    }
  }
}
