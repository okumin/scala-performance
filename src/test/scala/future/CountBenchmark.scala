package future

import org.scalameter.api._
import test.Benchmark

object CountBenchmark extends Benchmark {
  val min = Option(System.getProperty("count.min")).map(_.toInt).getOrElse(1)
  val max = System.getProperty("count.max").toInt
  val sizes: Gen[Int] = Gen.exponential("size")(min, max, 10)

  performance of "count" in {
    measure.method("count").when("count") {
      using(sizes) in { x =>
        assert(Count.count(x) == x)
      }
    }

    measure.method("loop").when("loop", "big") {
      using(sizes) in { x =>
        assert(Count.countLoop(x) == x)
      }
    }

    measure.method("scala.util.Try").when("try") {
      using(sizes) in { x =>
        assert(Count.countTry(x).get == x)
      }
    }

    measure.method("scala.util.control.TailCalls").when("tail-calls", "big") {
      using(sizes) in { x =>
        assert(Count.countTailCalls(x).result == x)
      }
    }

    measure.method("scalaz.Trampoline").when("trampoline", "big") {
      using(sizes) in { x =>
        assert(Count.countTrampoline(x).run == x)
      }
    }

    measure.method("scala.concurrent.Future with global context").when("future-global", "big") {
      using(sizes) in { x =>
        import scala.concurrent.ExecutionContext.Implicits.global
        assert(await(Count.countFuture(x)) == x)
      }
    }

    measure.method("scala.concurrent.Future with thread pool context").when("future-thread-pool", "big") {
      using(sizes) in { x =>
        import future.ThreadPoolContext.Implicits.context
        assert(await(Count.countFuture(x)) == x)
      }
    }

    measure.method("scala.concurrent.Future with BlockContext").when("future-block") {
      using(sizes) in { x =>
        import future.BlockContext.Implicits.context
        assert(await(Count.countFuture(x)) == x)
      }
    }

    measure.method("scala.concurrent.Future with trampoline context").when("future-trampoline", "big") {
      using(sizes) in { x =>
        import play.api.libs.iteratee.Execution.Implicits.trampoline
        assert(await(Count.countFuture(x)) == x)
      }
    }

    measure.method("scalaz.concurrent.Task").when("task", "big") {
      using(sizes) in { x =>
        assert(Count.countTask(x).run == x)
      }
    }

    measure.method("scalaz.concurrent.Task with fork").when("task-fork", "big") {
      using(sizes) in { x =>
        assert(Count.countTaskWithFork(x).run == x)
      }
    }

    measure.method("scalaz.concurrent.Task with optimized").when("task-optimized", "big") {
      using(sizes) in { x =>
        assert(Count.countTaskOptimized(x).run == x)
      }
    }
  }
}
