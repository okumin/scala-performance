package future

import scala.concurrent.ExecutionContext
import scalaz.concurrent.Strategy

object ThreadPoolContext {
  object Implicits {
    implicit val context: ExecutionContext = ThreadPoolContext.context
  }
  
  val context: ExecutionContext = {
    ExecutionContext.fromExecutorService(Strategy.DefaultExecutorService)
  }
}
