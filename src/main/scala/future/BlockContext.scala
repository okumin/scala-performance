package future

import scala.concurrent.ExecutionContext

object BlockContext {
  object Implicits {
    implicit val context: ExecutionContext = BlockContext.context
  }

  val context: ExecutionContext = new ExecutionContext {
    override def execute(runnable: Runnable): Unit = runnable.run()
    override def reportFailure(cause: Throwable): Unit = ()
  }
}
