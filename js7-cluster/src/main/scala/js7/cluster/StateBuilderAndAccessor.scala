package js7.cluster

import js7.base.log.Logger
import js7.base.thread.Futures.syntax.RichFuture
import js7.base.utils.ScalaUtils.syntax._
import js7.cluster.StateBuilderAndAccessor._
import js7.data.event.{JournaledState, JournaledStateBuilder}
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.Scheduler

private final class StateBuilderAndAccessor[S <: JournaledState[S]](
  initialState: S)
  (implicit S: JournaledState.Companion[S])
{
  private val getStateMVarTask = MVar.of[Task, Task[S]](Task.pure(initialState)).memoize
  val state: Task[S] = getStateMVarTask.flatMap(_.read.flatten)

  def newStateBuilder()(implicit s: Scheduler): JournaledStateBuilder[S] = {
    val builder = S.newBuilder()
    (for {
        mVar <- getStateMVarTask
        s <- Task.fromFuture(builder.synchronizedStateFuture)  // May wait, but getStateMVarTask has a value anyway
        _ <- mVar.take
        _ <- mVar.put(s)
      } yield ()
    ).runToFuture/*asynchronous ???*/
      .onFailure { case t =>
        logger.error(s"PassiveClusterNode StateBuilderAndAccessor failed: ${t.toStringWithCauses}")
      }
    builder
  }
}

object StateBuilderAndAccessor {
  private val logger = Logger(getClass)
}
