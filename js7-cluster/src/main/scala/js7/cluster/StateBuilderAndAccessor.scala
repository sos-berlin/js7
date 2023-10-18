package js7.cluster

import js7.base.log.Logger
import js7.base.thread.Futures.syntax.RichFuture
import js7.base.utils.ScalaUtils.syntax.*
import js7.cluster.StateBuilderAndAccessor.*
import js7.data.event.{SnapshotableState, SnapshotableStateBuilder}
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.Scheduler

private final class StateBuilderAndAccessor[S <: SnapshotableState[S]](
  initialState: S)
  (implicit S: SnapshotableState.Companion[S]):

  private val getStateMVarTask = MVar.of[Task, Task[S]](Task.pure(initialState)).memoize
  val state: Task[S] = getStateMVarTask.flatMap(_.read.flatten)

  def newStateBuilder()(implicit s: Scheduler): SnapshotableStateBuilder[S] =
    val builder = S.newBuilder()
    (for
        mVar <- getStateMVarTask
        s <- Task.fromFuture(builder.synchronizedStateFuture)  // May wait, but getStateMVarTask has a value anyway
        _ <- mVar.take
        _ <- mVar.put(s)
      yield ()
    ).runToFuture/*asynchronous ???*/
      .onFailure { case t =>
        logger.error(s"PassiveClusterNode StateBuilderAndAccessor failed: ${t.toStringWithCauses}")
      }
    builder


object StateBuilderAndAccessor:
  private val logger = Logger[this.type]
