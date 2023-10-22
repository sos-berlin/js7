package js7.cluster

import js7.base.log.Logger
import js7.base.thread.Futures.syntax.RichFuture
import js7.base.utils.ScalaUtils.syntax.*
import js7.cluster.StateBuilderAndAccessor.*
import js7.data.event.{SnapshotableState, SnapshotableStateBuilder}
import js7.base.utils.MVar
import cats.effect.IO
import monix.execution.Scheduler
import cats.effect.IO

private final class StateBuilderAndAccessor[S <: SnapshotableState[S]](
  initialState: S)
  (implicit S: SnapshotableState.Companion[S]):

  private val getStateMVarIO = MVar.of[IO, IO[S]](IO.pure(initialState)).memoize
  val state: IO[S] = getStateMVarIO.flatMap(_.read.flatten)

  def newStateBuilder()(implicit s: Scheduler): SnapshotableStateBuilder[S] =
    val builder = S.newBuilder()
    (for
        mVar <- getStateMVarIO
        s <- IO.fromFuture(builder.synchronizedStateFuture)  // May wait, but getStateMVarIO has a value anyway
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
