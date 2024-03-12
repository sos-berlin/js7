package js7.cluster

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import js7.base.catsutils.UnsafeMemoizable.unsafeMemoize
import js7.base.log.Logger
import js7.base.thread.Futures.syntax.RichFuture
import js7.base.utils.MVar
import js7.base.utils.ScalaUtils.syntax.*
import js7.cluster.StateBuilderAndAccessor.*
import js7.data.event.{SnapshotableState, SnapshotableStateBuilder}
import scala.concurrent.ExecutionContext

private final class StateBuilderAndAccessor[S <: SnapshotableState[S]](
  initialState: S)
  (implicit S: SnapshotableState.Companion[S]):

  private val getStateMVarIO = MVar[IO].of(IO.pure(initialState)).unsafeMemoize
  val state: IO[S] = getStateMVarIO.flatMap(_.read.flatten)

  def newStateBuilder()(using ioRuntime: IORuntime): SnapshotableStateBuilder[S] =
    given ExecutionContext = ioRuntime.compute
    val builder = S.newBuilder()
    (for
      mVar <- getStateMVarIO
      sIO <- IO.fromFuture(IO.pure(builder.synchronizedStateFuture)) // May wait, but getStateMVarIO has a value anyway
      _ <- mVar.take
      _ <- mVar.put(sIO)
     yield ()
    ).unsafeToFuture()/*asynchronous ???*/
      .onFailure { case t =>
        logger.error(s"PassiveClusterNode StateBuilderAndAccessor failed: ${t.toStringWithCauses}")
      }
    builder

  // TODO Deprecate newStateBuilder!
  private def newStateBuilderIO(): IO[SnapshotableStateBuilder[S]] =
    IO.defer:
      val builder = S.newBuilder()
      (for
        mVar <- getStateMVarIO
        sIO <- IO.fromFuture(IO.pure(builder.synchronizedStateFuture)) // May wait, but getStateMVarIO has a value anyway
        _ <- mVar.take
        _ <- mVar.put(sIO)
      yield ())
        .onError(t => IO:
          logger.error(s"PassiveClusterNode StateBuilderAndAccessor failed: ${t.toStringWithCauses}"))
        .start /*asynchronous ???*/
        .as(builder)


object StateBuilderAndAccessor:
  private val logger = Logger[this.type]
