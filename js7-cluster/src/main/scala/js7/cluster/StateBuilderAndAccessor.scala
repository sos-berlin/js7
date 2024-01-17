package js7.cluster

import cats.effect.IO
import cats.effect.std.AtomicCell
import cats.effect.unsafe.IORuntime
import js7.base.catsutils.UnsafeMemoizable.given
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

  private val getStateMVarIO: IO[MVar[IO, S]] = MVar[IO].of(initialState).unsafeMemoize
  val state: IO[S] = getStateMVarIO.flatMap(_.read)

  @deprecated("Return IO!")
  def newStateBuilder()(using ioRuntime: IORuntime): SnapshotableStateBuilder[S] =
    given ExecutionContext = ioRuntime.compute
    val builder = S.newBuilder()
    (for
        mVar <- getStateMVarIO
        s <- IO.fromFuture(IO.pure(builder.synchronizedStateFuture)).flatten  // May wait, but getStateMVarIO has a value anyway
        _ <- mVar.take
        _ <- mVar.put(s)
      yield ()
    ).unsafeToFuture()/*asynchronous ???*/
      .onFailure { case t =>
        logger.error(s"PassiveClusterNode StateBuilderAndAccessor failed: ${t.toStringWithCauses}")
      }
    builder

  def newStateBuilderIO(): IO[SnapshotableStateBuilder[S]] =
    IO.defer:
      val builder = S.newBuilder()
      (for
        mVar <- getStateMVarIO
        s <- IO.fromFuture(IO.pure(builder.synchronizedStateFuture)).flatten // May wait, but getStateMVarIO has a value anyway
        _ <- mVar.take
        _ <- mVar.put(s)
      yield ())
        .onError(t => IO:
          logger.error(s"PassiveClusterNode StateBuilderAndAccessor failed: ${t.toStringWithCauses}"))
        .start /*asynchronous ???*/
        .as(builder)


object StateBuilderAndAccessor:
  private val logger = Logger[this.type]
