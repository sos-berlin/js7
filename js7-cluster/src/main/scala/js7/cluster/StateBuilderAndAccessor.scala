package js7.cluster

import cats.effect
import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Ref}
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax.*
import js7.cluster.StateBuilderAndAccessor.*
import js7.data.event.{SnapshotableState, SnapshotableStateBuilder}

private final class StateBuilderAndAccessor[S <: SnapshotableState[S]](
  initialState: S)
  (implicit S: SnapshotableState.Companion[S]):

  private val ref = Ref.unsafe[IO, IO[S]](IO.pure(initialState))
  val state: IO[S] = ref.get.flatten

  def newStateBuilder()(using ioRuntime: IORuntime): SnapshotableStateBuilder[S] =
    val builder = S.newBuilder()
    IO.fromFuture(IO.pure(builder.synchronizedStateFuture))
      .flatMap(ref.set)
      .onError(t => IO:
        logger.error(s"PassiveClusterNode StateBuilderAndAccessor failed: ${t.toStringWithCauses}"))
      .unsafeRunAndForget()/*asynchronous ???*/
    builder

  // TODO Deprecate newStateBuilder!
  private def newStateBuilderIO(): IO[SnapshotableStateBuilder[S]] =
    IO.defer:
      val builder = S.newBuilder()
      IO.fromFuture(IO.pure(builder.synchronizedStateFuture))
        .flatMap(ref.set)
        .onError(t => IO:
          logger.error:
            s"PassiveClusterNode StateBuilderAndAccessor failed: ${t.toStringWithCauses}")
        .start /*asynchronous ???*/
        .as(builder)


object StateBuilderAndAccessor:
  private val logger = Logger[this.type]
