package js7.base.catsutils

import cats.effect.SyncIO
import cats.effect.unsafe.{IORuntime, Scheduler}

object Js7IORuntime:

  val threadPrefix = "js7"

  private var _ioRuntime: IORuntime =
    OwnIORuntime
      .resource[SyncIO](
        name = threadPrefix,
        shutdownHooks = Seq(
          () => _ioRuntime = null))
      .allocated
      .unsafeRunSync()
      ._1

  def ioRuntime: IORuntime =
    _ioRuntime
