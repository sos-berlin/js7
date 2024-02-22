package js7.base.catsutils

import cats.effect.SyncIO
import cats.effect.unsafe.IORuntime
import cats.syntax.option.*
import java.util.concurrent.locks.ReentrantLock
import js7.base.utils.Atomic.extensions.*
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.{Allocated, Atomic}

object Js7IORuntime:

  val threadPrefix = "js7"

  private val lock = new ReentrantLock
  private var _ioRuntime = Atomic(none[Allocated[SyncIO, IORuntime]])

  final lazy val ioRuntime: IORuntime =
    lock.lockInterruptibly()
    try
      val allocated =
        OwnIORuntime
          .resource[SyncIO](
            name = threadPrefix,
            shutdownHooks = Seq(
              () => _ioRuntime = null))
          .toAllocated
          .unsafeRunSync()
      _ioRuntime := Some(allocated)
      allocated.allocatedThing
    finally
      lock.unlock()
