package js7.base.io.process

import cats.effect.IO
import scala.jdk.OptionConverters.*

object ProcessExtensions:

  extension (process: Pid | Js7Process)
    def toPid: Pid =
      process match
        case pid: Pid => pid
        case process: Js7Process => process.pid

    def isAlive: Boolean =
      process match
        case pid: Pid => pid.maybeProcessHandle.exists(_.isAlive)
        case process: Js7Process => process.isAlive

    def maybeProcessHandle: Option[ProcessHandle] =
      process match
        case pid: Pid => ProcessHandle.of(pid.number).toScala
        case process: Js7Process => process.maybeHandle


  extension (processHandle: ProcessHandle)
    def onExitIO: IO[Unit] =
      IO.fromCompletableFuture(IO:
          processHandle.onExit())
        .void
