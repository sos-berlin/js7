package js7.base.io.process

import cats.effect.IO
import js7.base.log.Logger
import scala.jdk.OptionConverters.*

object ProcessExtensions:

  private val logger = Logger[this.type]

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
      //sleepWhileAliveTest *>
      IO.fromCompletableFuture:
        IO:
          processHandle.onExit()
            //.pipeIf(logger.isTraceEnable): future =>
            //  future.thenApply: h =>
            //    println("PID:${h.pid} onExit") // this is executed
            //    logger.debug(s"PID:${h.pid} processHandle.onExit ✔ ") // NOT EXECUTED
            //    h
      .map: (h: ProcessHandle) =>
        logger.trace(s"PID:${h.pid} processHandle.onExit ✔ ")

    //private def sleepWhileAliveTest: IO[Unit] =
    //  fs2.Stream.eval(IO(processHandle.isAlive))
    //    .delayBy(10.ms)
    //    .takeWhile(identity)
    //    .compile.drain