package js7.launcher.crashpidfile

import cats.effect.{Deferred, IO, Resource}
import cats.instances.vector.*
import cats.syntax.foldable.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import fs2.Stream
import java.io.FileInputStream
import js7.base.catsutils.CatsEffectExtensions.startAndForget
import js7.base.fs2utils.Fs2Utils.bytesToLines
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.temporaryDirectoryResource
import js7.base.io.process.ProcessSignal.SIGKILL
import js7.base.io.process.Processes.startProcess
import js7.base.io.process.{Pid, ReturnCode}
import js7.base.log.Logger
import js7.base.test.OurAsyncTestSuite
import js7.base.thread.IOExecutor.env.interruptibleVirtualThread
import js7.base.time.ScalaTime.*
import js7.launcher.crashpidfile.CrashPidFileKiller.Conf
import js7.launcher.crashpidfile.CrashPidFileKillerTest.*
import scala.concurrent.duration.SECONDS

final class CrashPidFileKillerTest extends OurAsyncTestSuite:

  "test" in:
    temporaryDirectoryResource[IO]("CrashPidFileKillerTest-").use: dir =>
      val scriptFile = dir / "script.sh"
      val pidFile = dir / "pidfile"

      CrashPidFileService.file(pidFile).use: pidFileService =>
        IO.defer:
          scriptFile.writeUtf8Executable:
            """#!/usr/bin/env bash
              |sh -c 'echo SLEEP A; sleep 111' &
              |sh -c 'echo SLEEP B; sleep 111' &
              |wait
              |""".stripMargin

          (1 to 10).toVector
            .parTraverse: _ =>
              startProcess(scriptFile)
                .flatMap: (process, stdout, stderr) =>
                  val childA, childB = Deferred.unsafe[IO, Unit]
                  stdout.through(bytesToLines)
                    .merge:
                      stderr.through(bytesToLines)
                    .evalTap: line =>
                      IO(logger.info(s"${Pid(process.pid)} -> $line"))
                    .evalTap:
                      case "SLEEP A" => childA.complete(())
                      case "SLEEP B" => childB.complete(())
                      case _ => IO.unit
                    .compile.drain
                    .startAndForget
                    .flatMap: _ =>
                      IO.both(childA.get, childB.get)
                    .as(process)
            .flatTap: processes =>
              Stream.iterable(processes)
                .map(p => Pid(p.pid))
                .evalMap: pid =>
                  pidFileService.register(pid).allocated.void // We don't release
                .compile.drain
            .flatTap: _ =>
              Resource.fromAutoCloseable:
                IO(FileInputStream(pidFile.toFile))
              .use:
                CrashPidFileKiller.program(Conf(), _)
            .flatMap:
              _.traverse: process =>
                interruptibleVirtualThread:
                  process.waitFor(10, SECONDS)
                  assert(!process.isAlive)
                  assert(process.exitValue == ReturnCode(SIGKILL).number)
            .map(_.combineAll)


object CrashPidFileKillerTest:
  private val logger = Logger[this.type]
