package js7.launcher.crashpidfile

import cats.effect.{ExitCode, IO, Ref}
import fs2.Stream
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.common.configuration.BasicConfiguration
import js7.common.system.startup.SimpleServiceProgram
import scala.concurrent.duration.*
import scala.concurrent.duration.Deadline.now

object TestPidReusage extends SimpleServiceProgram[BasicConfiguration.Empty]:

  private type Pid = Long

  def program(conf: BasicConfiguration.Empty) =
    findDuplicateWhileShowingProgress.flatMap: (pid, duration) =>
      IO:
        println(s"Duplicate PID $pid after ${duration.pretty}")
        ExitCode.Success

  private def findDuplicateWhileShowingProgress: IO[(Pid, Duration)] =
    Ref[IO].of[(Int, Pid)](0 -> 0).flatMap: ref =>

      def showProgress: IO[Unit] =
        IO(println()) *>
          Stream.awakeDelay[IO](100.ms).evalMap: duration =>
            ref.get.map: (n, pid) =>
              print(s"\r${duration.toSeconds}s $n PIDS${(n > 0) ?? s" (last one was $pid)"}")
          .onFinalize:
            IO(print("\r\u001b[2K"/*delete line*/))
          .compile.drain

      showProgress.background.surround:
        findDuplicatePid(
          onProcess = pid => ref.getAndUpdate((n, _) => (n + 1) -> pid).void)

  private def findDuplicatePid(onProcess: Pid => IO[Unit]): IO[(Pid, Duration)] =
    Stream.constant(())
      .covary[IO]
      .parEvalMap(2 * sys.runtime.availableProcessors): _ =>
        runProcess.flatTap(onProcess)
      .mapAccumulate(Map.empty[Pid, Deadline]): (pids, pid) =>
        (pids + (pid -> now)) -> (pids -> pid)
      .map(_._2)
      .collect:
        case (pids, pid) if pids.contains(pid) => pid -> pids(pid).elapsed
      .head.compile.last
      .map(_.getOrElse(sys.error("Empty result?")))

  private def runProcess: IO[Pid] =
    IO.fromCompletableFuture:
      IO:
        val pb = ProcessBuilder("/bin/sleep", "0")
        val process = pb.start()
        process.onExit()
    .uncancelable // leave no zombie
    .map(_.pid)
