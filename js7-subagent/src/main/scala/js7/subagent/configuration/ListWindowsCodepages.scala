package js7.subagent.configuration

import cats.effect.{ExitCode, IO}
import fs2.{Chunk, Stream}
import java.nio.file.Paths
import js7.base.catsutils.OurApp
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import scala.concurrent.duration.Deadline
import scala.jdk.CollectionConverters.*

object ListWindowsCodepages extends OurApp:

  def run(args: List[String]): IO[ExitCode] =
    val subagentConf = SubagentConf.forTest(
      configAndData = Paths.get("/tmp/UNUSED"),
      name = "ListWindowsCodepages")

    val configuredCodepages = SubagentConf.DefaultConfig.getObject("js7.windows.codepages")
      .asScala.keySet

    val since = Deadline.now
    val n = 32768
    Stream.iterable(1 until n)
      // Parallelize for shorter duration (4s instead of 17s)
      .chunkLimit(n / sys.runtime.availableProcessors)
      .parEvalMapUnbounded(chunk => IO:
        chunk.flatMap: cp =>
          subagentConf.windowsCodepageToEncoding(cp) match
            case Left(_) => Chunk.empty // Ignore problem
            case Right(encoding) => Chunk.singleton(cp -> encoding))
      .unchunks
      .foreach((codepage, encoding) => IO:
        println(s"Windows code page $codepage -> $encoding " +
          encoding.aliases.asScala.toVector.sorted.mkString(", ") +
          configuredCodepages(codepage.toString) ?? " (configured)"))
      .compile
      .drain
      .<*(IO.whenA(false)(IO:
        println(since.elapsed.msPretty)))
      .as(ExitCode.Success)
