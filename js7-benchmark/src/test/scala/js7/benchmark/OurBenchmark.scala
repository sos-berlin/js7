package js7.benchmark

import cats.effect.SyncIO
import java.io.File
import java.net.URLClassLoader
import java.nio.file.Files.deleteIfExists
import java.nio.file.Paths
import java.util.regex.Pattern
import js7.base.log.Logger
import org.openjdk.jmh.annotations.{Scope, State, TearDown}
import org.openjdk.jmh.results.format.ResultFormatType
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options.OptionsBuilder
import scala.jdk.CollectionConverters.*

@State(Scope.Benchmark)
open class OurBenchmark:
  sys.props("js7.log4j.directory") = "target"
  deleteIfExists(Paths.get("target/js7.log"))
  deleteIfExists(Paths.get("target/js7-debug.log"))

  val loggerRelease = Logger.resource[SyncIO]("TestCycleExecutorBenchmark").allocated.unsafeRunSync()._2

  @TearDown
  def tearDown() =
    loggerRelease.unsafeRunSync()


object OurBenchmark:

  // Works only with forks(0):
  private def runBenchmark[A](cls: Class[A]) =
    new Runner(
      new OptionsBuilder()
        .shouldFailOnError(true)
        //.detectJvmArgs()
        .jvmArgsAppend:
          "-Djava.class.path=" +
            System.getProperty("java.class.path") + File.pathSeparator +
            cls.getClassLoader.asInstanceOf[URLClassLoader]
              .getURLs.mkString(File.pathSeparator)
        //.forks(0) // <-- mit forks(0) funktioniert es
        .include(Pattern.quote(cls.getName))
        //.shouldDoGC(true)
        .resultFormat(ResultFormatType.JSON)
        .result("js7-benchmark/target/benchmark.json")
        //.output("js7-benchmark/target/benchmark.log")
        .build
    ).run();
