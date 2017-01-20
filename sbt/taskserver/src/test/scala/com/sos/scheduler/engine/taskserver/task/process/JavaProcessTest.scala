package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.common.process.StdoutStderr._
import com.sos.scheduler.engine.common.scalautil.Closers.implicits._
import com.sos.scheduler.engine.common.scalautil.Closers.withCloser
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.Futures._
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.system.FileUtils._
import com.sos.scheduler.engine.common.time.ScalaTime._
import com.sos.scheduler.engine.common.time.Stopwatch
import com.sos.scheduler.engine.data.job.ReturnCode
import com.sos.scheduler.engine.taskserver.task.process.JavaProcessTest._
import java.lang.System.{err, exit, out}
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * @author Joacim Zschimmer
 */
final class JavaProcessTest extends FreeSpec {

  "JavaProcess" in {
    withCloser { closer ⇒
      val stdFileMap = RichProcess.createStdFiles(temporaryDirectory, id = "JavaProcessTest")
      closer.onClose { RichProcess.tryDeleteFiles(stdFileMap.values) }
      val stopwatch = new Stopwatch
      val process = JavaProcess.startJava(
        ProcessConfiguration(stdFileMap),
        options = List("-Xmx10m", s"-Dtest=$TestValue"),
        classpath = Some(JavaProcess.OwnClasspath),
        mainClass = JavaProcessTest.getClass.getName stripSuffix "$", // Scala object class name ends with '$'
        arguments = Arguments)
      try {
        val returnCode = process.waitForTermination()
        assert(returnCode == ReturnCode(77))
        assert(stdFileMap(Stdout).contentString contains s"STDOUT $TestValue")
        assert(stdFileMap(Stderr).contentString contains s"STDERR $TestValue")
      }
      finally process.close()
      awaitResult(process.closed, 10.s)
      logger.info(s"$stopwatch for Java process")
    }
  }
}

private object JavaProcessTest {
  private val TestValue = "TEST TEST"
  private val Arguments = Vector("a", "1 2")
  private val logger = Logger(getClass)

  def main(args: Array[String]): Unit = {
    if (args.toVector == Arguments) {
      out.println(s"STDOUT $TestValue")
      err.println(s"STDERR $TestValue")
      exit(77)
    } else
      exit(3)
  }
}
