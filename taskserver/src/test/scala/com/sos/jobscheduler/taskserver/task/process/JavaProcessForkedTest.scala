package com.sos.jobscheduler.taskserver.task.process

import com.sos.jobscheduler.common.log.Log4j
import com.sos.jobscheduler.common.scalautil.AutoClosing.autoClosing
import com.sos.jobscheduler.common.scalautil.Closers.implicits._
import com.sos.jobscheduler.common.scalautil.Closers.withCloser
import com.sos.jobscheduler.common.scalautil.FileUtils.implicits._
import com.sos.jobscheduler.common.scalautil.Futures._
import com.sos.jobscheduler.common.scalautil.Futures.implicits._
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.common.system.FileUtils._
import com.sos.jobscheduler.common.time.ScalaTime._
import com.sos.jobscheduler.common.time.Stopwatch
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.system.{Stderr, Stdout}
import com.sos.jobscheduler.taskserver.task.process.JavaProcessForkedTest._
import java.lang.System.{err, out}
import org.scalatest.FreeSpec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.control.NonFatal

/**
  * build.sbt forks "IT" tests. This is needed to a propery sys.props("java.class.path") for JavaProcess.
  *
  * @author Joacim Zschimmer
  */
final class JavaProcessForkedTest extends FreeSpec {

  "JavaProcess" in {
    logger.trace("JavaProcessForkedTest")  // Avoid "The following set of substitute loggers may have been accessed during the initialization phase"
    withCloser { closer ⇒
      val stdFileMap = RichProcess.createStdFiles(temporaryDirectory, id = "JavaProcessForkedTest")
      closer.onClose { RichProcess.tryDeleteFiles(stdFileMap.values) }
      val stopwatch = new Stopwatch
      val process = JavaProcess.startJava(
        ProcessConfiguration(stdFileMap),
        options = List("-Xmx10m", s"-Dtest=$TestValue"),
        classpath = Some(JavaProcess.OwnClasspath),
        mainClass = JavaProcessForkedTest.getClass.getName stripSuffix "$", // Scala object class name ends with '$'
        arguments = Arguments)
      try {
        val returnCode = process.terminated await 99.s
        if (returnCode != ReturnCode(77)) {
          info(stdFileMap(Stdout).contentString)
          info(stdFileMap(Stderr).contentString)
        }
        assert(returnCode == ReturnCode(77))
        assert(stdFileMap(Stdout).contentString contains s"STDOUT $TestValue")
        assert(stdFileMap(Stderr).contentString contains s"STDERR $TestValue")
      }
      catch { case NonFatal(t) ⇒
        for ((typ, path) ← stdFileMap)
          autoClosing(scala.io.Source.fromFile(path)) { source ⇒
            for (line ← source.getLines) alert(s"$typ: $line")
          }
        throw t
      }
      finally process.close()
      awaitResult(process.closed, 10.s)
      info(s"$stopwatch for Java process")
    }
  }
}

private object JavaProcessForkedTest {
  private val TestValue = "TEST TEST"
  private val Arguments = Vector("a", "1 2")
  private val logger = Logger(getClass)

  def main(args: Array[String]): Unit = {
    if (args.toVector == Arguments) {
      out.println(s"STDOUT $TestValue")
      err.println(s"STDERR $TestValue")
      Log4j.shutdown()
      sys.runtime.exit(77)
    } else {
      Log4j.shutdown()
      sys.runtime.exit(3)
    }
  }
}
