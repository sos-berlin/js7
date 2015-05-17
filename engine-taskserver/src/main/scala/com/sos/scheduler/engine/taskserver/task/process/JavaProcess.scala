package com.sos.scheduler.engine.taskserver.task.process

import com.google.common.io.Closer
import com.sos.scheduler.engine.common.scalautil.AutoClosing.closeOnError
import com.sos.scheduler.engine.common.scalautil.Closers.implicits.RichClosersCloser
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.ScalaUtils.RichAny
import com.sos.scheduler.engine.common.system.OperatingSystem._
import java.io.File
import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * @author Joacim Zschimmer
 */
object JavaProcess {
  lazy val OwnClasspath = systemProperty("java.class.path")
  private lazy val JavaHome = new File(systemProperty("java.home"))

  def startJava(
      options: Seq[String],
      classpath: Option[String],
      mainClass: String,
      arguments: Seq[String],
      environment: immutable.Iterable[(String, String)] = Nil) =
  {
    val closer = Closer.create()
    closeOnError(closer) {
      val stdFileMap = RichProcess.createTemporaryStdFiles()
      closer.onClose { RichProcess.tryDeleteFiles(stdFileMap.values) }
      val richProcess = RichProcess.start(
        additionalEnvironment = environment,
        arguments = Vector(JavaExecutable.getPath) ++
          options ++
          (classpath.toVector flatMap { o ⇒ Vector("-classpath", o.substitute("" → File.pathSeparator)) }) ++ // Java does not like empty classpath
          Vector(mainClass) ++
          arguments,
        stdFileMap = stdFileMap,
        infoProgramFile = JavaExecutable)
      richProcess.closed.onComplete { _ ⇒ closer.close() }
      richProcess
    }
  }

  private lazy val JavaExecutable: File = {
    // See http://stackoverflow.com/questions/4421658/how-can-i-start-a-second-java-process
    val result = if (isWindows) WindowsJava else UnixJava
    if (!result.exists || result.isDirectory) throw new RuntimeException(s"Missing Java executable expected at $result")
    result
  }

  private def WindowsJava = JavaHome / "bin/java.exe"   // Without sufficient memory, javaw.exe blocks with a Windows message box "Could not create the Java Virtual Machine"
  private def UnixJava = JavaHome / "bin/java"

  private def systemProperty(name: String): String =
    sys.props(name) match {
      case null ⇒ throw new RuntimeException(s"Missing Java system property '$name'")
      case o ⇒ o
    }
}
