package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.base.utils.ScalaUtils.RichAny
import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.system.OperatingSystem._
import java.io.File
import scala.concurrent.ExecutionContext

/**
 * @author Joacim Zschimmer
 */
object JavaProcess {
  lazy val OwnClasspath = systemProperty("java.class.path")
  private lazy val JavaHome = new File(systemProperty("java.home"))

  def startJava(
    processConfiguration: ProcessConfiguration,
    options: Seq[String],
    classpath: Option[String],
    mainClass: String,
    arguments: Seq[String])
    (implicit executionContext: ExecutionContext) =
  {
    val classpathEnv = "CLASSPATH" → (classpath getOrElse "").substitute("" → File.pathSeparator)  // Java does not like empty classpath
    RichProcess.start(
      processConfiguration.copy(
        fileOption = Some(JavaExecutable),
        additionalEnvironment = processConfiguration.additionalEnvironment + classpathEnv),
      file = JavaExecutable,
      arguments = options ++ List(mainClass) ++ arguments)
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
