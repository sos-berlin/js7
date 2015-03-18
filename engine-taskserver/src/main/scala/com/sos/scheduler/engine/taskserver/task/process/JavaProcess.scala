package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.common.scalautil.FileUtils.implicits._
import com.sos.scheduler.engine.common.scalautil.ScalaUtils.RichAny
import com.sos.scheduler.engine.common.system.OperatingSystem._
import java.io.File

/**
 * @author Joacim Zschimmer
 */
object JavaProcess {
  lazy val OwnClasspath = systemProperty("java.class.path")
  private lazy val JavaHome = new File(systemProperty("java.home"))

  def startJava(options: Seq[String], classpath: Option[String], mainClass: String, arguments: Seq[String]) =
    RichProcess.start(
      arguments = Vector(JavaExecutable.getPath) ++
        options ++
        (classpath.toVector flatMap { o ⇒ Vector("-classpath", o.substitute("" → File.pathSeparator)) }) ++    // java does not like empty classpath
        Vector(mainClass) ++
        arguments,
      infoProgramFile = JavaExecutable)

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
