package js7.common.system

import js7.data.system.JavaInformation
import js7.data.system.JavaInformation.Memory

/**
  * @author Joacim Zschimmer
  */
object JavaInformations:
  private val JavaSystemPropertyKeys = List(
    "java.vm.name",
    "java.runtime.name",
    "java.version",
    "java.vendor",
    "os.arch",
    "os.name",
    "os.version")
  private val systemProperties: Map[String, String] =
    (for
      k <- JavaSystemPropertyKeys
      v <- sys.props.get(k)
    yield k -> v).toMap

  def javaInformation(): JavaInformation =
    JavaInformation(
      version = implementationVersion,
      availableProcessors = sys.runtime.availableProcessors,
      Memory(
        maximum = sys.runtime.maxMemory,
        total = sys.runtime.totalMemory,
        free = sys.runtime.freeMemory),
      systemProperties)

  lazy val implementationVersion: String =
    try classOf[Runtime].getMethod("version").invoke(null).toString
    catch
      case _: Throwable => sys.props("java.version")
