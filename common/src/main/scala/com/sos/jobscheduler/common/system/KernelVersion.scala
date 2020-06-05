package js7.common.system

import js7.base.utils.Collections.implicits.RichTraversableOnce
import js7.base.utils.SideEffect.ImplicitSideEffect
import js7.common.scalautil.Logger
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
final case class KernelVersion(kernelName: String, version: Seq[Int]) {
  def >=(o: KernelVersion) = kernelName == o.kernelName && (version compareElementWise o.version) >= 0

  override def toString = List(kernelName, version mkString ".") mkString " "
}

private object KernelVersion {
  private val logger = Logger(getClass)
  val Unknown = KernelVersion("UNKNOWN-KERNEL", Nil)

  private val Singleton = ignoreError { KernelVersion(sys.props("os.name"), parseVersion(sys.props("os.version"))) } sideEffect { o => logger.info(s"$o") }

  private def parseVersion(string: String) = (string split "[.-]" take 3 map { _.toInt }).toList

  private def ignoreError(body: => KernelVersion): KernelVersion =
    try body
    catch {
      case NonFatal(t) =>
        logger.warn(s"Ignored: $t", t)
        Unknown
    }

  def apply(): KernelVersion = Singleton
}
