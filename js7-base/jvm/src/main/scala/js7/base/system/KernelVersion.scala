package js7.base.system

import js7.base.log.Logger
import js7.base.utils.Collections.implicits.RichIterableOnce
import js7.base.utils.SideEffect.ImplicitSideEffect
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
final case class KernelVersion(kernelName: String, version: Seq[Int]):

  def >=(o: KernelVersion): Boolean =
    kernelName == o.kernelName && (version compareElementWise o.version) >= 0

  override def toString: String =
    List(kernelName, version mkString ".") mkString " "

private object KernelVersion:
  private val logger = Logger[this.type]
  val Unknown = KernelVersion("UNKNOWN-KERNEL", Nil)

  private val Singleton = ignoreError { KernelVersion(sys.props("os.name"), parseVersion(sys.props("os.version"))) } sideEffect { o => logger.info(s"$o") }

  private def parseVersion(string: String) = string.split("[.-]").take(3).map(_.toInt).toList

  private def ignoreError(body: => KernelVersion): KernelVersion =
    try body
    catch
      case NonFatal(t) =>
        logger.warn(s"Ignored: $t", t)
        Unknown

  def apply(): KernelVersion = Singleton
