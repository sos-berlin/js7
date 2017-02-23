package com.sos.scheduler.engine.common.system

import com.sos.scheduler.engine.common.scalautil.Collections.implicits.RichTraversableOnce
import com.sos.scheduler.engine.common.scalautil.Logger
import com.sos.scheduler.engine.common.scalautil.SideEffect.ImplicitSideEffect
import scala.collection.immutable
import scala.util.control.NonFatal

/**
 * @author Joacim Zschimmer
 */
final case class KernelVersion(kernelName: String, version: immutable.Seq[Int]) {
  def >=(o: KernelVersion) = kernelName == o.kernelName && (version compareElementWise o.version) >= 0

  override def toString = List(kernelName, version mkString ".") mkString " "
}

private object KernelVersion {
  private val logger = Logger(getClass)
  val Unknown = KernelVersion("UNKNOWN-KERNEL", Nil)

  private val Singleton = ignoreError { KernelVersion(sys.props("os.name"), parseVersion(sys.props("os.version"))) } sideEffect { o ⇒ logger.info(s"$o") }

  private def parseVersion(string: String) = (string split "[.-]" take 3 map { _.toInt }).toList

  private def ignoreError(body: ⇒ KernelVersion): KernelVersion =
    try body
    catch {
      case NonFatal(t) ⇒
        logger.warn(s"Ignored: $t", t)
        Unknown
    }

  def apply(): KernelVersion = Singleton
}
