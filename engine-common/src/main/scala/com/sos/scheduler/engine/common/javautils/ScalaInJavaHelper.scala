package com.sos.scheduler.engine.common.javautils

/**
 * @author Joacim Zschimmer
 */
private[javautils] object ScalaInJavaHelper {
  private[javautils] def toScalaSet[A <: AnyRef](o: Array[A]): Set[A] = o.toSet
}
