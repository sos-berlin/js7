package js7.base.utils

import scala.jdk.CollectionConverters._

object Tests
{
  private val classNamePrefixes = Set("org.scalatest.", "xsbt.boot.Boot.")

  lazy val isTest: Boolean =
    Thread.getAllStackTraces.asScala.view
      .flatMap(_._2.view)
      .map(_.getClassName)
      .exists(cls => classNamePrefixes.exists(cls.startsWith))
}
