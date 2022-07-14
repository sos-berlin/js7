package js7.base.utils

import scala.jdk.CollectionConverters.*

object Tests
{
  private val classNamePrefixes = Set("org.scalatest.", "xsbt.boot.Boot.")

  lazy val isStrict: Boolean =
    isTest || sys.props.contains("js7.strict")

  lazy val isTest: Boolean =
    Thread.getAllStackTraces.asScala.view
      .flatMap(_._2.view)
      .map(_.getClassName)
      .exists(cls => classNamePrefixes.exists(cls.startsWith))
}
