package js7.base.utils

import scala.jdk.CollectionConverters._

object Tests
{
  lazy val isStrict: Boolean =
    isTest || sys.props.contains("js7.strict")

  lazy val isTest: Boolean =
    isScalaTest || isSbt

  lazy val isScalaTest: Boolean =
    hasPackagePrefix(Set("org.scalatest."))

  lazy val isSbt: Boolean =
    hasPackagePrefix(Set("xsbt.boot."))

  lazy val isIntelliJIdea: Boolean =
    hasPackagePrefix(Set("com.intellij.rt."))

  private def hasPackagePrefix(packagePrefixes: Set[String]): Boolean =
    Thread.getAllStackTraces.asScala.view
      .flatMap(_._2.view)
      .map(_.getClassName)
      .exists(cls => packagePrefixes.exists(cls.startsWith))
}
