package js7.base.utils

import scala.jdk.CollectionConverters.MapHasAsScala

private object TestsPlatform:
  def allActiveClasses: Set[String] =
    Thread.getAllStackTraces.asScala.view
      .flatMap(_._2.view)
      .map(_.getClassName)
      .toSet
