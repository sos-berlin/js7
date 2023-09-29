package js7.base.utils

import js7.base.log.Logger

object Tests:
  val (isIntelliJIdea, isScalaTest, isSbt, isTest, isStrict) =
    val classNames = TestsPlatform.allActiveClasses

    def hasPackagePrefix(packagePrefixes: Set[String]): Boolean =
      classNames.exists(name => packagePrefixes.exists(name.startsWith))

    val isIntelliJIdea =
      hasPackagePrefix(Set("org.jetbrains.plugins.scala.testingSupport.scalaTest.ScalaTestRunner"))
    val isScalaTest = hasPackagePrefix(Set("org.scalatest."))
    val isSbt = hasPackagePrefix(Set("xsbt.boot."))
    val isTest = isScalaTest || isSbt
    val isStrict = isTest || sys.props.contains("js7.strict")

    (isIntelliJIdea, isScalaTest, isSbt, isTest, isStrict)

  def log(): Unit =
    val onlyTrues = Vector(
      "isStrict" -> isStrict,
      "isTest" -> isTest,
      "isIntelliJIdea" -> isIntelliJIdea,
      "isScalaTest" -> isScalaTest,
      "isSbt" -> isSbt
    ).filter(_._2)
    if onlyTrues.nonEmpty then
      Logger[this.type].info(onlyTrues.view.map(_._1).mkString(" · "))
