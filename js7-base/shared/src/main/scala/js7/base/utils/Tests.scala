package js7.base.utils

import js7.base.convert.As.StringAsBoolean
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
    val isTest = (isScalaTest || isSbt)
      && !sys.props.get("js7.noTest").fold(false)(StringAsBoolean(_))
    val isStrict = isTest || sys.props.get("js7.strict").fold(false)(StringAsBoolean(_))

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

  val isTestParallel: Boolean =
    isTest && sys.props.contains("test.parallel" /*see build.sbt*/)
