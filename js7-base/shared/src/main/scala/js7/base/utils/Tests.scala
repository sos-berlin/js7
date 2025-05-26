package js7.base.utils

import js7.base.log.Logger
import js7.base.utils.SystemPropertiesExtensions.asSwitch

object Tests:

  val (isIntelliJIdea, isScalaTest, isSbt, isTest, isStrict, longTestEnabled) =
    val classNames = TestsPlatform.allActiveClasses

    def hasPackagePrefix(packagePrefixes: Set[String]): Boolean =
      classNames.exists(name => packagePrefixes.exists(name.startsWith))

    val isIntelliJIdea =
      hasPackagePrefix(Set("org.jetbrains.plugins.scala.testingSupport.scalaTest.ScalaTestRunner"))
    val isScalaTest = hasPackagePrefix(Set("org.scalatest."))
    val isSbt = hasPackagePrefix(Set("xsbt.boot."))
    val isTest = (isScalaTest || isSbt)
      && !sys.props.asSwitch("js7.noTest")
      && !sys.props.contains("test.speed")
    val isStrict = sys.props.asSwitch("js7.strict", ifMissing = isTest)
    val longTestEnabled = (isTest && isIntelliJIdea) || sys.props.asSwitch("js7.longTest")

    (isIntelliJIdea, isScalaTest, isSbt, isTest, isStrict, longTestEnabled)

  def log(): Unit =
    val onlyTrues = Vector(
      "isStrict" -> isStrict,
      "isTest" -> isTest,
      "isIntelliJIdea" -> isIntelliJIdea,
      "isScalaTest" -> isScalaTest,
      "longTestEnabled" -> longTestEnabled,
      "isSbt" -> isSbt
    ).filter(_._2)
    if onlyTrues.nonEmpty then
      Logger[this.type].info(onlyTrues.view.map(_._1).mkString(" · "))

  val isTestParallel: Boolean =
    isTest && sys.props.contains("test.parallel" /*see build.sbt*/)
