package js7.base.test

import js7.base.system.OperatingSystem.isUnix
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import org.scalatest.freespec.AnyFreeSpec

abstract class OurTestSuite extends AnyFreeSpec, LoggingAnyFreeSpec, TestMixin:

  protected final def unixOnly(body: => Unit): Unit =
    if !isUnix then
      info("❗️This test requires Unix")
    else
      body

  protected final def repeatTest(n: Int)(body: Int => Any): Unit =
    for i <- 1 to n do
      Logger(getClass.scalaName).info(s"╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌ repeatTest #$i")
      withClue(s"#$i: "):
        body(i)
