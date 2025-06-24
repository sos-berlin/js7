package js7.base.catsutils

import cats.effect.unsafe.IORuntime
import js7.base.log.{Log4j, Logger}
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.base.utils.Tests.isStrict

trait OurApp extends IOAppWithCpuStarvationCheck:

  protected def productName: String =
    getClass.shortClassName.stripSuffix("Main")

  override protected final lazy val runtime: IORuntime =
    // Initialize Logger only when runtime is used, as in production.
    // If a test calls a method of the main program, then this code is not executed.
    Log4j.earlyInitializeForProduction()
    Logger.initialize(productName)
    OurIORuntime.commonIORuntime

  override protected def blockedThreadDetectionEnabled =
    super.blockedThreadDetectionEnabled || isStrict
