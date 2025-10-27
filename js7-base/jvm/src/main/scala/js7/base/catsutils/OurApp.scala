package js7.base.catsutils

import cats.effect.IOApp
import cats.effect.unsafe.IORuntime
import js7.base.BuildInfo
import js7.base.log.Logger
import js7.base.log.log4j.Log4j
import js7.base.system.startup.StartUp
import js7.base.utils.Atomic
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.base.utils.Tests.isStrict

trait OurApp extends IOApp:

  private val initialized = Atomic(false)

  protected def productName: String =
    getClass.shortClassName.stripSuffix("Main")

  override protected final lazy val runtime: IORuntime =
    // Initialize Logger only when runtime is used, as in production.
    // If a test calls a method of the main program, then this code is not executed.
    initialize()
    OurIORuntime.commonIORuntime

  override protected def blockedThreadDetectionEnabled =
    super.blockedThreadDetectionEnabled || isStrict

  private def initialize(): Unit =
    if !initialized.getAndSet(true) then
      StartUp.initializeMain()
      StartUp.printlnWithClock(s"JS7 $productName ${BuildInfo.prettyVersion}")
      Log4j.earlyInitializeForProduction()
      Logger.initialize(productName)
