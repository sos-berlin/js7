package js7.base.catsutils

import cats.effect.IOApp
import cats.effect.unsafe.IORuntime
import js7.base.utils.Tests.isIntelliJIdea

trait OurApp extends IOApp with CpuStarvationCheck:

  override protected final lazy val runtime: IORuntime =
    OurIORuntime.commonIORuntime

  override protected def blockedThreadDetectionEnabled = isIntelliJIdea
