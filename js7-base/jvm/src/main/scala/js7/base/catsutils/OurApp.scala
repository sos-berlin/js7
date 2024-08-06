package js7.base.catsutils

import cats.effect.unsafe.IORuntime
import js7.base.utils.Tests.isIntelliJIdea

trait OurApp extends IOAppWithCpuStarvationCheck:

  override protected final lazy val runtime: IORuntime =
     OurIORuntime.commonIORuntime

  override protected def blockedThreadDetectionEnabled = isIntelliJIdea
