package js7.base.catsutils

import cats.effect.IOApp
import cats.effect.unsafe.IORuntime

trait OurApp extends IOApp:

  override protected final lazy val runtime: IORuntime = 
    OurIORuntime.commonIORuntime
