package js7.base.catsutils

import cats.effect.IOApp

trait OurApp extends IOApp:

  override protected final val runtime = OurIORuntime.ioRuntime

