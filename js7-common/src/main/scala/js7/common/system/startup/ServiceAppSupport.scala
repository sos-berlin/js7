package js7.common.system.startup

import cats.effect.{IO, ResourceIO}
import com.typesafe.config.Config
import js7.base.metering.HeapWatch

/** Supporting resources for a main application. */
object ServiceAppSupport:

  def resource(config: Config, useOwnResponsivenessMeter: Boolean): ResourceIO[Unit] =
    for
      _ <- HeapWatch.resourceForMainApp[IO]
      _ <- LibrarySupport.service(config, useOwnResponsivenessMeter)
    yield
      ()
