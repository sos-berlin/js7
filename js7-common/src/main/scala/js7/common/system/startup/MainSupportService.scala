package js7.common.system.startup

import cats.effect.ResourceIO
import cats.syntax.applicative.*
import com.typesafe.config.Config
import js7.base.metering.{CallMeterLoggingService, ResponsivenessMeter}
import js7.base.service.Service
import js7.base.system.MBeanUtils.registerStaticMBean
import js7.base.system.ThreadsMXBean
import js7.base.utils.AsyncLock
import js7.common.http.HttpMXBean
import js7.common.pekkohttp.web.session.SessionRegister

/** Supporting services for a main program. */
object MainSupportService:

  def service(config: Config, useOwnResponsivenessMeter: Boolean): ResourceIO[Service] =
    for
      _ <- CallMeterLoggingService.service(config)
      _ <- ResponsivenessMeter.service(config).whenA(useOwnResponsivenessMeter)
      _ <- registerStaticMBean("Threads", ThreadsMXBean.Bean)
      _ <- registerStaticMBean("AsyncLock", AsyncLock.Bean)
      _ <- registerStaticMBean("HttpMXBean", HttpMXBean.Bean)
      _ <- registerStaticMBean("Sessions", SessionRegister.Bean)
    yield
      Service.Empty
