package js7.common.system.startup

import cats.effect.{IO, ResourceIO}
import cats.syntax.applicative.*
import com.typesafe.config.Config
import js7.base.bean.MBeanUtils.registerStaticMBean
import js7.base.metering.{CallMeterLoggingService, HeapMBean, ResponsivenessMeter}
import js7.base.service.Service
import js7.base.system.ThreadsMXBean
import js7.base.utils.AsyncLock
import js7.common.http.HttpMXBean
import js7.common.pekkohttp.web.session.SessionRegister

/** Supporting resources for a library. */
object LibrarySupport:

  def service(config: Config, useOwnResponsivenessMeter: Boolean): ResourceIO[Service] =
    for
      _ <- CallMeterLoggingService.service(config)
      _ <- HeapMBean.register
      _ <- ResponsivenessMeter.service(config).whenA(useOwnResponsivenessMeter)
      _ <- registerStaticMBean("AsyncLock", AsyncLock.Bean)
      _ <- registerStaticMBean("HttpMXBean", HttpMXBean.Bean)
      _ <- registerStaticMBean("Sessions", SessionRegister.Bean)
      _ <- registerStaticMBean("Threads", ThreadsMXBean.Bean)
    yield
      Service.Empty
