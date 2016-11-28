package com.sos.scheduler.engine.taskserver.configuration.inject

import com.google.inject.{AbstractModule, Provides}
import com.sos.scheduler.engine.taskserver.data.{TaskServerArguments, TaskServerMainTerminated}
import javax.inject.Singleton
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
final class TaskServerModule(
  arguments: TaskServerArguments,
  taskServerMainTerminated: Option[Future[TaskServerMainTerminated.type]])
extends AbstractModule {

  def configure() = {
    bind(classOf[TaskServerArguments]) toInstance arguments
  }

  /** If task server runs in an own process, the Future of its termination. */
  @Provides @Singleton
  def terminatedFutureOption: Option[Future[TaskServerMainTerminated.type]] = taskServerMainTerminated
}
