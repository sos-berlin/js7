package com.sos.scheduler.engine.taskserver.configuration.inject

import com.google.inject.{AbstractModule, Provides}
import com.sos.scheduler.engine.taskserver.data.{TaskServerMainTerminated, TaskStartArguments}
import javax.inject.Singleton
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
final class TaskServerModule(
  taskStartArguments: TaskStartArguments,
  taskServerMainTerminated: Option[Future[TaskServerMainTerminated.type]])
extends AbstractModule {

  def configure() = {
    bind(classOf[TaskStartArguments]) toInstance taskStartArguments
  }

  /** If task server runs in an own process, the Future of its termination. */
  @Provides @Singleton
  def terminatedFutureOption: Option[Future[TaskServerMainTerminated.type]] = taskServerMainTerminated
}
