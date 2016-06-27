package com.sos.scheduler.engine.taskserver.configuration.inject

import com.google.inject.Provides
import com.sos.scheduler.engine.common.guice.ScalaAbstractModule
import com.sos.scheduler.engine.taskserver.data.TaskStartArguments
import javax.inject.Singleton
import scala.concurrent.Future

/**
 * @author Joacim Zschimmer
 */
final class TaskServerModule(
  taskStartArguments: TaskStartArguments,
  taskServerMainTerminated: Option[Future[Unit]])
extends ScalaAbstractModule {

  def configure() = {
    bindInstance[TaskStartArguments](taskStartArguments)
  }

  /** If task server runs in an own process, the Future of its termination. */
  @Provides @Singleton
  def unitFutureOption: Option[Future[Unit]] = taskServerMainTerminated
}
