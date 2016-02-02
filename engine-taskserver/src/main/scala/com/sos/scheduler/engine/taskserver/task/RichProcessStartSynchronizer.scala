package com.sos.scheduler.engine.taskserver.task

import akka.actor.ActorRefFactory
import com.sos.scheduler.engine.common.async.synchronizer.OwnActorSynchronizer
import com.sos.scheduler.engine.taskserver.task.process.RichProcess
import javax.inject.{Inject, Singleton}

/**
  * @author Joacim Zschimmer
  */
@Singleton
final class RichProcessStartSynchronizer @Inject()(implicit protected val actorRefFactory: ActorRefFactory)
extends OwnActorSynchronizer[RichProcess]
