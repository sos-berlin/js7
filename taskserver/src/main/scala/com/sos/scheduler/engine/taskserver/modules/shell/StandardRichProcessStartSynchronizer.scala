package com.sos.scheduler.engine.taskserver.modules.shell

import akka.actor.ActorRefFactory
import com.sos.scheduler.engine.common.async.synchronizer.OwnActorSynchronizer
import com.sos.scheduler.engine.taskserver.task.process.RichProcess
import javax.inject.{Inject, Singleton}

/**
  * @author Joacim Zschimmer
  */
@Singleton
final class StandardRichProcessStartSynchronizer @Inject()(implicit protected val actorRefFactory: ActorRefFactory)
extends RichProcessStartSynchronizer with OwnActorSynchronizer[RichProcess]
