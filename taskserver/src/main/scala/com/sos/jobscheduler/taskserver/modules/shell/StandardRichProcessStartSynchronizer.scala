package com.sos.jobscheduler.taskserver.modules.shell

import akka.actor.ActorRefFactory
import com.sos.jobscheduler.common.async.synchronizer.OwnActorSynchronizer
import com.sos.jobscheduler.taskserver.task.process.RichProcess
import javax.inject.{Inject, Singleton}

/**
  * @author Joacim Zschimmer
  */
@Singleton
final class StandardRichProcessStartSynchronizer @Inject()(implicit protected val actorRefFactory: ActorRefFactory)
extends RichProcessStartSynchronizer with OwnActorSynchronizer[RichProcess]
