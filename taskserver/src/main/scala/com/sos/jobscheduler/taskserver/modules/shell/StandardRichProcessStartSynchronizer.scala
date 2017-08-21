package com.sos.jobscheduler.taskserver.modules.shell

import akka.actor.ActorSystem
import com.sos.jobscheduler.common.async.synchronizer.OwnActorSynchronizer
import com.sos.jobscheduler.taskserver.task.process.RichProcess
import javax.inject.{Inject, Singleton}

/**
  * @author Joacim Zschimmer
  */
@Singleton
final class StandardRichProcessStartSynchronizer @Inject()(implicit protected val actorRefFactory: ActorSystem/*place actor in root*/)
extends RichProcessStartSynchronizer with OwnActorSynchronizer[RichProcess]
