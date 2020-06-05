package js7.taskserver.modules.shell

import akka.actor.ActorSystem
import js7.common.async.synchronizer.OwnActorSynchronizer
import js7.taskserver.task.process.RichProcess
import javax.inject.{Inject, Singleton}

/**
  * @author Joacim Zschimmer
  */
@Singleton
final class StandardRichProcessStartSynchronizer @Inject()(implicit protected val actorRefFactory: ActorSystem/*place actor in root*/)
extends RichProcessStartSynchronizer with OwnActorSynchronizer[RichProcess]
