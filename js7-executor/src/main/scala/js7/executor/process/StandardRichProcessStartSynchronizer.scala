package js7.executor.process

import akka.actor.ActorSystem
import javax.inject.{Inject, Singleton}
import js7.common.async.synchronizer.OwnActorSynchronizer

/**
  * @author Joacim Zschimmer
  */
@Singleton
final class StandardRichProcessStartSynchronizer @Inject()(implicit protected val actorRefFactory: ActorSystem/*place actor in root*/)
extends RichProcessStartSynchronizer with OwnActorSynchronizer[RichProcess]
