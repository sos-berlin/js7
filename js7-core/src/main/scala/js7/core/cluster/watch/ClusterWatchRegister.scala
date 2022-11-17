package js7.core.cluster.watch

import javax.inject.{Inject, Singleton}
import js7.base.monixutils.MonixDeadline.syntax.*
import js7.data.controller.ControllerId
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.Scheduler

@Singleton
final class ClusterWatchRegister @Inject private[cluster](scheduler: Scheduler)
{
  private val mvarTask = MVar[Task].of(Map.empty[ControllerId, ClusterWatch]).memoize

  def apply(controllerId: ControllerId): Task[ClusterWatch] =
    mvarTask.flatMap(mvar =>
      mvar.read.flatMap(_
        .get(controllerId) match {
          case Some(custerWatch) =>
            Task.pure(custerWatch)
          case None =>
            mvar.take.flatMap { map =>
              map.get(controllerId) match {
                case Some(custerWatch) =>
                  mvar.put(map)
                    .map(_ => custerWatch)
                case None =>
                  val custerWatch = new ClusterWatch(controllerId,
                    () => scheduler.now)
                  mvar.put(map + (controllerId -> custerWatch))
                    .map(_ => custerWatch)
              }
            }
      }))

  def tryRead(controllerId: ControllerId): Task[Option[ClusterWatch]] =
    mvarTask.flatMap(_.read)
      .map(_.get(controllerId))
}
