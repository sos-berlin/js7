package com.sos.jobscheduler.core.cluster

import com.sos.jobscheduler.data.master.MasterId
import javax.inject.{Inject, Singleton}
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.Scheduler

@Singleton
final class ClusterWatchRegister @Inject private[cluster](scheduler: Scheduler)
{
  private val mvarTask = MVar[Task].of(Map.empty[MasterId, ClusterWatch]).memoize

  def apply(masterId: MasterId): Task[ClusterWatch] =
    mvarTask.flatMap(mvar =>
      mvar.read.flatMap(_
        .get(masterId) match {
          case Some(custerWatch) =>
            Task.pure(custerWatch)
          case None =>
            mvar.take.flatMap { map =>
              map.get(masterId) match {
                case Some(custerWatch) =>
                  mvar.put(map)
                    .map(_ => custerWatch)
                case None =>
                  val custerWatch = new ClusterWatch(masterId, scheduler)
                  mvar.put(map + (masterId -> custerWatch))
                    .map(_ => custerWatch)
              }
            }
      }))

  def tryRead(masterId: MasterId): Task[Option[ClusterWatch]] =
    mvarTask.flatMap(_.read)
      .map(_.get(masterId))
}
