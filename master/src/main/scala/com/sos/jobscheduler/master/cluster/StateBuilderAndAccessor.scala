package com.sos.jobscheduler.master.cluster

import com.sos.jobscheduler.base.utils.ScalaUtils.RichThrowable
import com.sos.jobscheduler.common.scalautil.Logger
import com.sos.jobscheduler.core.event.state.JournaledStateBuilder
import com.sos.jobscheduler.data.event.{Event, JournaledState}
import com.sos.jobscheduler.master.cluster.StateBuilderAndAccessor._
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.Scheduler
import scala.util.{Failure, Success}

private final class StateBuilderAndAccessor[S <: JournaledState[S, E], E <: Event](
  originalNewStateBuilder: () => JournaledStateBuilder[S, E])
{
  private val getStateMVarTask = MVar.empty[Task, Task[S]]().memoize
  val state: Task[S] = getStateMVarTask.flatMap(_.read.flatten)

  def newStateBuilder()(implicit s: Scheduler): JournaledStateBuilder[S, E] = {
    val builder = originalNewStateBuilder()
    (for {
        mVar <- getStateMVarTask
        _ <- mVar.tryTake
        _ <- mVar.put(builder.synchronizedStateTask)
      } yield ()
    ).runToFuture  // Asynchronous ???
      .onComplete {
        case Success(()) =>
        case Failure(t) => logger.error(s"PassiveClusterNode StateBuilderAndAccessor failed: ${t.toStringWithCauses}")
      }
    builder
  }
}

object StateBuilderAndAccessor {
  private val logger = Logger(getClass)
}
