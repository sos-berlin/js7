package js7.master.cluster

import js7.base.utils.ScalaUtils.RichThrowable
import js7.common.scalautil.Logger
import js7.data.event.{JournaledState, JournaledStateBuilder}
import js7.master.cluster.StateBuilderAndAccessor._
import monix.catnap.MVar
import monix.eval.Task
import monix.execution.Scheduler
import scala.util.{Failure, Success}

private final class StateBuilderAndAccessor[S <: JournaledState[S]](
  initialState: S,
  originalNewStateBuilder: () => JournaledStateBuilder[S])
{
  private val getStateMVarTask = MVar.of[Task, Task[S]](Task.pure(initialState)).memoize
  val state: Task[S] = getStateMVarTask.flatMap(_.read.flatten)

  def newStateBuilder()(implicit s: Scheduler): JournaledStateBuilder[S] = {
    val builder = originalNewStateBuilder()
    (for {
        mVar <- getStateMVarTask
        s <- Task.fromFuture(builder.synchronizedStateFuture)  // May wait, but getStateMVarTask has a value anyway
        _ <- mVar.take
        _ <- mVar.put(s)
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
