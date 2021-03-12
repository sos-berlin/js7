package js7.tests.jobs

import cats.syntax.traverse._
import java.nio.file.Files.deleteIfExists
import java.nio.file.{Path, Paths}
import js7.base.log.Logger
import js7.base.utils.ScalaUtils.syntax._
import js7.data.agent.AgentId
import js7.data.job.InternalExecutable
import js7.data.ordersource.FileOrderSource.FileArgumentName
import js7.data.value.NamedValues
import js7.data.workflow.instructions.Execute
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.executor.internal.InternalJob
import js7.executor.internal.InternalJob.{OrderContext, OrderProcess, Result}
import js7.tests.jobs.DeleteFileJob._
import monix.eval.Task
import monix.reactive.Observer

final class DeleteFileJob extends InternalJob
{
  def processOrder(orderContext: OrderContext) =
    OrderProcess(
      orderContext.arguments.checked("file")
        .orElse(orderContext.order.arguments.checked(FileArgumentName))
        .map(_.convertToString)
        .map(Paths.get(_))
        .traverse(deleteFile(_, orderContext.out))
        .rightAs(Result(NamedValues.empty)))

  private def deleteFile(file: Path, out: Observer[String]): Task[Unit] =
    Task(deleteIfExists(file))
      .flatMap { deleted =>
        if (deleted)
          Task.defer {
            logger.info(s"Deleted $file")
            Task.deferFuture(out.onNext(s"Deleted $file\n"))
              .as(())
          }
        else
          Task.unit
      }
}

object DeleteFileJob
{
  private val logger = Logger[this.type]

  def execute(agentId: AgentId) =
    Execute(WorkflowJob(agentId, InternalExecutable(classOf[DeleteFileJob].getName)))
}
