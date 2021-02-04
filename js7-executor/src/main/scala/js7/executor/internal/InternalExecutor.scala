package js7.executor.internal

import java.lang.reflect.Modifier.isPublic
import java.lang.reflect.{Constructor, InvocationTargetException}
import js7.base.problem.Checked.CheckedOption
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax._
import js7.data.job.InternalExecutable
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.executor.internal.InternalExecutor.startClass
import js7.executor.internal.InternalJob.{JobContext, OrderProcess, OrderContext}
import monix.eval.Task
import scala.util.control.NonFatal

final class InternalExecutor(
  executable: InternalExecutable,
  workflowJob: WorkflowJob)
{
  private lazy val runningJob: Checked[InternalJob] =
    startClass(executable.className, JobContext(executable, workflowJob))

  val start: Task[Checked[Unit]] =
    Task.pure(runningJob).flatMapT(_.start).memoize

  def processOrder(context: OrderContext): Task[Checked[OrderProcess]] =
    start.flatMapT(_ => Task.pure(runningJob.map(_
      .processOrder(context))))

  override def toString = s"InternalExecutor(${executable.className})"
}

object InternalExecutor
{
  def startClass(className: String, startJob: JobContext): Checked[InternalJob] =
    loadClass(className)
      .flatMap(getConstructor)
      .flatMap(construct(_, startJob))

  private def loadClass(className: String): Checked[Class[_]] =
    try Right(Class.forName(className))
    catch {
      case t: ExceptionInInitializerError =>
        Left(Problem.pure(Option(t.getCause).getOrElse(t).toStringWithCauses))
      case t: LinkageError =>
        Left(Problem.pure(t.toStringWithCauses))
      case NonFatal(t) =>
        Left(Problem.pure(t.toStringWithCauses))
    }

  private def getConstructor(clas: Class[_]): Checked[Constructor[InternalJob]] =
    Checked.catchNonFatal {
      val constructors = clas
        .getConstructors.asInstanceOf[Array[Constructor[InternalJob]]]
        .filter(o => isPublic(o.getModifiers))
      constructors
        .find(_.getParameterTypes sameElements Array(classOf[InternalJob.JobContext]))
        .orElse(constructors.find(_.getParameterTypes.isEmpty))
        .toChecked(Problem.pure(
          s"Class '${clas.getName}' does not have an appropriate constructor (empty or InternalJob.Context)"))
    }.flatten

  private def construct(constructor: Constructor[InternalJob], startJob: JobContext)
  : Checked[InternalJob] =
    try
      Right(constructor.getParameterCount match {
        case 0 => constructor.newInstance()
        case 1 => constructor.newInstance(startJob)
      })
    catch {
      case t @ (_: InvocationTargetException | _: ExceptionInInitializerError) =>
        Left(Problem.fromThrowable(Option(t.getCause) getOrElse t))
      case NonFatal(t) =>
        Problem.fromThrowable(t)
    }
}
