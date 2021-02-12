package js7.executor.internal

import cats.syntax.flatMap._
import java.lang.reflect.Modifier.isPublic
import java.lang.reflect.{Constructor, InvocationTargetException}
import js7.base.problem.Checked.CheckedOption
import js7.base.problem.{Checked, Problem}
import js7.base.utils.Classes.superclassesOf
import js7.base.utils.ScalaUtils.syntax._
import js7.common.scalautil.Logger
import js7.data.job.InternalExecutable
import js7.executor.internal.InternalExecutor._
import js7.executor.internal.InternalJob.{JobContext, OrderContext, OrderProcess}
import monix.eval.Task
import monix.execution.Scheduler
import scala.util.control.NonFatal

final class InternalExecutor(
  executable: InternalExecutable,
  blockingJobScheduler: Scheduler)
  (implicit scheduler: Scheduler)
{
  private lazy val runningJob: Checked[InternalJob] =
    toInstantiator(executable.className)
      .flatMap(_())

  val start: Task[Checked[Unit]] =
    Task { runningJob }
      .flatMapT(_.start)
      .tapEval {
        case Left(problem) => Task(
          logger.debug(s"${executable.className} start: $problem", problem.throwableOption.orNull))
        case Right(_) => Task.unit
      }
      .memoize

  def processOrder(context: OrderContext): Task[Checked[OrderProcess]] =
    start
      .flatMapT(_ => Task {
        runningJob.map(_.processOrder(context))
      })
      .map(_.map(orderProcess =>
        orderProcess.copy(
          completed = orderProcess.completed.flatTap(_ => Task {
            context.out.onComplete()
            context.err.onComplete()
          }))))
      .tapEval {
        case Left(problem) => Task(logger.debug(s"${executable.className} " +
          s"processOrder(${context.order.id.string}): $problem", problem.throwableOption.orNull))
        case Right(_) => Task.unit
      }

  private def toInstantiator(className: String): Checked[() => Checked[InternalJob]] =
    loadClass(className)
      .flatMap(cls =>
        if (classOf[InternalJob] isAssignableFrom cls)
          getConstructor(cls.asInstanceOf[Class[InternalJob]])
            .map(con => () => construct(con, toJobContext(cls)))
        else
          tryAdapter(cls))

  private def tryAdapter(cls: Class[_]): Checked[() => Checked[InternalJob]] = {
    val internalJobs = superclassesOf(cls)
      .flatMap(cls =>
        Option(cls.getAnnotation(classOf[InternalJobAdapter])))
      .map(_.value)
    if (internalJobs.sizeIs > 1)
      Left(Problem(s"Class ${cls.getName} has multiple @InternalJobAdapter annotations"))
    else
      internalJobs
        .headOption
        .toChecked(Problem(s"Class '${cls.getName}' is not an InternalJob"))
        .flatMap(getConstructor)
        .map(con =>
          () => construct(con, toJobContext(cls)))
  }

  private def toJobContext(cls: Class[_]) =
    JobContext(cls, executable.jobArguments, scheduler, blockingJobScheduler)

  override def toString = s"InternalExecutor(${executable.className})"
}

object InternalExecutor
{
  private val logger = Logger(getClass)

  private def loadClass(className: String): Checked[Class[_ <: AnyRef]] =
    try Right(Class.forName(className).asInstanceOf[Class[_ <: AnyRef]])
    catch {
      case t: ExceptionInInitializerError =>
        Left(Problem.pure(Option(t.getCause).getOrElse(t).toStringWithCauses))
      case t: LinkageError =>
        Left(Problem.pure(t.toStringWithCauses))
      case NonFatal(t) =>
        Left(Problem.pure(t.toStringWithCauses))
    }

  private val isAllowedConstructorParamameterClass = Set[Class[_]](
    classOf[InternalJob.JobContext])

  private def getConstructor(clas: Class[_ <: InternalJob]): Checked[Constructor[InternalJob]] =
    Checked.catchNonFatal {
      val constructors = clas
        .getConstructors.asInstanceOf[Array[Constructor[InternalJob]]]
        .filter(o => isPublic(o.getModifiers))
      constructors
        .find(_.getParameterTypes.forall(isAllowedConstructorParamameterClass))
        .toChecked(Problem.pure(
          s"Class '${clas.getName}' does not have an appropriate constructor (empty or InternalJob.Context)"))
    }.flatten

  private def construct(constructor: Constructor[InternalJob], jobContext: JobContext)
  : Checked[InternalJob] = {
    val args = constructor.getParameterTypes
      .map(cls =>
        if (cls isAssignableFrom classOf[InternalJob.JobContext])
          jobContext
        else
          sys.error(s"Unsupported constructor parameter: ${cls.getName}"))  // Should not happen
    try
      Right(constructor.newInstance(args: _*))
    catch {
      case t @ (_: InvocationTargetException | _: ExceptionInInitializerError) =>
        Left(Problem.fromThrowable(Option(t.getCause) getOrElse t))
      case NonFatal(t) =>
        Problem.fromThrowable(t)
    }
  }
}
