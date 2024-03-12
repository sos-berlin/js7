package js7.launcher.forjava.internal

import cats.effect.IO
import io.vavr.control.Either as VEither
import izumi.reflect.Tag
import java.lang.reflect.Modifier.isPublic
import java.lang.reflect.{Constructor, InvocationTargetException}
import js7.base.catsutils.CatsEffectExtensions.{blockingOn, catchIntoChecked}
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.implicitClass
import js7.base.utils.ScalaUtils.syntax.{RichEither, RichEitherF}
import js7.base.utils.SetOnce
import js7.data.order.Outcome
import js7.data_for_java.vavr.VavrConverters.*
import js7.launcher.OrderProcess
import scala.concurrent.ExecutionContext
import scala.reflect.ClassTag
import scala.util.control.NonFatal

private[internal] final class InternalJobAdapterHelper[J: ClassTag: Tag](
  blockingJobEC: ExecutionContext):

  private val checkedJobOnce = SetOnce[Checked[J]]  // SetOnce for late arriving Scheduler

  def callStart(jJobContext: JavaJobContext, call: J => IO[VEither[Problem, Void]]): IO[Checked[Unit]] =
    IO.blockingOn(blockingJobEC):
      instantiate(jJobContext)
    .flatMapT: jInternalJob =>
      call(jInternalJob)
        .map(_.toScala.rightAs(()))
        .catchIntoChecked
        .flatTap: checked =>
          IO:
            checkedJobOnce := checked.map(_ => jInternalJob)
            checked

  def callStop(call: J => IO[Unit]): IO[Unit] =
    IO.defer:
      checkedJobOnce.toOption.fold(IO.unit)(checked =>
        call(checked.orThrow))

  def callProcessOrder(call: J => OrderProcess): OrderProcess =
    checkedJobOnce.checked.flatten match
      case Left(problem) =>
        OrderProcess(IO.pure(Outcome.Failed.fromProblem(problem)))

      case Right(jInternalJob) =>
        call(jInternalJob)

  private def instantiate(jJobContext: JavaJobContext): Checked[J] =
    val cls = jJobContext.asScala.implementationClass
    if !implicitClass[J].isAssignableFrom(cls) then
      Left(Problem.pure(s"Class '${cls.getName}' must be a ${implicitClass[J].getName}"))
    else
      getConstructor(cls.asInstanceOf[Class[? <: J]], jJobContext.getClass)
        .flatMap(construct(_, jJobContext))

  private def getConstructor(clas: Class[? <: J], jJobContextClass: Class[? <: JavaJobContext])
  : Checked[Constructor[J]] =
    catchNonFatal {
      val constructors = clas
        .getConstructors.asInstanceOf[Array[Constructor[J]]]
        .filter(o => isPublic(o.getModifiers))
      constructors
        .find(_.getParameterTypes sameElements Array(jJobContextClass))
        .orElse(constructors.find(_.getParameterTypes.isEmpty))
        .toChecked(Problem.pure(
          s"Class '${clas.getName}' does not have an appropriate public constructor (empty or BlockingInternalJob.JobContext)"))
    }.flatten

  private def construct(constructor: Constructor[J], jobContext: JavaJobContext): Checked[J] =
    try
      Right(constructor.getParameterCount match {
        case 0 => constructor.newInstance()
        case 1 => constructor.newInstance(jobContext)
      })
    catch
      case t @ (_: InvocationTargetException | _: ExceptionInInitializerError) =>
        Left(Problem.fromThrowable(Option(t.getCause) getOrElse t))
      case NonFatal(t) =>
        Problem.fromThrowable(t)
