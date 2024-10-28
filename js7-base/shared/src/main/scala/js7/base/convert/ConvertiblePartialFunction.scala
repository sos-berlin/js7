package js7.base.convert

import js7.base.convert.ConvertiblePartialFunction.*
import js7.base.convert.ConvertiblePartialFunctions.wrappedConvert
import js7.base.problem.Checked.*
import js7.base.problem.{Checked, Problem}

/**
  * Provides methods for convertion of the result of a PartialFunction (for example a Map).
  *
  * @author Joacim Zschimmer
  */
trait ConvertiblePartialFunction[K, V] extends PartialFunction[K, V]:

  def as[W](key: K)(implicit convert: As[V, W]): W =
    wrappedConvert(convert.apply, renderKey(key))(apply(key))

  def as[W](key: K, default: => W)(implicit convert: As[V, W]): W =
    optionAs[W](key) getOrElse default

  def checkedAs[W](key: K)(implicit convert: As[V, W]): Checked[W] =
    checkedAs[W](key, None)

  def checkedAs[W](key: K, default: => Option[W])(implicit convert: As[V, W]): Checked[W] =
    optionAs[W](key, default).toChecked(MissingConfigurationKeyProblem(key.toString))

  def checkedOptionAs[W](key: K)(implicit convert: As[V, W]): Checked[Option[W]] =
    Right(lift(key) map wrappedConvert(convert.apply, renderKey(key)))

  def optionAs[W](key: K, default: => Option[W])(implicit convert: As[V, W]): Option[W] =
    optionAs(key)(convert) orElse default

  def optionAs[W](key: K)(implicit convert: As[V, W]): Option[W] =
    lift(key) map wrappedConvert(convert.apply, renderKey(key))

  protected def renderKey(key: K) = s"key '$key'"


object ConvertiblePartialFunction:
  final case class MissingConfigurationKeyProblem(key: String) extends Problem.Coded:
    def arguments: Map[String, String] =
      Map(
        "key" -> key)
