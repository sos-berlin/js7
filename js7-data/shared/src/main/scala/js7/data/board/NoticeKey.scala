package js7.data.board

import js7.base.annotation.javaApi
import js7.base.generic.GenericString
import js7.base.utils.ScalaUtils.syntax.RichEither

final case class NoticeKey private(string: String) extends GenericString:

  override def toString =
    if string.isEmpty then "NoticeKey.empty" else string


object NoticeKey extends GenericString.Checked_[NoticeKey]:

  val empty: NoticeKey = new NoticeKey("")

  protected def unchecked(string: String) =
    new NoticeKey(string)

  @javaApi
  @throws[RuntimeException]
  def of(string: String): NoticeKey =
    if string.isEmpty then
      empty
    else
      checked(string).orThrow

  given Ordering[NoticeKey] = GenericString.ordering
