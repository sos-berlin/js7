package js7.data.board

import javax.annotation.Nonnull
import js7.base.generic.GenericString
import js7.base.utils.ScalaUtils.syntax.*

final case class NoticeId(string: String)
extends GenericString
{
  override def toString = s"NoticeId:$string"
}

object NoticeId extends GenericString.NonEmpty[NoticeId]
{
  protected def unchecked(string: String) =
    new NoticeId(string)

  @Nonnull
  def of(string: String): NoticeId =
    checked(string).orThrow
}
