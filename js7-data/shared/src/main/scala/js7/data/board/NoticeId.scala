package js7.data.board

import js7.base.generic.GenericString

final case class NoticeId(string: String)
extends GenericString

object NoticeId extends GenericString.NonEmpty[NoticeId]
{
  protected def unchecked(string: String) =
    new NoticeId(string)
}
