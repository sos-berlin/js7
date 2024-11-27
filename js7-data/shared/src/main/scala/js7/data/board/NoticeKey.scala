package js7.data.board

import js7.base.generic.GenericString

final case class NoticeKey private(string: String) extends GenericString


object NoticeKey extends GenericString.NonEmpty[NoticeKey]:

  val empty: NoticeKey = new NoticeKey("")

  protected def unchecked(string: String) =
    new NoticeKey(string)