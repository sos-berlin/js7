package js7.data.workflow.instructions

import io.circe.Codec
import js7.base.circeutils.CirceUtils.enumCodec
import js7.data.board.BoardPathExpression

trait ExpectOrConsumeNoticesInstruction extends NoticeInstruction:

  def boardPaths: BoardPathExpression




object ExpectOrConsumeNoticesInstruction:

  /** How to handle not announced PlannedNoticeKey in ConsumeNotices and ExpectedNotices instructions. */
  enum WhenNotAnnounced:
    /** When required NoticeIds have not been announced, wait nevertheless. */
    case Wait

    /** When required NoticeIds have not been announced, skip the instruction. */
    case SkipWhenNoNotice

    /** When required NoticeIds are ignored (regarded as existent).
      * ConsumeNotices consumes only existent NoticeIds. */
    case DontWait

  object WhenNotAnnounced:
    given Codec[WhenNotAnnounced] = enumCodec(valueOf)
