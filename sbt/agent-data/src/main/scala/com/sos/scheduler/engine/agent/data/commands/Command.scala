package com.sos.scheduler.engine.agent.data.commands

import com.sos.scheduler.engine.base.sprayjson.typed.{Subtype, TypedJsonFormat}

/**
 * @author Joacim Zschimmer
 */
trait Command {
  type Response <: com.sos.scheduler.engine.agent.data.commandresponses.Response

  def toShortString = toString

  /**
   * true if toString returns a longer string than toShortString.
   */
  def toStringIsLonger = false
}

object Command {

  implicit val MyJsonFormat = TypedJsonFormat[Command](typeField = "$TYPE", shortenTypeOnlyValue = false)(
    Subtype[CloseTask](CloseTask.SerialTypeName),
    Subtype[DeleteFile](DeleteFile.SerialTypeName),
    Subtype[MoveFile](MoveFile.SerialTypeName),
    Subtype[Login.type](Login.SerialTypeName),
    Subtype[Logout.type](Logout.SerialTypeName),
    Subtype[NoOperation.type](NoOperation.SerialTypeName),
    Subtype[RequestFileOrderSourceContent](RequestFileOrderSourceContent.SerialTypeName),
    Subtype[SendProcessSignal](SendProcessSignal.SerialTypeName),
    Subtype[StartApiTask](StartApiTask.SerialTypeName),
    Subtype[StartNonApiTask](StartNonApiTask.SerialTypeName),
    Subtype[Terminate](Terminate.SerialTypeName),
    Subtype[AbortImmediately.type](AbortImmediately.SerialTypeName))
}
