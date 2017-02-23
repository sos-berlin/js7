package com.sos.jobscheduler.agent.data.commands

import com.sos.jobscheduler.base.sprayjson.typed.{Subtype, TypedJsonFormat}

/**
 * @author Joacim Zschimmer
 */
trait Command {
  type Response <: com.sos.jobscheduler.agent.data.commandresponses.Response

  def toShortString = toString

  /**
   * true if toString returns a longer string than toShortString.
   */
  def toStringIsLonger = false
}

object Command {

  implicit val MyJsonFormat = TypedJsonFormat[Command](typeField = "$TYPE", shortenTypeOnlyValue = false)(
    Subtype[AbortImmediately.type](AbortImmediately.SerialTypeName),
    Subtype[CloseTask](CloseTask.SerialTypeName),
    Subtype[DeleteFile](DeleteFile.SerialTypeName),
    Subtype[MoveFile](MoveFile.SerialTypeName),
    Subtype[DetachOrder](DetachOrder.SerialTypeName),
    Subtype[Login.type](Login.SerialTypeName),
    Subtype[Logout.type](Logout.SerialTypeName),
    Subtype[NoOperation.type](NoOperation.SerialTypeName),
    Subtype[RequestFileOrderSourceContent](RequestFileOrderSourceContent.SerialTypeName),
    Subtype[RegisterAsMaster.type](RegisterAsMaster.SerialTypeName),
    Subtype[SendProcessSignal](SendProcessSignal.SerialTypeName),
    Subtype[StartApiTask](StartApiTask.SerialTypeName),
    Subtype[StartNonApiTask](StartNonApiTask.SerialTypeName),
    Subtype[Terminate](Terminate.SerialTypeName),
    Subtype[AbortImmediately.type](AbortImmediately.SerialTypeName),
    Subtype[AddJobNet](AddJobNet.SerialTypeName),
    Subtype[AddOrder](AddOrder.SerialTypeName))
}
