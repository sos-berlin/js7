package com.sos.scheduler.engine.minicom.remoting.calls

/**
 * @author Joacim Zschimmer
 */
private[remoting] object MessageClass {
  //val None = 'N'.toByte     // Nachricht bezieht sich auf keine Session oder Objekt
  val Session = 'S'.toByte  // Nachricht bezieht sich auf eine Session (bezeichnet durch die folgenden Bytes)
  val Object = 'O'.toByte   // Nachricht bezieht sich auf ein Objekt (bezeichnet durch die folgenden Bytes)
  val Answer = 'A'.toByte   // Nachricht ist eine Antwort
  val Error = 'E'.toByte    // Nachricht ist eine Fehlerantwort
  val KeepAlive = 'K'.toByte  // Keep-alive message to keep HTTP connection up and to detect master's connection loss

  val isCall = Set(Session, Object)  // Ignoring KeepAlive
  val isResult = Set(Answer, Error)
}
