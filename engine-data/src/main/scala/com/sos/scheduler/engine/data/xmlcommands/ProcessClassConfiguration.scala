package com.sos.scheduler.engine.data.xmlcommands

import scala.collection.immutable

/**
 * &lt;process_class>.
 *
 * @author Joacim Zschimmer
 */
final case class ProcessClassConfiguration(
  processMaximum: Option[Int] = None,
  agentUris: immutable.Seq[String] = Nil,
  select: Option[String] = None)
extends XmlCommand {

  def xmlElem: xml.Elem = {
    val theOnlyAgent = if (agentUris.size == 1) Some(agentUris.head) else None  // Needed for JS-1301 (v1.9) and legacy TCP C++ Agent
    <process_class max_processes={(processMaximum map { _.toString}).orNull} remote_scheduler={theOnlyAgent.orNull}>{
      agentUris match {
        case Nil ⇒ xml.NodeSeq.Empty
        case Seq(_) ⇒ xml.NodeSeq.Empty
        case _ ⇒
          <remote_schedulers select={select.orNull}>{
            agentUris map { o ⇒ <remote_scheduler remote_scheduler={o}/> }
          }</remote_schedulers>
      }
    }</process_class>
  }
}
