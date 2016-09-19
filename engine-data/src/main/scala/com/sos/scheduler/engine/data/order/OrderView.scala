package com.sos.scheduler.engine.data.order

import com.sos.scheduler.engine.data.filebased.HasPath
import com.sos.scheduler.engine.data.jobchain.NodeKey
import com.sos.scheduler.engine.data.scheduler.ClusterMemberId
import spray.json._

/**
  * @author Joacim Zschimmer
  */
trait OrderView extends HasPath {
  def orderKey: OrderKey

  private[engine] def occupyingClusterMemberId: Option[ClusterMemberId]

  private[engine] def processingState: OrderProcessingState

  private[engine] def nodeKey: NodeKey

  final def processingStateClass = processingState.getClass
}

object OrderView {
  trait Companion[V <: OrderView] {
    implicit def jsonFormat: RootJsonFormat[V]

    implicit final def implicitCompanion: Companion[V] = this

    val name = getClass.getSimpleName stripSuffix "$"
  }

//  object Companion {
//    import scala.PartialFunction.condOpt
//    def option(name: String): Option[Companion[_ <: OrderView]] =
//      condOpt(name) {
//        case "OrderOverview" ⇒ OrderOverview
//        case "OrderDetailed" ⇒ OrderDetailed
//      }
//  }

  implicit def jsonFormat[V <: OrderView: OrderView.Companion] = implicitly[OrderView.Companion[V]].jsonFormat

//  implicit val MyJsonWriter: RootJsonWriter[OrderView] = new RootJsonWriter[OrderView] {
//    def write(o: OrderView) = o match {
//      case o: OrderDetailed ⇒ o.toJson
//      case o: OrderOverview ⇒ o.toJson
//    }
//  }
}
