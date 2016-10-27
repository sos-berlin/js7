package com.sos.scheduler.engine.data.filebaseds

import com.sos.scheduler.engine.base.sprayjson.SprayJson.implicits.RichJsValue
import com.sos.scheduler.engine.base.utils.ScalaUtils
import com.sos.scheduler.engine.base.utils.ScalaUtils.implicitClass
import com.sos.scheduler.engine.data.filebased.TypedPath._
import com.sos.scheduler.engine.data.filebased.{FileBasedType, TypedPath}
import com.sos.scheduler.engine.data.folder.FolderPath
import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.jobchain.JobChainPath
import com.sos.scheduler.engine.data.lock.LockPath
import com.sos.scheduler.engine.data.monitor.MonitorPath
import com.sos.scheduler.engine.data.order.OrderKey
import com.sos.scheduler.engine.data.processclass.ProcessClassPath
import com.sos.scheduler.engine.data.schedule.SchedulePath
import scala.reflect.ClassTag
import spray.json.{JsString, JsValue, JsonFormat}

/**
  * @author Joacim Zschimmer
  */
object TypedPathRegister {

  private val TypedPathCompanions: Set[TypedPath.AnyCompanion] = Set(
    FolderPath,
    JobPath,
    JobChainPath,
    LockPath,
    MonitorPath,
    OrderKey,
    ProcessClassPath,
    SchedulePath)

  val fileBasedTypedToCompanion: FileBasedType ⇒ AnyCompanion =
    (TypedPathCompanions map { o ⇒ o.fileBasedType → o }).toMap

  private val classToAnyCompanion: Map[Class[_ <: TypedPath], AnyCompanion] =
    (TypedPathCompanions map { o ⇒ o.typedPathClass → o }).toMap

  def classToCompanion[P <: TypedPath](c: Class[P]): Companion[P] =
    classToAnyCompanion(c).asInstanceOf[Companion[P]]

//  def companion[P <: TypedPath: ClassTag]: Companion[P] =
//    classToCompanion(implicitClass[P])

  private val camelNameToCompanion: String ⇒ AnyCompanion =
    (TypedPathCompanions map { o ⇒ o.fileBasedType.camelName → o }).toMap

  private val cppNameToCompanion: String ⇒ AnyCompanion =
    (TypedPathCompanions map { o ⇒ o.fileBasedType.cppName → o }).toMap

  private def splitTypeAndPath(typeAndPath: String): (String, String) = {
    typeAndPath indexOf ":" match {
      case -1 | 0 ⇒ throw new IllegalArgumentException(s"Missing type prefix for TypedPath '$typeAndPath'")
      case i ⇒
        val typeName = typeAndPath.substring(0, i)
        val path = typeAndPath.substring(i + 1)
        (typeName, path)
    }
  }

  private[filebaseds] def fromTypedString(typeAndPath: String): TypedPath = {
    val (typeName, path) = splitTypeAndPath(typeAndPath)
    camelNameToCompanion(typeName).apply(path)
  }

  private[engine] def fromCppTypedString(typeAndPath: String): TypedPath = {
    val (typeName, path) = splitTypeAndPath(typeAndPath)
    cppNameToCompanion(typeName).apply(path)
  }

  val WithCompanionJsonFormat: JsonFormat[TypedPath] =
    new JsonFormat[TypedPath] {
      def write(o: TypedPath) = JsString(o.toTypedString)
      def read(json: JsValue) = fromTypedString(json.asString)
    }
}
