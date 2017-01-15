package com.sos.scheduler.engine.data.filebaseds

import com.sos.scheduler.engine.base.sprayjson.SprayJson.implicits.RichJsValue
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
import spray.json.{JsString, JsValue, JsonFormat}

/**
  * @author Joacim Zschimmer
  */
object TypedPathRegister {

  private val FileBasedTypedToCompanion: Map[FileBasedType, TypedPath.AnyCompanion] = Map(
    FileBasedType.Folder → FolderPath,
    FileBasedType.Job → JobPath,
    FileBasedType.JobChain → JobChainPath,
    FileBasedType.Lock → LockPath,
    FileBasedType.Monitor → MonitorPath,
    FileBasedType.Order → OrderKey,
    FileBasedType.ProcessClass → ProcessClassPath,
    FileBasedType.Schedule → SchedulePath)

  private val Companions = FileBasedTypedToCompanion.values

  private val classToAnyCompanion: Map[Class[_ <: TypedPath], AnyCompanion] =
    (Companions map { o ⇒ o.typedPathClass → o }).toMap

  def fileBasedTypedToCompanion(t: FileBasedType): TypedPath.AnyCompanion =
    FileBasedTypedToCompanion(t)

  def classToCompanion[P <: TypedPath](c: Class[P]): Companion[P] =
    classToAnyCompanion(c).asInstanceOf[Companion[P]]

  val camelNameToCompanion: Map[String, AnyCompanion] =
    (Companions map { o ⇒ o.camelName → o }).toMap

  val lowerCaseCamelNameToCompanion: Map[String, AnyCompanion] =
    (Companions map { o ⇒ o.lowerCaseCamelName → o }).toMap

  private val cppNameToCompanion: String ⇒ AnyCompanion =
    (Companions map { o ⇒ o.cppName → o }).toMap

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
