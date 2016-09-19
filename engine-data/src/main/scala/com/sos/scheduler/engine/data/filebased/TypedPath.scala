package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.base.generic.IsString
import com.sos.scheduler.engine.base.sprayjson.SprayJson.implicits._
import com.sos.scheduler.engine.data.filebased.TypedPath._
import java.io.File
import spray.json.{JsString, JsValue, JsonFormat}

trait TypedPath
extends AbsolutePath {

  def companion: Companion[_ <: TypedPath]

  def fileBasedType: FileBasedType = companion.fileBasedType

  def file(baseDirectory: File): File =
    new File(baseDirectory, relativeFilePath)

  /** @return Relativer Pfad mit Schrägstrich beginnend. Großschreibung kann bei manchen Typen abweichen. */
  def relativeFilePath: String =
    if (fileBasedType eq FileBasedType.Folder) string + "/"
    else string + "." + fileBasedType.cppName + ".xml"

  def asTyped[A <: TypedPath: TypedPath.Companion]: A = {
    val c = implicitly[TypedPath.Companion[A]]
    if (c == companion)
      this.asInstanceOf[A]
    else
      c.apply(string)
  }

  override def toString = toTypedString

  def toTypedString: String = s"${fileBasedType.camelName}:$string"
}


object TypedPath {
  def fromTypedString(typeAndPath: String): TypedPath = {
    val (typeName, path) = splitTypeAndPath(typeAndPath)
    FileBasedType.fromCamelName(typeName).companion()(path)
  }

  private[engine] def fromCppTypedString(typeAndPath: String): TypedPath = {
    val (typeName, path) = splitTypeAndPath(typeAndPath)
    FileBasedType.fromCppName(typeName).companion()(path)
  }

  private def splitTypeAndPath(typeAndPath: String): (String, String) = {
    typeAndPath indexOf ":" match {
      case -1 | 0 ⇒ throw new IllegalArgumentException(s"Missing type prefix for TypedPath '$typeAndPath'")
      case i ⇒
        val typeName = typeAndPath.substring(0, i)
        val path = typeAndPath.substring(i + 1)
        (typeName, path)
    }
  }

  implicit def ordering[A <: TypedPath]: Ordering[A] =
    new Ordering[A] {
      def compare(a: A, b: A) = a.string compare b.string
    }

  implicit val MyJsonFormat = new IsString.MyJsonFormat[TypedPath](UnknownTypedPath.apply)

  val WithCompanionJsonFormat: JsonFormat[TypedPath] =
    new JsonFormat[TypedPath] {
      def write(o: TypedPath) = JsString(o.toTypedString)
      def read(json: JsValue) = fromTypedString(json.asJsString.value)
    }

  val extensions: Set[String] = FileBasedTypes.forFiles map { _.filenameExtension }

  type AnyCompanion = Companion[_ <: TypedPath]

  trait Companion[A <: TypedPath] extends AbsolutePath.Companion[A] {
    implicit val implicitCompanion = this

    def fileBasedType: FileBasedType
  }
}
