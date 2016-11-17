package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.base.utils.ScalaUtils.implicitClass
import com.sos.scheduler.engine.data.filebased.TypedPath._
import com.sos.scheduler.engine.data.filebaseds.TypedPathRegister
import java.io.File
import scala.reflect.ClassTag
import spray.json.JsonFormat

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
  implicit def ordering[A <: TypedPath]: Ordering[A] =
    new Ordering[A] {
      def compare(a: A, b: A) = a.string compare b.string
    }

  implicit val jsonFormat: JsonFormat[TypedPath] = TypedPathRegister.WithCompanionJsonFormat

  val extensions: Set[String] = FileBasedTypes.forFiles map { _.filenameExtension }

  type AnyCompanion = Companion[_ <: TypedPath]

  abstract class Companion[A <: TypedPath: ClassTag] extends AbsolutePath.Companion[A] {
    implicit val implicitCompanion = this

    def fileBasedType: FileBasedType

    def typedPathClass: Class[A] = implicitClass[A]
  }
}
