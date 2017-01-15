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

  def file(baseDirectory: File): File =
    new File(baseDirectory, relativeFilePath)

  /** @return Relativer Pfad mit Schrägstrich beginnend. Großschreibung kann bei manchen Typen abweichen. */
  final def relativeFilePath: String =
    string + companion.filenameExtension

  def asTyped[A <: TypedPath: TypedPath.Companion]: A = {
    val c = implicitly[TypedPath.Companion[A]]
    if (c == companion)
      this.asInstanceOf[A]
    else
      c.apply(string)
  }

  override def toString = toTypedString

  def toTypedString: String = s"${companion.camelName}:$string"
}

object TypedPath {
  implicit def ordering[A <: TypedPath]: Ordering[A] =
    new Ordering[A] {
      def compare(a: A, b: A) = a.string compare b.string
    }

  implicit val jsonFormat: JsonFormat[TypedPath] = TypedPathRegister.WithCompanionJsonFormat

  type AnyCompanion = Companion[_ <: TypedPath]

  abstract class Companion[A <: TypedPath: ClassTag] extends AbsolutePath.Companion[A] {
    implicit val implicitCompanion: Companion[A] = this

    def typedPathClass: Class[A] = implicitClass[A]

    val camelName: String = name stripSuffix "Path"
    final lazy val lowerCaseCamelName = camelName.substring(0, 1).toLowerCase + camelName.substring(1)
    private[engine] final lazy val cppName: String = lowerCaseCamelName map { c ⇒ if (c.isUpper) "_" + c.toLower else c } mkString ""
    lazy val filenameExtension: String = s".$cppName.xml"
  }
}
