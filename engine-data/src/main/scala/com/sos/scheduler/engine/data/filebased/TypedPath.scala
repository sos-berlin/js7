package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.base.generic.IsString
import com.sos.scheduler.engine.data.filebased.TypedPath._
import java.io.File

trait TypedPath
extends AbsolutePath {

  def companion: Companion[_ <: TypedPath]

  def fileBasedType: FileBasedType = companion.fileBasedType

  def file(baseDirectory: File): File =
    new File(baseDirectory, relativeFilePath)

  /** @return Relativer Pfad mit Schrägstrich beginnend. Großschreibung kann bei manchen Typen abweichen. */
  def relativeFilePath: String =
    if (fileBasedType eq FileBasedType.folder) string + "/"
    else string + "." + fileBasedType.cppName + ".xml"

  override def toString =
    s"$fileBasedType ${super.toString}"
}


object TypedPath {
  implicit def ordering[A <: TypedPath]: Ordering[A] =
    new Ordering[A] {
      def compare(a: A, b: A) = a.string compare b.string
    }

  implicit val MyJsonFormat = new IsString.MyJsonFormat[TypedPath](IsString.UnsupportedJsonDeserialization)

  val extensions: Set[String] = FileBasedTypes.forFiles map { _.filenameExtension }

  type AnyCompanion = Companion[_ <: TypedPath]

  trait Companion[A <: TypedPath] extends AbsolutePath.Companion[A] {
    implicit val implicitCompanion = this

    def fileBasedType: FileBasedType
  }
}
