package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.base.IsString
import java.io.File

trait TypedPath
extends AbsolutePath {

  def fileBasedType: FileBasedType

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
  implicit def ordering[A <: TypedPath]: Ordering[A] with Object {def compare(a: A, b: A): Int} = new Ordering[A] {
    def compare(a: A, b: A) = a.string compare b.string
  }

  implicit val MyJsonFormat = new IsString.MyJsonFormat[TypedPath](IsString.UnsupportedJsonDeserialization)
}
