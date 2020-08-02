package js7.proxy.javaapi.data

import js7.base.annotation.javaApi
import js7.data.filebased.{FileBased, TypedPath}

@javaApi
trait JFileBased[A <: JFileBased[A, P], P <: TypedPath]
extends JJsonable[A]
{
  protected type Underlying <: FileBased

  def id: JItemId[P]
}
