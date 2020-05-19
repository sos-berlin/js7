package com.sos.jobscheduler.proxy.javaapi.data

import com.sos.jobscheduler.base.annotation.javaApi
import com.sos.jobscheduler.data.filebased.{FileBased, TypedPath}

@javaApi
trait JFileBased[A <: JFileBased[A, P], P <: TypedPath]
extends JJsonable[A]
{
  protected type Underlying <: FileBased

  def id: JFileBasedId[P]
}
