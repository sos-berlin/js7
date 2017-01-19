package com.sos.scheduler.engine.common.guice

import com.google.inject.Scopes.SINGLETON
import com.google.inject.{AbstractModule, Provider}
import com.sos.scheduler.engine.base.utils.ScalaUtils.implicitClass
import scala.reflect.ClassTag

abstract class ScalaAbstractModule extends AbstractModule {

  private lazy val myBinder = binder skipSources classOf[ScalaAbstractModule]

  final def bindInstance[A <: AnyRef : ClassTag](instance: A) =
    bindClass[A] toInstance instance

  final def bindClass[A <: AnyRef : ClassTag] =
    myBinder bind implicitClass[A]

  final def provideSingleton[A <: AnyRef : ClassTag](provider: ⇒ A) =
    provide[A](provider) in SINGLETON

  private def provide[A <: AnyRef : ClassTag](provider: ⇒ A) =
    myBinder bind implicitClass[A] toProvider new Provider[A] { def get = provider }
}
