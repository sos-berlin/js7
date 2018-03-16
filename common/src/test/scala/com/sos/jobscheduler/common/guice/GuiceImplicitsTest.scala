package com.sos.jobscheduler.common.guice

import com.google.inject.{AbstractModule, Guice, Provides}
import com.sos.jobscheduler.common.guice.GuiceImplicits._
import javax.inject.Singleton
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class GuiceImplicitsTest extends FreeSpec {

  private val injector = Guice.createInjector(new AbstractModule {
    @Provides @Singleton
    def long(): Long = 7
  })

  ".instance" in {
    assert(injector.instance[Long] == 7)
    intercept[com.google.inject.ConfigurationException] {
      injector.instance[Integer]
    }
  }

  ".instanceOption" in {
    assert(injector.option[Long] == Some(7))
    assert(injector.option[Integer] == None)
  }
}
