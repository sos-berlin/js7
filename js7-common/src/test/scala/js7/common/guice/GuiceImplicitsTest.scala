package js7.common.guice

import com.google.inject.{AbstractModule, Guice, Provides}
import javax.inject.Singleton
import js7.base.test.OurTestSuite
import js7.common.guice.GuiceImplicits.*

/**
  * @author Joacim Zschimmer
  */
final class GuiceImplicitsTest extends OurTestSuite {

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
