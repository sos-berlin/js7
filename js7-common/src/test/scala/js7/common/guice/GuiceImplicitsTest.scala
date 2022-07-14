package js7.common.guice

import com.google.inject.{AbstractModule, Guice, Provides}
import javax.inject.Singleton
import js7.common.guice.GuiceImplicits.*
import org.scalatest.freespec.AnyFreeSpec

/**
  * @author Joacim Zschimmer
  */
final class GuiceImplicitsTest extends AnyFreeSpec {

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
