package com.sos.scheduler.engine.taskserver.moduleapi

import com.sos.scheduler.engine.taskserver.moduleapi.ModuleFactoryRegister.UnsupportedRawModuleArgumentsException
import com.sos.scheduler.engine.taskserver.moduleapi.ModuleFactoryRegisterTest._
import org.scalatest.FreeSpec

/**
  * @author Joacim Zschimmer
  */
final class ModuleFactoryRegisterTest extends FreeSpec {

  "Non matching" in {
    val raw = RawModuleArguments(TestModuleLanguage, None, Script("ALIEN"), None, None)
    intercept[UnsupportedRawModuleArgumentsException] { TestRegister.moduleFactory(raw) }
  }

  "Matching AModule" in {
    val script = Script("TEST-A")
    val raw = RawModuleArguments(TestModuleLanguage, None, script, None, None)
    val args = AModule.Arguments(script)
    assert(TestRegister.moduleFactory(raw) == AModule)
    assert(TestRegister.toModuleArguments(raw) == args)
    assert(TestRegister.newModule(raw) == AModule(args))
  }

  "Matching BModule" in {
    val script = Script("TEST-B")
    val raw = RawModuleArguments(TestModuleLanguage, None, script, None, None)
    val args = BModule.Arguments(script)
    assert(TestRegister.moduleFactory(raw) == BModule)
    assert(TestRegister.toModuleArguments(raw) == args)
    assert(TestRegister.newModule(raw) == BModule(args))
  }
}

private object ModuleFactoryRegisterTest {
  private val TestModuleLanguage = ModuleLanguage("test")

  private case class AModule(arguments: AModule.Arguments) extends Module

  private object AModule extends ModuleFactory {
    def toModuleArguments = {
      case RawModuleArguments(TestModuleLanguage, None, script, None, None) if script.string startsWith "TEST-A" ⇒ Arguments(script)
    }

    def newModule(arguments: ModuleArguments) = AModule(arguments.asInstanceOf[Arguments])

    final case class Arguments(script: Script) extends ModuleArguments {
      override def moduleFactory = AModule
    }
  }

  private case class BModule(arguments: BModule.Arguments) extends Module

  private object BModule extends ModuleFactory {
    def toModuleArguments = {
      case RawModuleArguments(TestModuleLanguage, None, script, None, None) if script.string startsWith "TEST-B" ⇒ Arguments(script)
    }

    def newModule(arguments: ModuleArguments) = BModule(arguments.asInstanceOf[Arguments])

    final case class Arguments(script: Script) extends ModuleArguments {
      override def moduleFactory = BModule
    }
  }

  private val TestRegister = new ModuleFactoryRegister(List(AModule, BModule))
}
