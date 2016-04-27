package com.sos.scheduler.engine.taskserver.module

import com.sos.scheduler.engine.taskserver.module.ModuleRegister.UnsupportedRawModuleArgumentsException
import com.sos.scheduler.engine.taskserver.module.ModuleRegisterTest.{AModule, _}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class ModuleRegisterTest extends FreeSpec {

  "Non matching" in {
    val raw = RawModuleArguments(TestModuleLanguage, None, Script("ALIEN"), None, None)
    intercept[UnsupportedRawModuleArgumentsException] { TestModuleRegister.moduleType(raw) }
  }

  "Matching AModule" in {
    val script = Script("TEST-A")
    val raw = RawModuleArguments(TestModuleLanguage, None, script, None, None)
    val args = AModule.Arguments(script)
    assert(TestModuleRegister.moduleType(raw) == AModule)
    assert(TestModuleRegister.toModuleArguments(raw) == args)
    assert(TestModuleRegister.newModule(raw) == AModule(args))
  }

  "Matching BModule" in {
    val script = Script("TEST-B")
    val raw = RawModuleArguments(TestModuleLanguage, None, script, None, None)
    val args = BModule.Arguments(script)
    assert(TestModuleRegister.moduleType(raw) == BModule)
    assert(TestModuleRegister.toModuleArguments(raw) == args)
    assert(TestModuleRegister.newModule(raw) == BModule(args))
  }
}

object ModuleRegisterTest {
  private object TestModuleLanguage extends ModuleLanguage {
    def string = "TEST"
  }

  private case class AModule(arguments: AModule.Arguments) extends Module
  private case class BModule(arguments: BModule.Arguments) extends Module

  private object AModule extends ModuleType {
    def toModuleArguments = {
      case RawModuleArguments(TestModuleLanguage, None, script, None, None) if script.string startsWith "TEST-A" ⇒ Arguments(script)
    }

    def newModule(arguments: ModuleArguments) = AModule(arguments.asInstanceOf[Arguments])

    final case class Arguments(script: Script) extends ModuleArguments {
      override def moduleType = AModule
    }
  }

  private object BModule extends ModuleType {
    def toModuleArguments = {
      case RawModuleArguments(TestModuleLanguage, None, script, None, None) if script.string startsWith "TEST-B" ⇒ Arguments(script)
    }

    def newModule(arguments: ModuleArguments) = BModule(arguments.asInstanceOf[Arguments])

    final case class Arguments(script: Script) extends ModuleArguments {
      override def moduleType = BModule
    }
  }

  private val TestModuleRegister = new ModuleRegister(List(AModule, BModule))
}
