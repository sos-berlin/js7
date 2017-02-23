package com.sos.scheduler.engine.jobapi.scripting

import com.google.common.base.Supplier
import com.google.common.collect.ImmutableMap
import com.sos.scheduler.engine.common.utils.JavaResource
import com.sos.scheduler.engine.jobapi.scripting.ScriptInstanceTest._
import org.scalatest.Matchers._
import org.scalatest.{FreeSpec, OneInstancePerTest}
import scala.collection.JavaConversions._
import scala.collection.mutable

final class ScriptInstanceTest extends FreeSpec with OneInstancePerTest {

  private val scriptLogger = new ScriptInstanceTest.JavaScriptLogger

  "testUnknownLanguage" in {
    intercept[RuntimeException] {
      newExecutor(Map(), "unknownLanguage", "a = 0")
    } .getMessage should include ("ECMAScript")  // Fehlermeldung listet bekannte Sprachen auf
  }

  "testEmptyScript" in {
    newExecutor(Map(), "javascript", "")
  }

  "testJavaScriptApi" in {
    val script = """
      |var cnt;
      |function spooler_init() {
      |   cnt = 0;
      |   return true;
      |}
      |
      |function spooler_process() {
      |  if (cnt < 5) {;
      |    cnt++;
      |    spooler_log.info(cnt);
      |    return true;
      |  }
      |  return false;
      |}""".stripMargin
    val executor = newExecutor(Map(), "javascript", script)
    executor.runLimited(5)
    assert(scriptLogger.lines == List("1", "2", "3", "4", "5"))
  }

  /** Spooler_process is the implicitly given function if no function body exists.
    * This behavior is similiar to the current behavior for using the scripting API */
  "javascriptWithoutFunctions" in {
    val msg = "Empty script ran successfully"
    val jobExecutor = newExecutor(Map(), "javascript", s"spooler_log.info('$msg');")
    jobExecutor.runLimited(5)
    assert(scriptLogger.lines == List(msg))
  }

  /** Executes a simple javascript.
    * Calls a javascript snippet and gives them some objects via the addObject method.
    *
    * The script does not contain any function, but calls the scheduler_process
    * method to cause the executing of the script. This is a special behavior
    * of the JobScheduler api: The execution of call("scheduler_process") is just the
    * same like call() if the function scheduler_process is not defined in the script.
    * http://www.mozilla.org/rhino/ */
  "javascriptWithObjects" in {
    val script = "function spooler_process() { spooler_log.info('Hello, my name is ' + name); return false }"
    val executor = newExecutor(Map("name" → "Walter"), "javascript", script)
    executor.runLimited(1)
    assert(scriptLogger.lines == List("Hello, my name is Walter"))
  }

  "javascriptWithoutReturnValue" in {
    val script = """
      |function spooler_init() { spooler_log.info('spooler_init') }
      |function spooler_exit() { spooler_log.info('spooler_exit') }
      |function spooler_open() { spooler_log.info('spooler_open') }
      |function spooler_close() { spooler_log.info('spooler_close') }
      |function spooler_process() { spooler_log.info('spooler_process') }
      |""".stripMargin
    val executor = newExecutor(Map(), "javascript", script)
    executor.runLimited(1)
    assert(scriptLogger.lines == List(
      "spooler_init", "spooler_open", "spooler_process", "spooler_close", "spooler_exit"))
  }

  /** Executes a simple groovy script,
    * Calls a groovy script and gives them some objects via the addObject method.
    * The script contains some funtions called by the call method. */
  "groovyScriptFromFile" in {
    runScript("groovy", groovyResource.asUTF8String)
  }

  /** Executes a simple java script.
    * Calls a java script and gives them some objects via the addObject method. */
  "javaScriptFromFile" in {
    runScript("javascript", javaScriptResource.asUTF8String)
  }

  private def runScript(language: String, script: String): Unit = {
    val executor = newExecutor(Map("name" → language), language, script)
    executor.runLimited(7)
    assert(scriptLogger.lines == List(
      s"spooler_init is called by $language",
      s"spooler_open is called by $language",
      s"spooler_process is called by $language",
      s"spooler_process is called by $language",
      s"spooler_process is called by $language",
      s"spooler_close is called by $language",
      s"spooler_exit is called by $language"))
  }

  private def newExecutor(bindings: Map[String, AnyRef], language: String, script: String) = {
    val completeBindings = Map("spooler_log" → scriptLogger) ++ bindings
    new TestExecutor(new JobScriptInstanceAdapter(
      language,
      new Supplier[ImmutableMap[String, AnyRef]] {
        def get() = ImmutableMap.copyOf(completeBindings)
      },
      script))
  }
}

private object ScriptInstanceTest {
  private val javaScriptResource = JavaResource.apply("com/sos/scheduler/engine/jobapi/scripting/test.js")
  private val groovyResource = JavaResource.apply("com/sos/scheduler/engine/jobapi/scripting/test.groovy")

  private final class JavaScriptLogger {
    val lines = mutable.Buffer[String]()
    def info(line: String) = lines += line
  }
}
