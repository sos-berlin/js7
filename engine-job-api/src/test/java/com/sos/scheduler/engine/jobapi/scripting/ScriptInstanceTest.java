package com.sos.scheduler.engine.jobapi.scripting;

import com.google.common.base.Charsets;
import com.google.common.base.Supplier;
import com.google.common.collect.ImmutableMap;
import com.google.common.io.Resources;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.junit.Test;

import static com.google.common.base.Throwables.propagate;
import static com.google.common.io.Resources.getResource;
import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public final class ScriptInstanceTest {
    private static final String javaScriptResourcePath = "com/sos/scheduler/engine/jobapi/scripting/test.js";
    private static final String groovyResourcePath = "com/sos/scheduler/engine/jobapi/scripting/test.groovy";
    private static final ImmutableMap<String, Object> emptyBindings = ImmutableMap.of();

    private final JavaScriptLogger scriptLogger = new JavaScriptLogger();

    @Test public void testUnknownLanguage() {
        try {
            newExecutor(emptyBindings, "unknownLanguage", "a = 0");
            fail("Exception for unknown language expected");
        } catch (RuntimeException e) {
            assertThat(e.getMessage(), containsString("ECMAScript"));   // Fehlermeldung listet bekannte Sprachen auf
        }
    }

    @Test public void testEmptyScript() {
        newExecutor(emptyBindings, "javascript", "");
    }

    @Test public void testJavaScriptApi() throws Exception {
		String script = "var cnt;\n"
			+ "function spooler_init() {\n"
			+ "   cnt = 0;\n"
			+ "   return true;\n"
			+ "}"
			+ " "
			+ "function spooler_process() {\n"
			+ "  if (cnt < 5) {;\n"
			+ "    cnt++;\n"
			+ "    spooler_log.info(cnt);\n"
			+ "    return true;\n"
			+ "  }"
			+ "  return false;\n"
			+ "}";
		TestExecutor executor = newExecutor(emptyBindings, "javascript", script);
        executor.runLimited(5);
        assertEquals(scriptLogger.lines, asList("1", "2", "3", "4", "5"));
	}

	/** Spooler_process is the implicitly given function if no function body exists.
     * This behavior is similiar to the current behavior for using the scripting API */
	@Test
	public void javascriptWithoutFunctions() throws Exception {
        String msg = "Empty script ran successfully";
        TestExecutor jobExecutor = newExecutor(emptyBindings, "javascript", "spooler_log.info('"+msg+"');");
        jobExecutor.runLimited(5);
        assertEquals(asList(msg), scriptLogger.lines);
	}

	/** Executes a simple javascript.
	 * Calls a javascript snippet and gives them some objects via the addObject method.
	 *
	 * The script does not contain any function, but calls the scheduler_process
	 * method to cause the executing of the script. This is a special behavior
	 * of the JobScheduler api: The execution of call("scheduler_process") is just the
	 * same like call() if the function scheduler_process is not defined in the script.
	 * http://www.mozilla.org/rhino/ */
	@Test
	public void javascriptWithObjects() throws Exception {
		String script =	"function spooler_process() { spooler_log.info('Hello, my name is ' + name); return false }";
		TestExecutor executor = newExecutor(ImmutableMap.<String, Object>of("name", "Walter"), "javascript", script);
		executor.runLimited(1);
        assertEquals(asList("Hello, my name is Walter"), scriptLogger.lines);
	}

    @Test
    public void javascriptWithoutReturnValue() throws Exception {
        String script =
                "function spooler_init() { spooler_log.info('spooler_init') }\n"+
                "function spooler_exit() { spooler_log.info('spooler_exit') }\n"+
                "function spooler_open() { spooler_log.info('spooler_open') }\n"+
                "function spooler_close() { spooler_log.info('spooler_close') }\n"+
                "function spooler_process() { spooler_log.info('spooler_process') }\n";
        TestExecutor executor = newExecutor(emptyBindings, "javascript", script);
        executor.runLimited(1);
        assertEquals(asList("spooler_init", "spooler_open", "spooler_process", "spooler_close", "spooler_exit"), scriptLogger.lines);
    }

    /** Executes a simple groovy script,
     * Calls a groovy script and gives them some objects via the addObject method.
     * The script contains some funtions called by the call method. */
    @Test
	public void groovyScriptFromFile() throws Exception {
        runScript("groovy", resourceString(groovyResourcePath));
	}

    /** Executes a simple java script.
     * Calls a java script and gives them some objects via the addObject method. */
    @Test
    public void javaScriptFromFile() throws Exception {
        runScript("javascript", resourceString(javaScriptResourcePath));
    }

    private void runScript(String language, String script) throws Exception {
        TestExecutor executor = newExecutor(ImmutableMap.<String, Object>of("name", language),
                language, script);
        executor.runLimited(7);
        assertEquals(asList(
                "spooler_init is called by " + language,
                "spooler_open is called by " + language,
                "spooler_process is called by " + language,
                "spooler_process is called by " + language,
                "spooler_process is called by " + language,
                "spooler_close is called by " + language,
                "spooler_exit is called by " + language),
                scriptLogger.lines);
    }

    private static String resourceString(String path) {
        try {
            return Resources.toString(getResource(path), Charsets.UTF_8);
        } catch (IOException e) { throw propagate(e); }
    }

    private TestExecutor newExecutor(ImmutableMap<String, Object> bindings, String language, String script) {
        final ImmutableMap<String, Object> completeBindings = new ImmutableMap.Builder<String,Object>()
                .put("spooler_log", scriptLogger)
                .putAll(bindings).build();
        JobScriptInstanceAdapter adapter = new JobScriptInstanceAdapter(language, new Supplier<ImmutableMap<String, Object>>() {
            @Override public ImmutableMap<String, Object> get() {
                return completeBindings;
            }}, script);
        return new TestExecutor(adapter);
    }

    public static class JavaScriptLogger {
        private final List<String> lines = new ArrayList<String>();

        public final void info(String line) {
            lines.add(line);
        }
    }
}
