package com.sos.scheduler.engine.jobapi.scripting;

import org.junit.Test;

import javax.script.*;

import static org.junit.Assert.assertEquals;

public final class ScriptEngineTest {
	private final ScriptEngine engine = new ScriptEngineManager().getEngineByName("JavaScript");

    @Test public void testBinding() {
        Bindings bindings = engine.getBindings(ScriptContext.ENGINE_SCOPE);
        bindings.put("name", "hello");
        assertEquals("hello", engine.get("name"));
    }

	@Test public void testFunction() throws ScriptException, NoSuchMethodException {
        engine.eval("function add (a, b) { var c = a + b; return c; }");
		Invocable invocable = (Invocable)engine;
        assertEquals(15.0, ((Number)invocable.invokeFunction("add", 10, 5)).doubleValue(), 0.01);   // Java 7 liefert Double, Java 8 liefert Integer (oder Long?)

        // Brauchen wir nicht
        // Since Java 8: ECMAScript Exception: TypeError: Can not extend/implement non-public class/interface com.sos.scheduler.engine.jobapi.scripting.ScriptEngineTest.IntegerAdder.
        //IntegerAdder adder = invocable.getInterface(IntegerAdder.class);
        //assertEquals(15, adder.add(10, 5));
	}

//	private interface IntegerAdder {
//	    int add(int a, int b);
//	}
}
