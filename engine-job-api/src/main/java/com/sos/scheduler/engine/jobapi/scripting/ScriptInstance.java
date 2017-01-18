package com.sos.scheduler.engine.jobapi.scripting;

import com.google.common.base.Function;
import com.google.common.base.Joiner;
import com.google.common.base.Supplier;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Iterables;

import javax.script.Invocable;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;

import static com.google.common.base.Throwables.propagate;
import static javax.script.ScriptContext.ENGINE_SCOPE;

/**
 * General wrapper for the javax.script interface
 *
 * This class provides a general mechanism to call script in different languages.
 *
 * {@code
ScriptInstance module = new ScriptInstance("javascript");
module.setSourceCode("print('Hello ' + name + '\\n');");
module.addObject("nick", "name");
module.call();
 * }
 */
public class ScriptInstance {

    private final ScriptEngine engine;
    private final Supplier<ImmutableMap<String, Object>> bindingsLazy;
    private final String script;

    public ScriptInstance(String language, Supplier<ImmutableMap<String, Object>> bindingsLazy, String script) {
        this.engine = newScriptEngine(language.toLowerCase());
        this.bindingsLazy = bindingsLazy;
        this.script = script;
    }

    private static ScriptEngine newScriptEngine(String language) {
        ScriptEngine result = new ScriptEngineManager().getEngineByName(language);
        if (result == null) throw throwUnknownLanguage(language);
        return result;
    }

    private static RuntimeException throwUnknownLanguage(String language) {
        String availableLanguages = Joiner.on(", ").join(Iterables.transform(new ScriptEngineManager().getEngineFactories(),
                new Function<ScriptEngineFactory, String>() {
                    public String apply(ScriptEngineFactory o) {
                        return o.getLanguageName();
                    }
                }));
        throw new RuntimeException("Script language '"+ language +"' is unknown. Available languages are "+availableLanguages);
    }

    public final void loadScript() {
        try {
            for (ImmutableMap.Entry<String,Object> e: bindingsLazy.get().entrySet())
                engine.put(e.getKey(), e.getValue());
            engine.eval(script);
        }
        catch (ScriptException e) { throw propagate(e); }
    }

    public final boolean callBooleanWhenExists(boolean defaultResult, String name, Object... parameters) {
        try {
            return callBooleanWithDefault(defaultResult, name, parameters);
        } catch (NoSuchMethodException e) {
            //logger.debug(e +", method="+ name);
            return defaultResult;
        }
    }

    public final boolean callBooleanWithDefault(boolean deflt, String functionName, Object... parameters) throws NoSuchMethodException {
        return resultToBoolean(call(functionName, parameters), deflt);
    }

    private static boolean resultToBoolean(Object result, boolean deflt) {
        if (result instanceof Boolean) return (Boolean)result;
        else
        if (result == null) return deflt;
        //else
        //if (result instanceof Integer) return (Integer)result != 0;
        else
            throw new RuntimeException("The function has not returned a Boolean: "+ result);
    }

    public final void callWhenExists(String name, Object... parameters) {
        try {
            call(name, parameters);
        } catch (NoSuchMethodException e) {
            //logger.trace(e +", function="+name);
        }
    }

    public final Object call(String functionName, Object... parameters) throws NoSuchMethodException {
		try {
			//logger.trace("Call function " + functionName);
			Invocable invocableEngine = (Invocable)engine;
			Object result = invocableEngine.invokeFunction(functionName, parameters);
            //logger.trace("Result is " + result);
            return (Boolean)result;
        }
        catch (ScriptException e) { throw propagate(e); }
    }

    public final void close() {
        engine.setBindings(engine.createBindings(), ENGINE_SCOPE);
    }
}
