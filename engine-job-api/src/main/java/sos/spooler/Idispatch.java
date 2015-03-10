package sos.spooler;

/**
 * @author Joacim Zschimmer
 */
public class Idispatch {
    // Two different use cases: Dispatcher for C++ or pure Java (the Agent).
    // For compatibility with C++ and due to the lack of mixins in Java both use cases are implemented in this class. See com_call.
    private volatile long _idispatch;
    private final Invoker invoker;

    protected Idispatch(long idispatch) {
        _idispatch = idispatch;
        invoker = null;
    }

    protected Idispatch(Invoker invoker) {
        _idispatch = 0;
        this.invoker = invoker;
    }

    final void com_clear() {
        _idispatch = 0;
    }

    Object com_call(String name) {
        return com_call(_idispatch, name, null);
    }

    boolean boolean_com_call(String name) {
        return (Boolean)com_call(name);
    }

    boolean boolean_com_call(String name, Object par1) {
        Object[] params = new Object[1];
        params[0] = par1;
        return (Boolean)com_call(name, params);
    }

    int int_com_call(String name) {
        return (Integer)com_call(name);
    }

    double double_com_call(String name) {
        return (Double)com_call(name);
    }

    Object com_call(String name, Object... arguments) {
        if (invoker != null) {
            return invoker.call(name, arguments);
        } else {
            return com_call(_idispatch, name, arguments);
        }
    }

    /** Not implemented in a pure Java context. JVM links this only when called. */
    private static native Object com_call(long idispatch, String name, Object[] params);
}
