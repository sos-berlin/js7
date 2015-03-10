package sos.spooler;

/**
 * @author Joacim Zschimmer
 */
public interface Invoker {
    Object call(String name, Object[] params);
}
