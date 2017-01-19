package sos.spooler;

public interface IJob_impl extends HasInitializeSpoolerVariables {
    boolean spooler_init() throws Exception;
    void spooler_exit() throws Exception;
    boolean spooler_open() throws Exception;
    void spooler_close() throws Exception;
    boolean spooler_process() throws Exception;
    void spooler_on_error() throws Exception;
    void spooler_on_success() throws Exception;
}
