package sos.spooler;

public interface IMonitor_impl extends HasInitializeSpoolerVariables {
    boolean spooler_task_before() throws Exception;
    void spooler_task_after() throws Exception;
    boolean spooler_process_before() throws Exception;
    boolean spooler_process_after(boolean spooler_process_result) throws Exception;
}
