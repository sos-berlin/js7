package sos.spooler;

public interface HasInitializeSpoolerVariables {

    /**
     * Called by the JobScheduler Agent to initialize the old-style Job_impl and Monitor_impl.
     * <p>
     *     This Method is not called under C++ (local execution or legacy Agent).
     *     To be compatible with the C++ environment, Job_impl and Monitor_impl have to provide
     *     public writable variables called spooler_log, spooler, spooler_job and spooler_task.
     */
    void initializeSpoolerVariables(Log log, Spooler spooler, Job job, Task task);
}
