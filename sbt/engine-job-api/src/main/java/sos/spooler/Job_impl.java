package sos.spooler;

public class Job_impl implements IJob_impl {

    public Log spooler_log;
    public Task spooler_task;
    public Job spooler_job;
    public Spooler spooler;

    protected Job_impl() {}

    @Override
    public final void initializeSpoolerVariables(Log log, Spooler spooler, Job job, Task task) {
        this.spooler_log = log;
        this.spooler = spooler;
        this.spooler_task = task;
        this.spooler_job = job;
    }

    @Override
    public boolean spooler_init() throws Exception {
        return true;
    }

    @Override
    public void spooler_exit() throws Exception {}

    @Override
    public boolean spooler_open() throws Exception {
        return true;
    }

    @Override
    public void spooler_close() throws Exception {}

    @Override
    public boolean spooler_process() throws Exception {
        return false;
    }

    @Override
    public void spooler_on_error() throws Exception {}

    @Override
    public void spooler_on_success() throws Exception {}
}
