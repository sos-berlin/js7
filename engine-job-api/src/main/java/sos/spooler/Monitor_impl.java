// $Id: Monitor_impl.java 3946 2005-09-26 08:52:01Z jz $

package sos.spooler;

/**
 * Oberklasse für die Implementierung eines Monitors
 *
 * @author Joacim Zschimmer, Zschimmer GmbH
 * @version $Revision: 3946 $
 */
public class Monitor_impl implements IMonitor_impl {

    public Log spooler_log;
    public Task spooler_task;
    public Job spooler_job;
    public Spooler spooler;

    protected Monitor_impl() {}

    @Override
    public final void initializeSpoolerVariables(Log log, Spooler spooler, Job job, Task task) {
        this.spooler_log = log;
        this.spooler = spooler;
        this.spooler_task = task;
        this.spooler_job = job;
    }

    @Override
    public boolean spooler_task_before() throws Exception {
        return true;
    }

    @Override
    public void spooler_task_after() throws Exception {
    }

    @Override
    public boolean spooler_process_before() throws Exception {
        return true;
    }

    @Override
    public boolean spooler_process_after(boolean spooler_process_result) throws Exception {
        return spooler_process_result;
    }
}
