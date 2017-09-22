package sos.spooler;

public class Subprocess extends Idispatch implements HasBean<SubprocessBean> {
    public Subprocess(Invoker invoker) {
        super(invoker);
    }

    private Subprocess(long idispatch) {
        super(idispatch);
    }

    public void close() {
        com_call("close");
    }

    public void start(String command_line) {
        com_call("start", command_line);
    }

    public void start(String filename_and_arguments[]) {
        com_call("start", (Object)filename_and_arguments);
    }

    @SchedulerGetter
    public int pid() {
        return int_com_call("<pid");
    }

    @SchedulerGetter
    public boolean terminated() {
        return boolean_com_call("<terminated");
    }

    @SchedulerGetter
    public int exit_code() {
        return int_com_call("<exit_code");
    }

    @SchedulerGetter
    public int termination_signal() {
        return int_com_call("<termination_signal");
    }

    public void set_ignore_error(boolean b) {
        com_call(">ignore_error", b);
    }

    @SchedulerGetter
    public boolean ignore_error() {
        return boolean_com_call("<ignore_error");
    }

    public void set_ignore_signal(boolean b) {
        com_call(">ignore_signal", b);
    }

    @SchedulerGetter
    public boolean ignore_signal() {
        return boolean_com_call("<ignore_signal");
    }

    public void set_timeout(double seconds) {
        com_call(">timeout", seconds);
    }

    public void set_environment(String entry_name, String value) {
        com_call(">environment", entry_name, value);
    }

    public boolean wait_for_termination(double seconds) {
        return boolean_com_call("wait_for_termination", seconds);
    }

    public void wait_for_termination() {
        com_call("wait_for_termination");
    }

    public void kill(int signal) {
        com_call("kill", signal);
    }

    public void kill() {
        com_call("kill");
    }

    public void set_priority(int priority) {
        com_call(">priority", priority);
    }

    @SchedulerGetter
    public int priority() {
        return int_com_call("<priority");
    }

    public void set_priority_class(String priority_class) {
        com_call(">priority_class", priority_class);
    }

    @SchedulerGetter
    public String priority_class() {
        return (String)com_call("<priority_class");
    }

    public void set_own_process_group(boolean b) {
        com_call(">own_process_group", b);
    }

    @SchedulerGetter
    public boolean own_process_group() {
        return boolean_com_call("<own_process_group");
    }

    @SchedulerGetter
    public Variable_set env() {
        return (Variable_set)com_call("<env");
    }

    @Override public final SubprocessBean toBean() {
        return new SubprocessBean(this);
    }
}
