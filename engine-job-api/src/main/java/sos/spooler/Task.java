package sos.spooler;

public class Task extends Idispatch implements HasBean<TaskBean> {
    public Task(Invoker invoker) {
        super(invoker);
    }

    private Task(long idispatch) {
        super(idispatch);
    }

    @SchedulerGetter
    public Job job() {
        return (Job)com_call("<job");
    }

    @SchedulerGetter
    public int id() {
        return int_com_call("<id");
    }

    @SchedulerGetter
    public Variable_set params() {
        return (Variable_set)com_call("<params");
    }

    public void set_result(String value) {
        com_call(">result", value);
    }

    @SchedulerGetter
    public String result() {
        return (String)com_call("<result");
    }

    public void set_repeat(double seconds) {
        com_call(">repeat", seconds);
    }

    public void end() {
        com_call("end");
    }

    public void set_error(String text) {
        com_call(">error", text);
    }

    @SchedulerGetter
    public Error error() {
        return (Error)com_call("<error");
    }

    public boolean wait_until_terminated() {
        return boolean_com_call("wait_until_terminated");
    }

    public boolean wait_until_terminated(double wait_seconds) {
        return (Boolean)com_call("wait_until_terminated", wait_seconds);
    }

    public void set_history_field(String name, String value) {
        com_call(">history_field", name, value);
    }

    public void set_delay_spooler_process(double seconds) {
        com_call(">delay_spooler_process", seconds);
    }

    public void set_delay_spooler_process(String hhmm_ss) {
        com_call(">delay_spooler_process", hhmm_ss);
    }

    @SchedulerGetter
    public Order order() {
        return (Order)com_call("<order");
    }

    @SchedulerGetter
    public String changed_directories() {
        return (String)com_call("<changed_directories");
    }

    public void add_pid(int pid) {
        com_call("add_pid", pid);
    }

    public void add_pid(int pid, double timeout_seconds) {
        com_call("add_pid", pid, timeout_seconds);
    }

    public void remove_pid(int pid) {
        com_call("remove_pid", pid);
    }

    @SchedulerGetter
    public String stdout_text() {
        return (String)com_call("<stdout_text");
    }

    @SchedulerGetter
    public String stdout_path() {
        return (String)com_call("<stdout_path");
    }

    @SchedulerGetter
    public String stderr_text() {
        return (String)com_call("<stderr_text");
    }

    @SchedulerGetter
    public String stderr_path() {
        return (String)com_call("<stderr_path");
    }

    public Subprocess create_subprocess() {
        return (Subprocess)com_call("create_subprocess");
    }

    public Subprocess create_subprocess(String command_line) {
        return (Subprocess)com_call("create_subprocess", command_line);
    }

    public Subprocess create_subprocess(String filename_and_arguments[]) {
        return (Subprocess)com_call("create_subprocess", (Object[])filename_and_arguments);
    }

    @SchedulerGetter
    public Web_service web_service() {
        return (Web_service)com_call("<web_service");
    }

    @SchedulerGetter
    public Web_service web_service_or_null() {
        return (Web_service)com_call("<web_service_or_null");
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

    public void set_exit_code(int exit_code) {
        com_call(">exit_code", exit_code);
    }

    @SchedulerGetter
    public int exit_code() {
        return int_com_call("<exit_code");
    }

    @SchedulerGetter
    public String trigger_files() {
        return (String)com_call("<trigger_files");
    }

    public boolean try_hold_lock(String lock_path) {
        return boolean_com_call("try_hold_lock", lock_path);
    }

    public boolean try_hold_lock_non_exclusive(String lock_path) {
        return boolean_com_call("try_hold_lock_non_exclusive", lock_path);
    }

    public void call_me_again_when_locks_available() {
        com_call("call_me_again_when_locks_available");
    }

    @SchedulerGetter
    public String web_service_access_token() {
        return (String)com_call("<web_service_access_token");
    }

    @Override public final TaskBean toBean() {
        return new TaskBean(this);
    }
}
