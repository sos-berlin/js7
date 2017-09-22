package sos.spooler;

public class Job extends Idispatch implements HasBean<JobBean> {
    public Job(Invoker invoker) {
        super(invoker);
    }

    private Job(long idispatch) {
        super(idispatch);
    }

    public Task start() {
        return (Task)com_call("start");
    }

    public Task start(Variable_set variables) {
        return (Task)com_call("start", variables);
    }

    public void wake() {
        com_call("wake");
    }

    public void start_when_directory_changed(String directory_name) {
        com_call("start_when_directory_changed", directory_name);
    }

    public void start_when_directory_changed(java.io.File directory_name) {
        com_call("start_when_directory_changed", directory_name.toString());
    }

    public void start_when_directory_changed(String directory_name, String filename_pattern) {
        com_call("start_when_directory_changed", directory_name, filename_pattern);
    }

    public void start_when_directory_changed(java.io.File directory_name, String filename_pattern) {
        com_call("start_when_directory_changed", directory_name.toString(), filename_pattern);
    }

    public void clear_when_directory_changed() {
        com_call("clear_when_directory_changed");
    }

    @SchedulerGetter
    public String include_path() {
        return (String)com_call("<include_path");
    }

    @SchedulerGetter
    public String name() {
        return (String)com_call("<name");
    }

    public void set_state_text(String line) {
        com_call(">state_text", line);
    }

    @SchedulerGetter
    public String title() {
        return (String)com_call("<title");
    }

    @SchedulerGetter
    public Order_queue order_queue() {
        return (Order_queue)com_call("<order_queue");
    }

    public void set_delay_after_error(int error_steps, double seconds) {
        com_call(">delay_after_error", error_steps, seconds);
    }

    public void set_delay_after_error(int error_steps, String hhmm_ss) {
        com_call(">delay_after_error", error_steps, hhmm_ss);
    }

    public void clear_delay_after_error() {
        com_call("clear_delay_after_error");
    }

    public void set_delay_order_after_setback(int setback_count, double seconds) {
        com_call(">delay_order_after_setback", setback_count, seconds);
    }

    public void set_delay_order_after_setback(int setback_count, String hhmm_ss) {
        com_call(">delay_order_after_setback", setback_count, hhmm_ss);
    }

    public void set_max_order_setbacks(int setback_count) {
        com_call(">max_order_setbacks", setback_count);
    }

    @SchedulerGetter
    public int max_order_setbacks() {
        return (Integer)com_call("<max_order_setbacks");
    }

    public void remove() {
        com_call("remove");
    }

    @SchedulerGetter
    public Process_class process_class() {
        return (Process_class)com_call("<process_class");
    }

    @SchedulerGetter
    public String folder_path() {
        return (String)com_call("<folder_path");
    }

    @SchedulerGetter
    public String configuration_directory() {
        return (String)com_call("<configuration_directory");
    }

    @Deprecated
    @SchedulerGetter
    public int setback_max() {
        return (Integer)com_call("<setback_max");
    }

    @SchedulerGetter
    public String script_code() {
        return (String)com_call("<script_code");
    }

    @Override public final JobBean toBean() {
        return new JobBean(this);
    }
}
