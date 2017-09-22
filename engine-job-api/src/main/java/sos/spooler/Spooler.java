package sos.spooler;

public class Spooler extends Idispatch implements HasBean<SpoolerBean> {
    private Spooler(long idispatch) {
        super(idispatch);
    }

    public Spooler(Invoker invoker) {
        super(invoker);
    }

    @SchedulerGetter
    public String id() {
        return (String)com_call("<id");
    }

    @SchedulerGetter
    public String param() {
        return (String)com_call("<param");
    }

    @SchedulerGetter
    public String directory() {
        return (String)com_call("<directory");
    }

    @SchedulerGetter
    public String include_path() {
        return (String)com_call("<include_path");
    }

    @SchedulerGetter
    public String log_dir() {
        return (String)com_call("<log_dir");
    }

    @SchedulerGetter
    public String db_name() {
        return (String)com_call("<db_name");
    }

    @SchedulerGetter
    public boolean is_service() {
        return boolean_com_call("<is_service");
    }

    @SchedulerGetter
    public String hostname() {
        return (String)com_call("<hostname");
    }

    @SchedulerGetter
    public Variable_set variables() {
        return (Variable_set)com_call("<variables");
    }

    public void set_var(String name, String value) {
        com_call(">var", name, value);
    }

    public String var(String name) {
        return (String)com_call("<var", name);
    }

    public Variable_set create_variable_set() {
        return (Variable_set)com_call("create_variable_set");
    }

    @SchedulerGetter
    public Log log() {
        return (Log)com_call("<log");
    }

    public Job job(String job_name) {
        return (Job)com_call("<job", job_name);
    }

    public Job_chain create_job_chain() {
        return (Job_chain)com_call("create_job_chain");
    }

    public void add_job_chain(Job_chain job_chain) {
        com_call("add_job_chain", job_chain);
    }

    public Job_chain job_chain(String name) {
        return (Job_chain)com_call("<job_chain", name);
    }

    public boolean job_chain_exists(String job_chain) {
        return boolean_com_call("job_chain_exists", job_chain);
    }

    public Order create_order() {
        return (Order)com_call("create_order");
    }

    public void terminate() {
        com_call("terminate");
    }

    public void terminate(int timeout_in_seconds) {
        com_call("terminate", timeout_in_seconds);
    }

    public void terminate(int timeout_in_seconds, boolean restart, boolean all_schedulers, boolean continue_exclusive_scheduler) {
        com_call("terminate", timeout_in_seconds,
            restart,
            all_schedulers,
            continue_exclusive_scheduler);
    }

    public void terminate_and_restart() {
        com_call("terminate_and_restart");
    }

    public void terminate_and_restart(int timeout_in_seconds) {
        com_call("terminate_and_restart", timeout_in_seconds);
    }

    public void let_run_terminate_and_restart() {
        com_call("let_run_terminate_and_restart");
    }

    public void abort_immediately() {
        com_call("abort_immediately");
    }

    public void abort_immediately_and_restart() {
        com_call("abort_immediately_and_restart");
    }

    @SchedulerGetter
    public String db_variables_table_name() {
        return (String)com_call("<db_variables_table_name");
    }

    @SchedulerGetter
    public String db_tasks_table_name() {
        return (String)com_call("<db_tasks_table_name");
    }

    @SchedulerGetter
    public String db_orders_table_name() {
        return (String)com_call("<db_orders_table_name");
    }

    @SchedulerGetter
    public String db_history_table_name() {
        return (String)com_call("<db_history_table_name");
    }

    @SchedulerGetter
    public String db_order_history_table_name() {
        return (String)com_call("<db_order_history_table_name");
    }

    @SchedulerGetter
    public String ini_path() {
        return (String)com_call("<ini_path");
    }

    public String execute_xml(String xml) {
        return (String)com_call("execute_xml", xml);
    }

    @SchedulerGetter
    public int tcp_port() {
        return int_com_call("<tcp_port");
    }

    @SchedulerGetter
    public int udp_port() {
        return int_com_call("<udp_port");
    }

    public Xslt_stylesheet create_xslt_stylesheet() {
        return (Xslt_stylesheet)com_call("Create_xslt_stylesheet");
    }

    public Xslt_stylesheet create_xslt_stylesheet(java.io.File file) {
        Xslt_stylesheet stylesheet = (Xslt_stylesheet)com_call("Create_xslt_stylesheet");
        stylesheet.load_file(file);
        return stylesheet;
    }

    public Xslt_stylesheet create_xslt_stylesheet(String xml) {
        Xslt_stylesheet stylesheet = (Xslt_stylesheet)com_call("Create_xslt_stylesheet");
        stylesheet.load_xml(xml);
        return stylesheet;
    }

    @SchedulerGetter
    public Locks locks() {
        return (Locks)com_call("<locks");
    }

    @SchedulerGetter
    public Process_classes process_classes() {
        return (Process_classes)com_call("<process_classes");
    }

    @SchedulerGetter
    public Supervisor_client supervisor_client() {
        return (Supervisor_client)com_call("<supervisor_client");
    }

    @SchedulerGetter
    public String configuration_directory() {
        return (String)com_call("<configuration_directory");
    }

    public Schedule schedule(String path) {
        return (Schedule)com_call("<schedule", path);
    }

    @SchedulerGetter
    public String uri() {
        return (String)com_call("<uri");
    }

    @Override public SpoolerBean toBean() {
        return new SpoolerBean(this);
    }
}
