package sos.spooler;

import static sos.spooler.Beans.toBean;
import static sos.spooler.Beans.toDelegate;

public final class SpoolerBean implements Bean<Spooler> {
    private final Spooler delegate;

    SpoolerBean(Spooler delegate) {
        this.delegate = delegate;
    }

    public String getId() {
        return delegate.id();
    }

    public String getParam() {
        return delegate.param();
    }

    public String getDirectory() {
        return delegate.directory();
    }

    public String getInclude_path() {
        return delegate.include_path();
    }

    public String getLog_dir() {
        return delegate.log_dir();
    }

    public String getDb_name() {
        return delegate.db_name();
    }

    public boolean isService() {
        return delegate.is_service();
    }

    public String getHostname() {
        return delegate.hostname();
    }

    public Variable_setBean getVariables() {
        return toBean(delegate.variables());
    }

    public void set_var(String name, String value) {
        delegate.set_var(name, value);
    }

    public String var(String name) {
        return delegate.var(name);
    }

    public Variable_setBean create_variable_set() {
        return toBean(delegate.create_variable_set());
    }

    public LogBean getLog() {
        return toBean(delegate.log());
    }

    public JobBean job(String jobName) {
        return toBean(delegate.job(jobName));
    }

    public Job_chainBean create_job_chain() {
        return toBean(delegate.create_job_chain());
    }

    public void add_job_chain(Job_chainBean jobChain) {
        delegate.add_job_chain(toDelegate(jobChain));
    }

    public Job_chainBean job_chain(String name) {
        return toBean(delegate.job_chain(name));
    }

    public boolean job_chain_exists(String jobChain) {
        return delegate.job_chain_exists(jobChain);
    }

    public OrderBean create_order() {
        return toBean(delegate.create_order());
    }

    public void terminate() {
        delegate.terminate();
    }

    public void terminate(int timeoutInSeconds) {
        delegate.terminate(timeoutInSeconds);
    }

    public void terminate(int timeoutInSeconds, boolean restart, boolean allSchedulers, boolean continueExclusiveScheduler) {
        delegate.terminate(timeoutInSeconds, restart, allSchedulers, continueExclusiveScheduler);
    }

    public void terminate_and_restart() {
        delegate.terminate_and_restart();
    }

    public void terminate_and_restart(int timeoutInSeconds) {
        delegate.terminate_and_restart(timeoutInSeconds);
    }

    public void let_run_terminate_and_restart() {
        delegate.let_run_terminate_and_restart();
    }

    public void abort_immediately() {
        delegate.abort_immediately();
    }

    public void abort_immediately_and_restart() {
        delegate.abort_immediately_and_restart();
    }

    public String getDb_variables_table_name() {
        return delegate.db_variables_table_name();
    }

    public String getDb_tasks_table_name() {
        return delegate.db_tasks_table_name();
    }

    public String getDb_orders_table_name() {
        return delegate.db_orders_table_name();
    }

    public String getDb_history_table_name() {
        return delegate.db_history_table_name();
    }

    public String getDb_order_history_table_name() {
        return delegate.db_order_history_table_name();
    }

    public String getIni_path() {
        return delegate.ini_path();
    }

    public String execute_xml(String xml) {
        return delegate.execute_xml(xml);
    }

    public int getTcp_port() {
        return delegate.tcp_port();
    }

    public int getUdp_port() {
        return delegate.udp_port();
    }

    public Xslt_stylesheetBean create_xslt_stylesheet() {
        return delegate.create_xslt_stylesheet().toBean();
    }

    public Xslt_stylesheetBean create_xslt_stylesheet(java.io.File file) {
        return delegate.create_xslt_stylesheet(file).toBean();
    }

    public Xslt_stylesheetBean create_xslt_stylesheet(String xml) {
        return delegate.create_xslt_stylesheet(xml).toBean();
    }

    public LocksBean getLocks() {
        return toBean(delegate.locks());
    }

    public Process_classesBean getProcess_classes() {
        return toBean(delegate.process_classes());
    }

    public Supervisor_clientBean getSupervisor_client() {
        return toBean(delegate.supervisor_client());
    }

    public String getConfiguration_directory() {
        return delegate.configuration_directory();
    }

    public ScheduleBean schedule(String path) {
        return delegate.schedule(path).toBean();
    }

    public String getUri() {
        return delegate.uri();
    }

    @Override public Spooler getDelegate() {
        return delegate;
    }
}
