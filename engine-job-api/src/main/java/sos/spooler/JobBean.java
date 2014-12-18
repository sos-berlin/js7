package sos.spooler;

import java.io.File;

import static sos.spooler.Beans.toBean;
import static sos.spooler.Beans.toDelegate;

public final class JobBean implements Bean<Job> {
    private final Job delegate;

    JobBean(Job delegate) {
        this.delegate = delegate;
    }

    public TaskBean start() {
        return toBean(delegate.start());
    }

    public TaskBean start(Variable_setBean variables) {
        return toBean(delegate.start(toDelegate(variables)));
    }

    public void wake() {
        delegate.wake();
    }

    public void start_when_directory_changed(String directoryPath) {
        delegate.start_when_directory_changed(directoryPath);
    }

    public void start_when_directory_changed(File directoryPath) {
        delegate.start_when_directory_changed(directoryPath);
    }

    public void start_when_directory_changed(String directoryPath, String filenamePattern) {
        delegate.start_when_directory_changed(directoryPath, filenamePattern);
    }

    public void start_when_directory_changed(File directoryPath, String filenamePattern) {
        delegate.start_when_directory_changed(directoryPath, filenamePattern);
    }

    public void clear_when_directory_changed() {
        delegate.clear_when_directory_changed();
    }

    public String getInclude_path() {
        return delegate.include_path();
    }

    public String getName() {
        return delegate.name();
    }

    public void setState_text(String line) {
        delegate.set_state_text(line);
    }

    public String getTitle() {
        return delegate.title();
    }

    public Order_queueBean getOrder_queue() {
        return toBean(delegate.order_queue());
    }

    public void set_delay_after_error(int errorSteps, double seconds) {
        delegate.set_delay_after_error(errorSteps, seconds);
    }

    public void set_delay_after_error(int errorSteps, String hhmmSs) {
        delegate.set_delay_after_error(errorSteps, hhmmSs);
    }

    public void clear_delay_after_error() {
        delegate.clear_delay_after_error();
    }

    public void set_delay_order_after_setback(int setbackCount, double seconds) {
        delegate.set_delay_order_after_setback(setbackCount, seconds);
    }

    public void set_delay_order_after_setback(int setbackCount, String hhmmSs) {
        delegate.set_delay_order_after_setback(setbackCount, hhmmSs);
    }

    public void setMax_order_setbacks(int setbackCount) {
        delegate.set_max_order_setbacks(setbackCount);
    }

    public int getMax_order_setbacks() {
        return delegate.max_order_setbacks();
    }

    public void remove() {
        delegate.remove();
    }

    public Process_classBean getProcess_class() {
        return toBean(delegate.process_class());
    }

    public String getFolder_path() {
        return delegate.folder_path();
    }

    public String getConfiguration_directory() {
        return delegate.configuration_directory();
    }

    @Deprecated
    public int getSetback_max() {
        return delegate.setback_max();
    }

    public String getScript_code() {
        return delegate.script_code();
    }

    public Job getDelegate() {
        return delegate;
    }
}
