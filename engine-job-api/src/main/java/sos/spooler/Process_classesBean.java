package sos.spooler;

import static sos.spooler.Beans.*;

public final class Process_classesBean implements Bean<Process_classes> {
    private final Process_classes delegate;

    Process_classesBean(Process_classes delegate) {
        this.delegate = delegate;
    }

    public Process_classBean process_class(String path) {
        return toBean(delegate.process_class(path));
    }

    public Process_classBean process_class_or_null(String path) {
        return toBean(delegate.process_class_or_null(path));
    }

    public Process_classBean create_process_class() {
        return toBean(delegate.create_process_class());
    }

    public void add_process_class(Process_classBean processClass) {
        delegate.add_process_class(toDelegate(processClass));
    }

    @Override public Process_classes getDelegate() {
        return delegate;
    }
}
