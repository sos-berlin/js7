package com.sos.scheduler.engine.taskserver.dotnet;

import com.sos.scheduler.engine.taskserver.dotnet.api.DotnetModuleInstanceFactory;
import com.sos.scheduler.engine.taskserver.dotnet.api.DotnetModuleReference;
import com.sos.scheduler.engine.taskserver.dotnet.api.TaskContext;
import java.nio.file.Path;

public final class Jni4netModuleInstanceFactory implements
        DotnetModuleInstanceFactory {

    private final DotnetBridge dotnetBridge;
    private final Path dllDirectory;

    public Jni4netModuleInstanceFactory(Path dllDirectory) throws Exception {
        this.dllDirectory = dllDirectory;
        boolean debug = System.getProperty("jni4net.debug") != null;
        dotnetBridge = new DotnetBridge();
        dotnetBridge.init(dllDirectory, debug);
    }

    public void close() {
    }

    public <T> T newInstance(Class<T> clazz, TaskContext taskContext, DotnetModuleReference reference) throws Exception {
        return newSchedulerDotnetAdapter(clazz, reference, taskContext);
    }

    @SuppressWarnings("unchecked")
    private <T> T newSchedulerDotnetAdapter(Class<T> clazz,
            DotnetModuleReference reference, TaskContext taskContext)
            throws Exception {
        DotnetApiImpl dotnetObject = new DotnetApiImpl(dotnetBridge, reference, taskContext);

        if (sos.spooler.IJob_impl.class.isAssignableFrom(clazz)) {
            return (T)new DotnetJob(taskContext, dotnetObject);
        } else if (sos.spooler.IMonitor_impl.class.isAssignableFrom(clazz)) {
            return (T)new DotnetMonitor(dotnetObject);
        } else {
            throw new IllegalArgumentException("Unsupported " + clazz);
        }
    }

    private <T> void closeInstance(T instance) {
        // TODO Close DotnetJob or DotnetMonitor
    }

    @Override
    public String toString() {
        return "Jni4netModuleInstanceFactory(" + dllDirectory + ")";
    }
}
