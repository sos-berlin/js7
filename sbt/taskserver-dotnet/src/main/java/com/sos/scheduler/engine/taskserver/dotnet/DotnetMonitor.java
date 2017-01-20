package com.sos.scheduler.engine.taskserver.dotnet;

public final class DotnetMonitor extends sos.spooler.Monitor_impl {
    private final DotnetApiImpl apiImpl;

    DotnetMonitor(DotnetApiImpl dotnetApiImpl)
            throws Exception {
        this.apiImpl = dotnetApiImpl;
    }

    @Override
    public boolean spooler_task_before() throws Exception {
        return apiImpl.spooler_task_before();
    }

    @Override
    public void spooler_task_after() throws Exception {
        apiImpl.spooler_task_after();
    }

    @Override
    public boolean spooler_process_before() throws Exception {
        return apiImpl.spooler_process_before();
    }

    @Override
    public boolean spooler_process_after(boolean spooler_process_result)
            throws Exception {
        return apiImpl.spooler_process_after(spooler_process_result);
    }
}
