package com.sos.scheduler.engine.taskserver.dotnet;

import com.sos.scheduler.engine.taskserver.dotnet.api.DotnetModuleReference;
import com.sos.scheduler.engine.taskserver.dotnet.api.TaskContext;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import net.sf.jni4net.Bridge;
import sos.spooler.Job;
import sos.spooler.Log;
import sos.spooler.Spooler;
import sos.spooler.Task;
import system.reflection.Assembly;

public class DotnetApiImpl {
    private static final String POWERSHELL_CLASS_NAME = "sos.spooler.PowershellAdapter";
    private static final String SCRIPT_CONTROL_CLASS_NAME = "sos.spooler.ScriptControlAdapter";

    private final DotnetBridge bridge;
    private final DotnetModuleReference reference;
    private final system.Type apiImplType;
    private Path path;
    private String className;
    private system.Object apiImplInstance;

    public DotnetApiImpl(DotnetBridge dotnetBridge, DotnetModuleReference ref, TaskContext taskContext) {
        bridge = dotnetBridge;
        reference = ref;
        setPropertiesFromReference();
        apiImplType = apiImplType(path, className);
        initApiImplInstance(taskContext.spoolerLog(), taskContext.spoolerTask(), taskContext.spoolerJob(), taskContext.spooler());
    }

    private void setPropertiesFromReference() {
        if (reference instanceof DotnetModuleReference.DotnetClass) {
            path = ((DotnetModuleReference.DotnetClass)reference).dll();
            className = ((DotnetModuleReference.DotnetClass)reference).className();
        } else if (reference instanceof DotnetModuleReference.Powershell) {
            path = bridge.getDotnetAdapterDll();
            className = POWERSHELL_CLASS_NAME;
        } else if (reference instanceof DotnetModuleReference.ScriptControl) {
            path = bridge.getDotnetAdapterDll();
            className = SCRIPT_CONTROL_CLASS_NAME;
        } else {
            throw new RuntimeException("Unsupported reference " + reference.getClass());
        }

        if (!Files.exists(path)) {
            throw new RuntimeException(String.format("File not found: %s",
                    path.toString()));
        }
    }

    private static system.Type apiImplType(Path path, String className) {
        Assembly assembly;
        try {
            assembly = Assembly.LoadFrom(path.toString());
        } catch (Exception ex) {
            throw new RuntimeException(String.format("[%s] Can't load assembly: %s",
                    path.toString(), ex.toString()));
        }

        return Optional.ofNullable(assembly.GetType(className))
                .orElseThrow(
                        () -> new RuntimeException(String.format(
                                "[%s] Class not found: %s", path.toString(),
                                className)));
    }

    private void initApiImplInstance(Log spoolerLog, Task spoolerTask, Job spoolerJob, Spooler spooler) {

        system.Type[] types;
        system.Object[] params;
        if (reference instanceof DotnetModuleReference.Powershell) {
            types = new system.Type[]{
                    bridge.getSchedulerApiTypes()[0],
                    bridge.getSchedulerApiTypes()[1],
                    bridge.getSchedulerApiTypes()[2],
                    bridge.getSchedulerApiTypes()[3],
                    system.Type.GetType("System.String")
            };
            params = new system.Object[]{
                    Bridge.wrapJVM(spoolerLog),
                    Bridge.wrapJVM(spoolerTask),
                    Bridge.wrapJVM(spoolerJob),
                    Bridge.wrapJVM(spooler),
                    new system.String(((DotnetModuleReference.Powershell)reference).script())
            };
        } else if (reference instanceof DotnetModuleReference.DotnetClass) {
            types = new system.Type[]{};
            params = new system.Object[]{};
        } else if (reference instanceof DotnetModuleReference.ScriptControl) {
            types = new system.Type[]{
                    bridge.getSchedulerApiTypes()[0],
                    bridge.getSchedulerApiTypes()[1],
                    bridge.getSchedulerApiTypes()[2],
                    bridge.getSchedulerApiTypes()[3],
                    system.Type.GetType("System.String"),
                    system.Type.GetType("System.String")
            };
            params = new system.Object[]{
                    Bridge.wrapJVM(spoolerLog),
                    Bridge.wrapJVM(spoolerTask),
                    Bridge.wrapJVM(spoolerJob),
                    Bridge.wrapJVM(spooler),
                    new system.String(((DotnetModuleReference.ScriptControl)reference).script()),
                    new system.String(((DotnetModuleReference.ScriptControl)reference).language())
            };
        } else {
            throw new IllegalArgumentException();
        }

        apiImplInstance = Optional
                .ofNullable(DotnetInvoker.createInstance(apiImplType, types, params))
                .orElseThrow(
                        () -> new RuntimeException(
                                String.format("[%s] Could not create a new instance of the class %s",
                                        path.toString(), className)));

        if (reference instanceof DotnetModuleReference.DotnetClass) {
            setApiImplInstanceProperty("spooler_log", spoolerLog);
            setApiImplInstanceProperty("spooler_task", spoolerTask);
            setApiImplInstanceProperty("spooler_job", spoolerJob);
            setApiImplInstanceProperty("spooler", spooler);
        }
    }

    private void setApiImplInstanceProperty(String name, java.lang.Object value) {
        apiImplType.GetProperty(name).SetValue(apiImplInstance, Bridge.wrapJVM(value), null);
    }

    private system.Object toDotnetBoolean(boolean value) throws Exception {
        return DotnetInvoker.invokeMethod(apiImplType, apiImplInstance,
                "ToBoolean", value? "true" : "false");
    }

    public boolean spooler_init() throws Exception {
        return DotnetInvoker.invokeMethod(apiImplType, apiImplInstance,
                "spooler_init", true);
    }

    public boolean spooler_open() throws Exception {
        return DotnetInvoker.invokeMethod(apiImplType, apiImplInstance,
                "spooler_open", true);
    }

    public boolean spooler_process() throws Exception {
        return DotnetInvoker.invokeMethod(apiImplType, apiImplInstance,
                "spooler_process", false);
    }

    public void spooler_close() throws Exception {
        if (apiImplType != null && apiImplInstance != null) {
            DotnetInvoker.invokeMethod(apiImplType, apiImplInstance,
                    "spooler_close");
        }
    }

    public void spooler_on_success() throws Exception {
        if (apiImplType != null && apiImplInstance != null) {
            DotnetInvoker.invokeMethod(apiImplType, apiImplInstance,
                    "spooler_on_success");
        }
    }

    public void spooler_on_error() throws Exception {
        if (apiImplType != null && apiImplInstance != null) {
            DotnetInvoker.invokeMethod(apiImplType, apiImplInstance,
                    "spooler_on_error");
        }
    }

    public void spooler_exit() throws Exception {
        if (apiImplType != null && apiImplInstance != null) {
            DotnetInvoker.invokeMethod(apiImplType, apiImplInstance,
                    "spooler_exit");
        }
    }

    public boolean spooler_task_before() throws Exception {
        return DotnetInvoker.invokeMethod(apiImplType, apiImplInstance,
                "spooler_task_before", true);
    }

    public boolean spooler_process_before() throws Exception {
        return DotnetInvoker.invokeMethod(apiImplType, apiImplInstance,
                "spooler_process_before", true);
    }

    public boolean spooler_process_after(boolean spooler_process_result)
            throws Exception {
        system.Type[] paramTypes = {system.Type.GetType("System.Boolean")};
        system.Object[] params = {toDotnetBoolean(spooler_process_result)};
        return DotnetInvoker.invokeMethod(apiImplType, apiImplInstance,
                "spooler_process_after", paramTypes, params, spooler_process_result);
    }

    public void spooler_task_after() throws Exception {
        DotnetInvoker.invokeMethod(apiImplType, apiImplInstance,
                "spooler_task_after");
    }
}
