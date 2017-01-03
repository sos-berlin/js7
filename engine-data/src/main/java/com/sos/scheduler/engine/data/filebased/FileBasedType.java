package com.sos.scheduler.engine.data.filebased;

import com.sos.scheduler.engine.base.sprayjson.JavaEnumJsonFormat;
import spray.json.JsonFormat;

public enum FileBasedType {
    Folder      ("Folder"        , "folder"       , "Folder"      ),
    Job         ("Job"           , "job"          , "Job"         ),
    JobChain    ("Job_chain"     , "job_chain"    , "JobChain"    ),
    Lock        ("Lock"          , "lock"         , "Lock"        ),
    Monitor     ("Monitor"       , "monitor"      , "Monitor"     ),
    Order       ("Standing_order", "order"        , "Order"       ),
    ProcessClass("Process_class" , "process_class", "ProcessClass"),
    Schedule    ("Schedule"      , "schedule"     , "Schedule"    ),
    Unknown     ("Unknown"       , "unknown"      , "Unknown"     );

    private final String internalCppName;
    private final String cppName;
    private final String camelName;

    FileBasedType(String internalCppName, String cppName, String camelName) {
        this.internalCppName = internalCppName;
        this.cppName = cppName;
        this.camelName = camelName;
    }

    public String cppName() {
        return cppName;
    }

    public String filenameExtension() {
        return this == Folder? "/" : "." + cppName + ".xml";
    }

    @Override public String toString() {
        return camelName;
    }

    public static FileBasedType fromInternalCppName(String name) {
        for (FileBasedType o: values())
            if (o.internalCppName.equals(name))
                return o;
        throw new RuntimeException("Unknown file based type '"+name+"'");
    }

    public static final JsonFormat<FileBasedType> MyJsonFormat = new JavaEnumJsonFormat<>(FileBasedType.class);
}
