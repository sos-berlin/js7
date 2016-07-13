package com.sos.scheduler.engine.common.system;

import com.google.common.base.Function;
import com.google.common.io.ByteStreams;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import javax.annotation.Nullable;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.io.Files.createParentDirs;
import static com.google.common.io.Files.createTempDir;
import static com.sos.scheduler.engine.common.system.Files.DirectoryHandling.dontRemoveDirectory;
import static com.sos.scheduler.engine.common.system.Files.DirectoryHandling.removeDirectory;
import static com.sos.scheduler.engine.common.system.OperatingSystemJava.isUnix;

public final class Files {
    public enum DirectoryHandling {removeDirectory, dontRemoveDirectory}

    private static final Logger logger = LoggerFactory.getLogger(Files.class);

    private Files() {}

    public static void makeExecutable(File f) {
        if (isUnix) {
            boolean ok = f.setExecutable(true);
            if (!ok)  throw new RuntimeException("setExecutable() failed on "+f);
        }
    }

    public static void makeDirectory(File dir) {
        boolean ok = dir.mkdir();
        if (!ok  &&  !dir.isDirectory())  throw new RuntimeException("Directory cannot be created: " + dir);
    }

    public static void makeDirectories(File dir) {
        try {
            createParentDirs(new File(dir, "x"));
        } catch (IOException x) { throw new RuntimeException(x); }
    }

    public static File makeTemporaryDirectory() {
        try {
            return createTempDir().getCanonicalFile();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static void tryRemoveDirectoryRecursivly(File directory) {
        removeDirectoryRecursivly(directory, tryRemoveFileFunction, removeDirectory);
    }

    public static void removeDirectoryRecursivly(File directory) {
        removeDirectoryRecursivly(directory, removeFileFunction, removeDirectory);
    }

    public static void removeDirectoryContentRecursivly(File directory) {
        removeDirectoryRecursivly(directory, removeFileFunction, dontRemoveDirectory);
    }

    public static void removeDirectoryRecursivly(File directory, Function<File,Void> remover, DirectoryHandling dirHandling) {
        try {
            checkArgument(directory.isDirectory() || !directory.exists(), "Not a directory: %s", directory);
            removeAbsoluteDirectoryRecursivly(directory.getCanonicalFile().getAbsoluteFile(), remover, dirHandling);
        } catch (IOException x) {
            if (directory.exists()) throw new RuntimeException(x);
        }
    }

    private static void removeAbsoluteDirectoryRecursivly(File dir, Function<File,Void> remover, DirectoryHandling dirHandling) throws IOException {
        String[] names = dir.list();
        if (names != null  &&  names.length > 0) {
            if (directoryCouldBeALink(dir, names[0]))
                logger.debug("Seems to be a link and will not be deleted: "+dir);
            else {
                for (String name: names) {
                    File f = new File(dir, name);
                    if (f.isDirectory()) removeAbsoluteDirectoryRecursivly(f, remover, removeDirectory);
                    else remover.apply(f);
                }
            }
        }
        if (dirHandling == removeDirectory)
            remover.apply(dir);
    }

    private static final Function<File,Void> removeFileFunction = new Function<File,Void>() {
        @Override @Nullable public Void apply(File f) {
            removeFile(f);
            return null;
        }
    };

    private static final Function<File,Void> tryRemoveFileFunction = new Function<File,Void>() {
        @Override @Nullable public Void apply(File f) {
            try {
                removeFile(f);
            } catch (Exception x) {
                logger.error("File '{}' cannot be removed: {}", f, x);
            }
            return null;
        }
    };

    private static boolean directoryCouldBeALink(File dir, String someDirectoryEntry) throws IOException {
        return !new File(dir, someDirectoryEntry).getCanonicalFile().getParentFile().equals(dir);
    }

    public static void removeFile(File f) {
        boolean ok = f.delete();
        if (!ok  &&  f.exists())  throw new RuntimeException("File cannot be deleted: " + f);
    }

//    public static void renameFile(File f, String newName) {
//        renameFile(f, new File(f.getParentFile(), newName));
//    }

    public static void renameFile(File f, File newFile) {
        boolean ok = f.renameTo(newFile);
        if (!ok)  throw new RuntimeException("File cannot be renamed: "+f+"-->"+newFile);
    }

    public static void copyURLToFile(URL source, File destination) {
        try (InputStream in = source.openStream()) {
            try (OutputStream out = new FileOutputStream(destination)) {
                ByteStreams.copy(in, out);
            }
        } catch (IOException x) {
            throw new RuntimeException("copyURLToFile(): "+x+", from="+source+" to="+destination, x);
        }
        logger.info("copyURLToFile " + source + ", " + destination + " okay");
    }
}
