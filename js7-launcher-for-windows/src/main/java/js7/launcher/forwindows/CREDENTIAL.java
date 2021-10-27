package js7.launcher.forwindows;

import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.WString;
import com.sun.jna.platform.win32.WinBase.FILETIME;
import java.util.List;
import static java.util.Arrays.asList;

public final class CREDENTIAL extends Structure
{
    @SuppressWarnings("unused") public int flags;
    @SuppressWarnings("unused") public int typ;
    @SuppressWarnings("unused") public WString targetName;
    @SuppressWarnings("unused") public WString comment;
    @SuppressWarnings("unused") public FILETIME lastWritten;
    @SuppressWarnings("unused") public int credentialBlobSize;
    @SuppressWarnings("unused") public Pointer credentialBlob;
    @SuppressWarnings("unused") public int persist;
    @SuppressWarnings("unused") public int attributeCount;
    @SuppressWarnings("unused") public Pointer attributes;
    @SuppressWarnings("unused") public WString targetAlias;
    @SuppressWarnings("unused") public WString userName;


    public CREDENTIAL() {}

    public CREDENTIAL(Pointer p) {
        super(p);
    }

    @Override
    protected List<String> getFieldOrder() {
        return asList("flags", "typ", "targetName", "comment", "lastWritten",
            "credentialBlobSize", "credentialBlob", "persist", "attributeCount", "attributes",
            "targetAlias", "userName");
    }
}
