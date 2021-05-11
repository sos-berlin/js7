package js7.executor.forwindows;

import com.sun.jna.Structure;
import com.sun.jna.WString;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import java.util.List;
import static java.util.Arrays.asList;

public final class PROFILEINFO extends Structure
{
    @SuppressWarnings("unused") public int dwSize;
    @SuppressWarnings("unused") public int flags;
    @SuppressWarnings("unused") public WString userName;
    @SuppressWarnings("unused") public WString profilePath;
    @SuppressWarnings("unused") public WString defaultPath;
    @SuppressWarnings("unused") public WString serverName;
    @SuppressWarnings("unused") public WString policyPath;
    @SuppressWarnings("unused") public HANDLE hProfile;

    protected List<String> getFieldOrder() {
        return asList("dwSize", "flags", "userName", "profilePath", "defaultPath",
            "serverName", "policyPath", "hProfile");
    }
}
