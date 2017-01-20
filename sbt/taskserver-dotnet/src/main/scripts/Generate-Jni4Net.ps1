# .SYNOPSIS
#     Generate .NET and Java proxy files, JobScheduler .NET adapter dll and copy this and the jni4net .NET dlls to the given locations
# .DESCRIPTION
#     - generates two proxy files to the given locations and use "JarBaseName" basename as the proxy basename
#       1 - .jn4-<version>.dll
#       2 - .jn4-<version>.jar
#     - generates the JobScheduler .NET adapter .dll (com.sos-berlin.engine-taskserver-dotnet.dll)
#     - copies jni4net .NET dll files from the "proxygen" directory to the DLL location
#
#     Environment variables WINDOWS_NET_SDK_HOME with the installation path of Micosoft Windows SDK (.Net-SDK) is needed,
#     for example: set WINDOWS_NET_SDK_HOME=%windir%\Microsoft.NET\Framework\v4.0.30319

$ErrorActionPreference = 'Stop'  # PowerShell preference

# ----------------------------------------------------------------------
# Settings
# ----------------------------------------------------------------------
# Directory with C# source files
$CSharpSourceDirectory = "src/main/dotnet"

# Directory with Java classes to be proxied
$JobApiClassesDirectory = "target\jni4net-input\javaClasses"

# Directory with extracted jni4net.zip distribution
$Jni4Net = "target\jni4net"

# Directory with extracted jni4net forked distribution
$Jni4NetForked = "target\jni4net_forked"

# Target DLL directory
$ProxyDllResultDirectory = mkdir 'target/classes/com/sos/scheduler/engine/taskserver/dotnet/dlls' -force

# Target .Net adapter DLL path, same name as in DotnetDlls.scala
$ResultAdapterAssemblyDll = Join-Path $ProxyDllResultDirectory "com.sos-berlin.engine.engine-job-api-dotnet.dll"  # Same name as in AssemblyInfo.cs

$BuildDirectory = mkdir "target/jni4net-build" -force
$Jni4NDllName   = "jni4net.n-0.8.8.0.dll"
$Jni4NetDlls    = @($Jni4NDllName, "jni4net.n.w32.v40-0.8.8.0.dll", "jni4net.n.w64.v40-0.8.8.0.dll")
$WindowsSDK     = ${env:WINDOWS_NET_SDK_HOME}
# ----------------------------------------------------------------------

function ExecuteCommand([string] $command, [Array]$arguments) {
    $process = Start-Process $command -NoNewWindow -Wait -PassThru -ArgumentList $arguments
    if ($process.exitCode -ne 0) {
        throw "Command failed with exit code $($process.exitCode): $command"
    }
}

# GenerateProxyJarAndDll
$jar = Join-Path $BuildDirectory "java-classes-for-dotnet.jar"
ExecuteCommand "${env:JAVA_HOME}\bin\jar" @("cf", """$jar""", "-C", """$JobApiClassesDirectory""", "sos/spooler")
ExecuteCommand "$Jni4Net\bin\proxygen.exe" @("""$jar""", "-wd", """$BuildDirectory""")

# CopyJni4NetDlls
$Jni4NetDlls | foreach {
    Copy-Item "$Jni4Net\lib\$_" $ProxyDllResultDirectory
}
# Overwrite the original jni4net.n DLL with the forked version
Copy-Item (join-path $Jni4NetForked $Jni4NDllName) $ProxyDllResultDirectory -force

# We simply mix our C# files with the generated ones to get a single DLL
Copy-Item $CSharpSourceDirectory (join-path $BuildDirectory "clr") -recurse -force
# Replace a generated with the proxygen tool java.lang.String parameter and return value type to a .NET string type
$csharpJobApiBuildDirectory = (join-path $BuildDirectory "clr\sos\spooler")
Get-ChildItem -Path $csharpJobApiBuildDirectory | Where {!$_.PSIsContainer -and $_.extension -eq ".cs"} | Foreach-Object {
     $content = [System.IO.File]::ReadAllText($_.FullName)
     $content = $content.replace('global::java.lang.String ','string ')
     $content = $content.replace('java.lang.String ','string ')
     $content = $content.replace("global::net.sf.jni4net.utils.Convertor.ParStrongCp2J(","sos.spooler.JniValueConverter.ParStrongCp2J(")

     $newFile = Join-Path -Path $csharpJobApiBuildDirectory -ChildPath $_.Name
     if(Test-Path($newFile)){
         Remove-Item $newFile -Force
     }
     [System.IO.File]::WriteAllText($newFile, $content);
}
# CompileCSharp
$powershellRef = [PsObject].Assembly.Location
ExecuteCommand "$WindowsSDK/csc" @("/nologo", "/warn:0", "/t:library", "/out:$ResultAdapterAssemblyDll",
                                   "/recurse:""$BuildDirectory\clr\*.cs""",
                                   "/reference:$powershellRef;""$ProxyDllResultDirectory\$Jni4NDllName""")
