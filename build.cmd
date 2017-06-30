@echo off
setlocal

rem Before starting this script, change into this directory!

if "%WINDOWS_NET_SDK_HOME%" == "" (
  set WINDOWS_NET_SDK_HOME=%windir%\Microsoft.NET\Framework\v4.0.30319
  rem echo Using WINDOWS_NET_SDK_HOME=%WINDOWS_NET_SDK_HOME%
)

set JAVA_OPTS=-XX:+CMSClassUnloadingEnabled -XX:MaxMetaspaceSize=512M -XX:MetaspaceSize=256M -Xms2G -Xmx2G
sbt "; clean; build"
endlocal
