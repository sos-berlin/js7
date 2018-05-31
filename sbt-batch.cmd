@echo off
setlocal

if "%WINDOWS_NET_SDK_HOME%" == "" (
  set WINDOWS_NET_SDK_HOME=%windir%\Microsoft.NET\Framework\v4.0.30319
)

rem set SBT_OPTS=
set JAVA_OPTS=-Xmx2500M -XX:MaxMetaspaceSize=1500M -XX:+CMSClassUnloadingEnabled -Dlog4j.configurationFile=project/log4j2.xml -Dsbt.log.noformat=true %JAVA_OPTS%
sbt.bat %*
endlocal
