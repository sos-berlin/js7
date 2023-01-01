FROM almalinux:9

# Directory structure according to the Filesystem Hierarchy Standard, https://refspecs.linuxfoundation.org/FHS_3.0/fhs-3.0.pdf
#   /opt/js7-VERSION/bin/
#   /opt/js7-VERSION/lib/
#   /opt/js7 -> js7-VERSION
#   /var/opt/js7/{controller,agent}/config/  read-only
#   /var/opt/js7/{controller,agent}/data/{cache,logs,state,work,...}  writable
#   /home/js7/bin

ENV LANG=C.UTF-8
ENV JAVA_HOME=/opt/java/openjdk
ENV PATH="${JAVA_HOME}/bin:${PATH}"
COPY --from=eclipse-temurin:19 $JAVA_HOME $JAVA_HOME
# Own Docker image layer with downloaded microdnf metadata (may take several seconds)
RUN yum -y install procps git unzip which sudo openssl
# Until here, same Dockerfile command sequence as in build-with-docker for shorter build time
#———————————————————————————————————————————————————————————————————————————————

# UID of the 'js7'. The mounted volume /var/opt/js7 must be rwx for this UID
# Used as group ID, too.
ARG UID

RUN \
  yum -y install less findutils which &&\
  yum -y clean packages

# A tiny improvement of the Java runtime:
#RUN $JAVA_HOME/bin/jlink --no-header-files --no-man-pages \
#    --add-modules java.base,java.compiler,java.desktop,java.logging,java.management,java.security.jgss,java.sql,java.xml,jdk.management,jdk.unsupported \
#    --output /opt/js7-java
#ENV JAVA_HOME /opt/js7-java

# To find libjdwp.so for --debug-port=
ENV LD_LIBRARY_PATH "$JAVA_HOME/lib"

ADD files /
ADD js7-*install-[0-9]*.tgz /opt/

RUN chown root:root -R /opt/js7-[1-9]* && \
    chmod o-w -R /opt/js7-[1-9]* && \
    ln -s /opt/js7-[1-9]* /opt/js7 && \
    groupadd --gid ${UID:-1000} js7 && \
    useradd --uid ${UID:-1000} --gid js7 --home-dir /home/js7 --no-create-home --shell /bin/bash js7 && \
    mkdir -p /var/opt/js7/{provider,cluster-watch,controller,agent}/data &&\
    mkdir -p /var/opt/js7/testControllerAgent && \
    chown js7:js7 -R /home/js7 /var/opt/js7

# ~js7/bin contains scripts controller and agent, useable for "docker run".
ENV PATH /home/js7/bin:/usr/local/bin:/usr/bin:/bin
USER js7

# Possible start:
# docker run --publish=4444:4444 -ti --tag=js7-subagent js7 js7-subagent
