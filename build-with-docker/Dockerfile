FROM almalinux:9

ENV LANG=C.UTF-8

ENV JAVA_HOME=/opt/java/openjdk
ENV PATH="${JAVA_HOME}/bin:${PATH}"
COPY --from=eclipse-temurin:8u392-b08-jdk-centos7 $JAVA_HOME $JAVA_HOME
# Own Docker image layer with downloaded microdnf metadata (may take several seconds)
RUN yum -y install procps git unzip which sudo openssl
# Until here, same Dockerfile command sequence as in JS7 Docker example for shorter build time
#———————————————————————————————————————————————————————————————————————————————

RUN \
  yum -y install nodejs &&\
  yum clean packages

RUN \
  sbtVersion=1.9.7 &&\
  mkdir --parents /opt/lib/sbt-$sbtVersion &&\
  curl --location https://github.com/sbt/sbt/releases/download/v$sbtVersion/sbt-$sbtVersion.tgz |\
    tar xzf - --directory=/opt/lib/sbt-$sbtVersion --strip-components=1 sbt/ &&\
  ln -snf /opt/lib/sbt-$sbtVersion/bin/sbt /usr/local/bin/sbt

RUN \
  adduser --comment "JS7 build" --no-create-home build &&\
  mkdir --parents /home/build/mount/{.cache,.ivy2,.npm,.sbt} &&\
  ln -s /home/build/mount/{.cache,.ivy2,.npm,.sbt} /home/build/ &&\
  mkdir /home/build/js7 &&\
  chown -R build.build /home/build/

# echo "build ALL=(ALL) NOPASSWD: ALL" >>/etc/sudoers

USER build
WORKDIR /home/build/js7
