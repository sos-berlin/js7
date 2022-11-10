FROM openjdk:8-oraclelinux8

ENV LANG=C.UTF-8
# Own Docker image layer with downloaded microdnf metadata (may take several seconds)
RUN microdnf install procps
# Until here, same Dockerfile command sequence as in JS7 Docker example for shorter build time
#———————————————————————————————————————————————————————————————————————————————

RUN \
  sbtVersion=1.7.3 &&\
  \
  : microdnf upgrade `# may take several minutes` &&\
  microdnf install git unzip which sudo openssl nodejs &&\
  microdnf clean all &&\
  \
  mkdir --parents /opt/lib/sbt-$sbtVersion &&\
  curl --location https://github.com/sbt/sbt/releases/download/v$sbtVersion/sbt-$sbtVersion.tgz |\
    tar xzf - --directory=/opt/lib/sbt-$sbtVersion --strip-components=1 sbt/ &&\
  ln -snf /opt/lib/sbt-$sbtVersion/bin/sbt /usr/local/bin/sbt

RUN \
  adduser --comment "JS7 build" --no-create-home build &&\
  mkdir --parents /home/build/mount/{.cache,.ivy2,.npm,.sbt} &&\
  ln -s /home/build/mount/{.cache,.ivy2,.npm,.sbt} /home/build/ &&\
  chown -R build.build /home/build/ &&\
  : echo "build ALL=(ALL) NOPASSWD: ALL" >>/etc/sudoers

USER build
WORKDIR /home/build/js7
