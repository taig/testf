FROM        openjdk:8u151-jdk-alpine3.7

RUN         apk update
RUN         apk add --no-cache bash curl

# Install sbt
RUN         apk add --no-cache --virtual=build-dependencies
RUN         curl -Ls https://git.io/sbt > /usr/local/bin/sbt && chmod 0755 /usr/local/bin/sbt
RUN         apk del build-dependencies
ENV         JVM_OPTS "-Xms2G -Xmx4G -Xss2M"

# Cache sbt
RUN         mkdir -p \
              ./cache/project/ \
              ./cache/src/main/scala/
ADD         ./project/build.properties ./cache/project/
RUN         cd ./cache/ && sbt -v exit

# Cache scala
ADD         ./scalaVersion.sbt ./cache/
RUN         echo "class App" > ./cache/src/main/scala/App.scala
RUN         cd ./cache/ && sbt -v compile

# Cache plugins
ADD         ./project/plugins.sbt ./cache/project/
RUN         cd ./cache/ && sbt -v compile

# Cache dependencies
ADD         ./project ./cache/project/
ADD         ./build.sbt ./cache/
RUN         cd ./cache/ && sbt -v test:compile

# Clean cache
RUN         rm -r ./cache/

WORKDIR     /home/testf/
