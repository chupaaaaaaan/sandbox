FROM maven:3.9.7-eclipse-temurin-17 as builder

ADD https://github.com/chupaaaaaaan/nablarch-example-mom-delayed-send.git#ibmmq-test /work

WORKDIR /work

RUN --mount=type=cache,target=/var/cache/apt/archives \
    apt update && apt -y install unzip
RUN --mount=type=cache,target=/root/.m2 \
    MAVEN_OPTS='--add-opens java.base/java.lang=ALL-UNNAMED' mvn clean package && \
    unzip -o target/application-5u24.zip


FROM icr.io/ibm-messaging/mq:9.3.0.17-r2

COPY --from=builder /work/lib /opt/sendapp/lib
COPY --from=builder /work/h2 /opt/sendapp/h2
COPY --from=builder /work/nablarch-example-mom-delayed-send-5u24.jar /opt/sendapp/

