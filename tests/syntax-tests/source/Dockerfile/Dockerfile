ARG architecture=amd64
FROM $architecture/centos:7
LABEL com.example.version="0.2.1-beta"
ARG architecture

ENV INTERESTING_PATH /usr/bin/interesting-software


COPY entrypoint.sh /usr/bin/entrypoint.sh

RUN if [ "$architecture" = "i386" ]; then echo "Building i386 docker image" && \
    linux32 yum update -y && linux32 yum install -y mysql ; \
    else yum update -y && yum install -y mysql

EXPOSE 80/tcp

VOLUME [/var/lib/mysql/data]

ENTRYPOINT ["/usr/bin/entrypoint.sh"]
