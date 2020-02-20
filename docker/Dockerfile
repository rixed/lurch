FROM rixed/lurch-docker-in-docker

RUN DEBIAN_FRONTEND=noninteractive \
      apt-get install --yes \
        cgroup-tools \
        lighttpd \
        postgresql

RUN mkdir /lurch
COPY www/ /lurch/www/
# FIXME:
COPY /home/rixed/bin/busybox /home/rixed/bin/busybox
COPY lighttpd.conf /lurch
COPY schema.sql /lurch
COPY start /

WORKDIR /lurch
ENTRYPOINT [ "/usr/sbin/lighttpd", "-f", "/lurch/lighttpd.conf", "-D" ]

EXPOSE 80/TCP

LABEL maintainer="rixed@happyleptic.org"
