FROM rixed/lurch-docker-in-docker

RUN DEBIAN_FRONTEND=noninteractive \
      apt-get install --yes \
        cgroup-tools \
        lighttpd \
        lighttpd-mod-deflate \
        postgresql

RUN mkdir /lurch
COPY www/ /lurch/www/
RUN ln -s /lurch/www/lurch /usr/bin/lurch
COPY busybox /usr/local/bin/busybox
COPY lighttpd.conf /lurch
COPY lighttpd.pwd /lurch
COPY schema.sql /lurch
COPY populate /lurch
COPY start /
COPY pg_hba.conf /etc/postgresql/13/main/

ENV BUSYBOX=/usr/local/bin/busybox
ENV LURCH_DB="user=postgres dbname=lurch"

EXPOSE 80/TCP

WORKDIR /lurch
ENTRYPOINT [ "/start" ]

LABEL maintainer="rixed@happyleptic.org"
