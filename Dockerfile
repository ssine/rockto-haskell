FROM debian

WORKDIR /data

COPY rockto /data/rockto
COPY resources /data/resources

CMD /data/rockto
