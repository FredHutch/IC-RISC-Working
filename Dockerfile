FROM fredhutch/r-shiny-server-base:latest
EXPOSE 3838
RUN rm -rf /srv/shiny-server
ADD . /srv/shiny-server/01_hello
WORKDIR /srv/shiny-server/01_hello
RUN chmod -R a+rw /srv/shiny-server
RUN chmod a+rw /srv/shiny-server/01_hello/*.RData
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
CMD /usr/bin/shiny-server.sh
