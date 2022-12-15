FROM fredhutch/r-shiny-server-base:3.6.0
EXPOSE 3838
RUN rm -rf /srv/shiny-server
ADD . /srv/shiny-server/
WORKDIR /srv/shiny-server/
RUN chmod -R a+rw /srv/shiny-server
RUN chmod a+rw /srv/shiny-server/*.RData
RUN chown -R shiny:shiny /srv/shiny-server
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
CMD /usr/bin/shiny-server.sh
