FROM fredhutch/r-shiny-base:latest

RUN apt-get update
RUN useradd -u 5555 -m -d /home/shiny -c "shiny user" shiny
ADD . /home/shiny/
RUN chown -R shiny:shiny /home/shiny 
WORKDIR /home/shiny
USER shiny 
EXPOSE 7777
CMD Rscript start.R 
