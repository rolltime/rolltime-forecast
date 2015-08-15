############################################################
# Container that runs the Rolltime Forecast micro-service. #
# It needs a performant R instance. Receive links from DB. #
############################################################

FROM rocker/r-devel:latest

RUN /
  git clone https://github.com/rolltime/rolltime-forecast /
  && cd rolltime-forecast /
  && make setup

WORKDIR "/rolltime-forecast"

EXPOSE 6000

CMD ["make", "run"]

