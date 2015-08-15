############################################################
# Container that runs the Rolltime Forecast micro-service. #
# It needs a performant R instance. Receive links from DB. #
############################################################

FROM ubuntu:trusty

USER root

#
# Setting up basic Ubuntu
# machine with R.
#
RUN /
  && apt-key adv –keyserver keyserver.ubuntu.com –recv-keys E084DAB9 /
  && add-apt-repository ‘deb https://cran.cnr.Berkeley.edu/bin/linux/ubuntu trusty/’
  && apt-get update /
  && apt-get upgrade /
  && apt-get install r-base r-base-dev /
  && apt-get install git

#
# Cloning repository and
# setting-up the application.
#
RUN /
  && git clone http://github.com/rolltime/rolltime-forecast /
  && cd rolltime-forecast /
  && make setup

WORKDIR "/rolltime-forecast"

EXPOSE 6000

CMD ["make", "run"]

