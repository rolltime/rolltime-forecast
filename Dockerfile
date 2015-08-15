############################################################
# Container that runs the Rolltime Forecast micro-service. #
# It needs a performant R instance. Receive links from DB. #
############################################################

FROM ubuntu:trusty

#
# Setting up basic Ubuntu
# machine with R.
#
RUN /
  sudo apt-key adv –keyserver keyserver.ubuntu.com –recv-keys E084DAB9 /
  && sudo add-apt-repository ‘deb https://cran.cnr.Berkeley.edu/bin/linux/ubuntu trusty/’
  && sudo apt-get update /
  && sudo apt-get upgrade /
  && sudo apt-get install r-base r-base-dev /
  && sudo apt-get install git

#
# Cloning repository and
# setting-up the application.
#
RUN /
  git clone http://github.com/rolltime/rolltime-forecast /
  && cd rolltime-forecast /
  && make setup

WORKDIR "/rolltime-forecast"

EXPOSE 6000

CMD ["make", "run"]

