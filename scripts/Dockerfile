from ubuntu

RUN mkdir -p /home/jappie
RUN useradd jappie
RUN chown jappie:100 /home/jappie

RUN apt-get update
RUN apt-get -y install curl
RUN curl -sSL https://get.haskellstack.org/ | sh 

VOLUME /home/jappie
WORKDIR /home/jappie
USER jappie:100

CMD ["scripts/package-deb.sh"]