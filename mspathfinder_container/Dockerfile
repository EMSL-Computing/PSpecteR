# Dockerfile for MSPathFinderT docker container

FROM chambm/wine-dotnet:4.7-x64

ENV DEBIAN_FRONTEND noninteractive

#update wine repo key
RUN wget -nc https://dl.winehq.org/wine-builds/winehq.key
#RUN wget -nc http://archive.ubuntu.com/ubuntu/dists/xenial-updates/Release.gpg
RUN apt-key add winehq.key

RUN apt-get update && \
    apt-get -y install python3 python3-venv gunicorn vim redis-server


COPY InformedProteomics_1.0.7017.zip /tmp/.
RUN unzip /tmp/InformedProteomics_1.0.7017.zip -d /wineprefix64/drive_c/InformedProteomics_1.0.7017


#RUN useradd -d /home/flask_user -m flask_user
#WORKDIR /home/flask_user

RUN mkdir /flask
WORKDIR /flask

# RUN chown -R flask_user:flask_user /wineprefix64

COPY MspfPyServer/requirements.txt requirements.txt

RUN python3 -m venv venv
RUN venv/bin/pip install --upgrade pip
RUN venv/bin/pip install -r requirements.txt
RUN venv/bin/pip install gunicorn


COPY start.sh start.sh
RUN chmod +x start.sh

ENV FLASK_APP mspf_flask.app.py

COPY MspfPyServer MspfPyServer
#RUN chown -R flask_user:flask_user ./

COPY MSPathFinder_Mods.txt .

#USER flask_user

EXPOSE 5000
ENTRYPOINT ["/flask/start.sh"]