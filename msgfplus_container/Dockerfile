## David Degnan, Pacific Northwest National Laboratory
## Last Updated: 2020_10_06
## Dockerfile for MSGF+ docker container

# Install python version 3.8
FROM python:3.8-buster

# Install Ubuntu Dependencies
RUN apt-get update && \
    apt-get -y install python3 python3-venv gunicorn vim redis-server default-jre

# Make Directory for Flask App
RUN mkdir /flask
WORKDIR /flask

# Copy and install python dependencies
COPY MsgfPyServer/requirements.txt requirements.txt
RUN python3 -m venv venv
RUN venv/bin/pip install --upgrade pip
RUN venv/bin/pip install -r requirements.txt
RUN venv/bin/pip install gunicorn

# Make start script with redis and celery workers executable
COPY start.sh start.sh
RUN chmod +x start.sh

# Set Flask App Environment
ENV FLASK_APP msgf_flask.app.py

# Copy MsgfPyServer and MSGFPlus to main container directory
COPY MsgfPyServer MsgfPyServer
COPY MSGFPlus.jar MSGFPlus.jar

# Expose and launch app
EXPOSE 5000
ENTRYPOINT ["/flask/start.sh"]