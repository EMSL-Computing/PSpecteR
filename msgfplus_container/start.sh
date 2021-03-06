#!/bin/bash
source venv/bin/activate
export PYTHONPATH=./MsgfPyServer:$PYTHONPATH
export GUNICORN_CMD_ARGS="--timeout 480"

exec redis-server  --port $REDIS_PORT &
exec celery --app=msgf_flask.celery_tasks worker --concurrency=1 --logfile=/flask/celery.log --loglevel=DEBUG &
exec gunicorn -b :$GUNICORN_PORT --access-logfile - --error-logfile - msgf_flask.app:app