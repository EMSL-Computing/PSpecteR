#!/bin/bash
source venv/bin/activate
export PYTHONPATH=./MspfPyServer:$PYTHONPATH
export GUNICORN_CMD_ARGS="--timeout 480"

exec redis-server &
exec celery worker --app=mspf_flask.celery_tasks --concurrency=1 --logfile=/flask/celery.log --loglevel=DEBUG &
exec gunicorn -b :5000 --access-logfile - --error-logfile - mspf_flask.app:app