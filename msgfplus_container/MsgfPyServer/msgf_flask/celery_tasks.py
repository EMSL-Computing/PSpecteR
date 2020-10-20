import zipfile
import os
import logging
from logging.config import dictConfig
from celery import Celery
from celery.states import STARTED, FAILURE
from celery.utils.log import get_task_logger
from msgf_flask.msgf_runner import MsgfRunner
from msgf_flask.config.celery_config import Config

# celery object for task queueing
clry = Celery()
clry.config_from_object(Config)

logger = get_task_logger(__name__)

runner = MsgfRunner()

@clry.task(bind=True, name='runFullProcess')
def runFullProcess(self, outputDirName, mzmlFile, fastaFile, paramsFile, zipResults=True, resultsInStatus=False):
    """
    Run MSGF+ in sequence and zip results for download - run MSGF+ in this command 
    """

    logger.debug("celery conf: " + str(clry.conf))

    self.update_state(state=STARTED)

    try:
        id = os.path.basename(outputDirName)
        logger.info('[%s] Starting MSGF+'%id)
        self.update_state(state=STARTED)
        mzidOutputFile = runner.runMSGF(outputDirName, mzmlFile, fastaFile, paramsFile)
        logger.debug("[%s] Finished" % id)
        return mzidOutputFile

    except Exception as err :
        logger.error('Error: %s' % ('\n'.join(map(str, err.args))))
        raise err