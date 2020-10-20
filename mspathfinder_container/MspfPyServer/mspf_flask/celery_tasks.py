import zipfile
import os
import logging
from logging.config import dictConfig
from celery import Celery
from celery.states import STARTED, FAILURE
from celery.utils.log import get_task_logger
from mspf_flask.mspf_runner import MspfRunner
from mspf_flask.config.celery_config import Config

# celery object for task queueing
clry = Celery()
clry.config_from_object(Config)

# clry.conf.update(
#     broker_url='redis://localhost:6379/0',
#     result_backend='redis://localhost:6379/0',
#     result_serializer = 'json',
#     # result_extended = True,
#     task_ignore_result = False,
#     EXE_DIR='/wineprefix64/drive_c/InformedProteomics_1.0.7017'
# )

logger = get_task_logger(__name__)

# runner = MspfRunner(clry.conf['EXE_DIR'])
runner = MspfRunner(clry.conf['EXE_DIR'])

@clry.task(bind=True, name='runFullProcess')
def runFullProcess(self, outputDirName, mzmlFile, fastaFile, modsFile, minCharge, maxCharge, minMass, maxMass, minLength, maxLength,
                   minFragCharge, maxFragCharge, precursorTol, fragTol, dbSearchMode, actMethod, zipResults=True, resultsInStatus=False):
    """
    Run PbfGen, ProMex and MSPathFinderT in sequence and zip results for download
    """
    logger.debug("celery conf: " + str(clry.conf))

    self.update_state(state=STARTED)

    try :
        id = os.path.basename(outputDirName)
        logger.info('[%s] Starting PbfGen'%id)
        # print('[%s] Starting PbfGen'%id)
        self.update_state(state=STARTED, meta={'stage':'PbfGen'})
        pbfFile = runner.runPbfGen(outputDirName, mzmlFile)
        logger.info('[%s] Starting ProMex'%id)
        # print('[%s] Starting ProMex'%id)
        self.update_state(state=STARTED, meta={'stage':'ProMex'})
        ms1ftFile = runner.runProMex(outputDirName, pbfFile, params={'minCharge':int(minCharge), 'maxCharge':int(maxCharge),
                                                                     'minMass':int(minMass), 'maxMass':int(maxMass)})
        logger.info('[%s] Starting MSPathFinderT'%id)
        print('[%s] Starting MSPathFinderT'%id)
        self.update_state(state=STARTED, meta={'stage':'MSPathFinderT'})
        mzidOutputFile, tsvOutputFile = runner.runMSPathFinderT(outputDirName, pbfFile,
                                                            ms1ftFile, fastaFile, modsFile,
                                                            params={'minLength':minLength, 'maxLength':maxLength,
                                                            'minCharge':minCharge, 'maxCharge':maxCharge,
                                                            'minMass':minMass,'maxMass':maxMass,
                                                            'minFragCharge':minFragCharge, 'maxFragCharge':maxFragCharge ,
                                                            't':precursorTol, 'f':fragTol, 'tda':dbSearchMode,
                                                            'act':actMethod
                                                            })

        if zipResults:
            logger.info('[%s] Zipping results'%id)
            # print('[%s] Zipping results'%id)
            self.update_state(state=STARTED, meta={'stage':'Zipping results'})
            # zip results together and return
            zipFileName = os.path.join(outputDirName, 'MSPathFinderT_results.zip')
            zipObj = zipfile.ZipFile(zipFileName, 'w')
            # zipObj.write(pbfFile, os.path.basename(pbfFile))
            zipObj.write(ms1ftFile, os.path.basename(ms1ftFile))
            zipObj.write(mzidOutputFile, os.path.basename(mzidOutputFile))
            zipObj.write(tsvOutputFile, os.path.basename(tsvOutputFile))
            zipObj.close()
            logger.debug("[%s] Finished" % id)
            if resultsInStatus:
                return zipFileName
        elif resultsInStatus:
            logger.debug("[%s] Finished" % id)
            return outputDirName
        else:
            logger.debug("[%s] Finished" % id)
            return "Finished successfully"

    except Exception as err :
        logger.error('Error: %s' % ('\n'.join(map(str, err.args))))
        # TODO: remove [mspf_...] from start of error message?
        #self.update_state(state=FAILURE, meta={'message':'\n'.join(map(str, err.args))})
        raise err
