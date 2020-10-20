import configparser
import os
import re
import subprocess
import logging
from msgf_flask.config.log_config import *
from datetime import date

class MsgfRunner:
    def __init__(self, logger=None):
        #self.config(configFile)
        if logger:
            self.logger = logger
        #elif 'LOGGER_NAME' in self.config and 'LOG_CONFIG_FILE' in self.config:
        #    logging.config.fileConfig(self.config['LOG_CONFIG_FILE'])
        #    self.logger = logging.getLogger(self.config['LOGGER_NAME'])
        else:
            config = dict(COMMON_LOGGING_CONFIG)
            config['handlers'] = dict(console=CONSOLE_HANDLER)
            config['root']['handlers'] = ['console']
            dictConfig(config)
            self.logger = LOGGER
            self.logger.info("Console logging configured")

    def runMSGF(self, dirname, mzmlFileName, fastaFileName, paramsFileName, params=None):
        id = os.path.basename(dirname)
        self.logger.info("[%s] Running MSGF"%id)
        bname = os.path.splitext(os.path.basename(mzmlFileName))[0]

        cmd = ['java', '-jar', os.path.join("/", "flask", "MSGFPlus.jar"), '-s', mzmlFileName, '-d', fastaFileName,
               '-conf', paramsFileName, "-o", os.path.join(dirname, bname+".mzid")]
        self.logger.debug("[%s] MSGF+ command: %s"%(id, str(cmd)))

        # need to execute this from the directory where the mzml file exists
        process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                   universal_newlines=True, cwd=dirname)
        stdout, stderr = process.communicate() #waits for process to complete

        # MSGF+ always throws this error which can be ignored:
        # Failed to find library for L"Windows.Foundation.Diagnostics.AsyncCausalityTracer"
        if len(stderr) > 0 and not re.match('.*AsyncCausalityTracer.*', stderr):
            self.logger.error('[%s] MSGF+ error: %s'%(id, stderr))
            raise Exception('[%s] MSGF+ error: %s'%(id, stderr))

        # get output filenames
        try :
            mzidOutputFile = self.__getOutputFileName(dirname, bname, '.mzid', 'MSGF+')
        except Exception as err:
            self.logger.error('[%s] MSGF+ stdout: \n%s'%(id, stdout))
            self.logger.error('[%s] MSGF+ stderr: \n%s'%(id, stderr))
            raise err

        self.logger.debug("[%s] MSGF+ mzid output: %s"%(id, mzidOutputFile))

        return mzidOutputFile

    def __getOutputFileName(self, dirname, basename, extension, module):
        """
        Extension *should* have '.'
        """
        id = os.path.basename(dirname)
        outputFile = os.path.join(dirname, basename+extension)

        if not os.path.isfile(outputFile):
            files = [x for x in os.listdir(dirname) if re.match('.*'+extension+'$', x)]
            if len(files) == 0:
                self.logger.error('[%s] %s error: no output %s file found'%(id, module, extension))
                raise Exception('[%s] %s error: no %s output file found'%(id, module, extension))
            if len(files) == 1:
                outputFile = os.path.join(dirname, files[0])
            if len(files) > 1:
                self.logger.error('[%s] %s error: multiple %s output files found'%(id, module, extension))
                raise Exception('[%s] %s error: multiple %s output files found'%(id, module, extension))
        return outputFile

    def __paramsToStringList(self, params):
        result = []
        if not params:
            return result

        for (k,v) in params.items():
            if not k.startswith('-'):
                k = '-'+k
            result.append(str(k)+' '+str(v))

        return result