import configparser
import os
import re
import subprocess
import logging
from mspf_flask.config.log_config import *

class MspfRunner:
    def __init__(self, exe_dir, logger=None):
        # self.config(configFile)
        self.config = {'EXE_DIR':exe_dir}
        if logger:
            self.logger = logger
        elif 'LOGGER_NAME' in self.config and 'LOG_CONFIG_FILE' in self.config:
            logging.config.fileConfig(self.config['LOG_CONFIG_FILE'])
            self.logger = logging.getLogger(self.config['LOGGER_NAME'])
        else:
            config = dict(COMMON_LOGGING_CONFIG)
            config['handlers'] = dict(console=CONSOLE_HANDLER)
            config['root']['handlers'] = ['console']
            dictConfig(config)
            self.logger = LOGGER
            self.logger.info("Console logging configured")

    # def __parseConfig(self, configFile):
    #     ## parse config file
    #     cfgObj = configparser.RawConfigParser()
    #     cfgObj.read(configFile)
    #
    #     self.config = {}
    #
    #     self.config['EXE_DIR'] = cfgObj.get('EXE', 'EXE_DIR')


    def runPbfGen(self, dirname, mzmlFileName):
        id = os.path.basename(dirname)
        self.logger.info("[%s] Running PbfGen"%id)
        bname = os.path.splitext(os.path.basename(mzmlFileName))[0]
        cmd = ['wine', os.path.join(self.config['EXE_DIR'], 'PbfGen.exe'), '-s %s' % mzmlFileName, '-o .']
        self.logger.debug("[%s] PbfGen command: %s"%(id, str(cmd)))
        # https://janakiev.com/blog/python-shell-commands/

        # need to execute this from the directory where the mzml file exists
        process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                   universal_newlines=True, cwd=dirname)
        stdout, stderr = process.communicate()  # waits for process to complete
        if len(stderr) > 0:
            self.logger.error('[%s] PbfGen error: %s'%(id, stderr))
            raise Exception('[%s] PbfGen error: %s'%(id, stderr))

        try:
            outputFile = self.__getOutputFileName(dirname, bname, 'pbf', 'PbfGen')
        except Exception as err:
            self.logger.error('[%s] PbfGen stdout: \n%s'%(id, stdout))
            self.logger.error('[%s] PbfGen stderr: \n%s'%(id, stderr))
            raise err

        self.logger.debug("[%s] PbfGen output: %s" % (id, outputFile))
        return outputFile


    def runProMex(self, dirname, pbfFileName, params=None):
        id = os.path.basename(dirname)
        self.logger.info("[%s] Running ProMex"%id)
        bname = os.path.splitext(os.path.basename(pbfFileName))[0]
        cmd = ['wine', os.path.join(self.config['EXE_DIR'], 'ProMex.exe'), '-i %s'%pbfFileName, '-o .',
               '-score n', '-csv n', '-featureMap n'] + self.__paramsToStringList(params)
        self.logger.debug("[%s] ProMex command: %s"%(id, str(cmd)))
        # https://janakiev.com/blog/python-shell-commands/

        # need to execute this from the directory where the mzml file exists
        process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                   universal_newlines=True, cwd=dirname)
        stdout, stderr = process.communicate() #waits for process to complete

        # ProMex always throws this error which can be ignored:
        # Failed to find library for L"Windows.Foundation.Diagnostics.AsyncCausalityTracer"
        if len(stderr) > 0 and not re.match('.*AsyncCausalityTracer.*', stderr):
            self.logger.error('[%s] ProMex error: %s'%(id, stderr))
            raise Exception('[%s] ProMex error: %s'%(id, stderr))

        try:
            outputFile = self.__getOutputFileName(dirname, bname, 'ms1ft', 'ProMex')
        except Exception as err:
            self.logger.error('[%s] ProMex stdout: \n%s'%(id, stdout))
            self.logger.error('[%s] ProMex stderr: \n%s'%(id, stderr))
            raise err

        self.logger.debug("[%s] ProMex output: %s"%(id, outputFile))
        return outputFile


    def runMSPathFinderT(self, dirname, pbfFileName, ms1ftFileName, fastaFileName, modsFile, params=None):
        id = os.path.basename(dirname)
        self.logger.info("[%s] Running MSPathFinderT"%id)
        bname = os.path.splitext(os.path.basename(pbfFileName))[0]
        cmd = ['wine', os.path.join(self.config['EXE_DIR'], 'MSPathFinderT.exe'), '-s %s'%pbfFileName,
               '-feature %s'%ms1ftFileName, '-d %s'%fastaFileName, '-o .',
               '-mod %s'%modsFile] + self.__paramsToStringList(params)
        # if paramFile:
        #     cmd.append("$(<%s)"%paramFile)
        self.logger.debug("[%s] MSPathFinderT command: %s"%(id, str(cmd)))

        # need to execute this from the directory where the mzml file exists
        process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                   universal_newlines=True, cwd=dirname)
        stdout, stderr = process.communicate() #waits for process to complete

        # MSPathFinderT always throws this error which can be ignored:
        # Failed to find library for L"Windows.Foundation.Diagnostics.AsyncCausalityTracer"
        if len(stderr) > 0 and not re.match('.*AsyncCausalityTracer.*', stderr):
            self.logger.error('[%s] MSPathFinderT error: %s'%(id, stderr))
            raise Exception('[%s] MSPathFinderT error: %s'%(id, stderr))

        # get output filenames
        try :
            mzidOutputFile = self.__getOutputFileName(dirname, bname, '.mzid', 'MSPathFinderT')
        except Exception as err:
            self.logger.error('[%s] MSPathFinderT stdout: \n%s'%(id, stdout))
            self.logger.error('[%s] MSPathFinderT stderr: \n%s'%(id, stderr))
            raise err

        self.logger.debug("[%s] MSPathFinderT mzid output: %s"%(id, mzidOutputFile))

        tsvOutputFile = self.__getOutputFileName(dirname, bname, '_IcTarget.tsv', 'MSPathFinderT')
        self.logger.debug("[%s] MSPathFinderT tsv output: %s"%(id, tsvOutputFile))

        return mzidOutputFile, tsvOutputFile


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
