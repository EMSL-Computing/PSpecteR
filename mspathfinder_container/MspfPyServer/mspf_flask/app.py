import os
import shutil
import sys
import re
import tempfile
from datetime import date

from celery.result import AsyncResult
from flask import Flask, request, Response, send_file
from flask.json import jsonify, loads
from flask.logging import default_handler
import logging
import traceback
from werkzeug.utils import secure_filename
import subprocess
import zipfile
from logging.config import dictConfig
from celery.states import SUCCESS

from mspf_flask.config.log_config import *
from mspf_flask.mspf_runner import MspfRunner
from mspf_flask.celery_tasks import runFullProcess, clry

######################################################################
## APP SETUP AND CONFIG

# LOGGING needs to be set up before app is created
# If MSPF_SERVER_LOG_CONFIG is used it needs to be this format: https://docs.python.org/3.6/library/logging.config.html#logging-config-fileformat
if 'MSPF_SERVER_LOG_CONFIG' in os.environ:
    print('Configuring logging from config file %s' % os.environ['MSPF_SERVER_LOG_CONFIG'])
    logging.config.fileConfig(os.environ['MSPF_SERVER_LOG_CONFIG'])
else:
    print('Configuring console logger')
    config = dict(COMMON_LOGGING_CONFIG)
    config['handlers'] = dict(console=CONSOLE_HANDLER)
    config['root']['handlers'] = ['console']
    dictConfig(config)


try :
    app = Flask(__name__)
    app.config.from_object('mspf_flask.config.flask_config.Config')
    # if FLASK_SERVER_SETTINGS exists config is loaded and overwrites above
    # FLASK_SERVER_SETTINGS can be .cfg or .py
    # see http://flask.pocoo.org/docs/1.0/config/#configuring-from-files
    app.config.from_envvar('FLASK_SERVER_SETTINGS', silent=True)

    # remove default console handler from flask app logger (will revert to root handler defined in config.log_config)
#     if 'MSPF_SERVER_LOG_CONFIG' in os.environ:
    app.logger.removeHandler(default_handler)

    app.logger.info(app.config)

    runner = MspfRunner(app.config['EXE_DIR'], app.logger)

except Exception as err:
    print(err) #prints to console / apache error log
    app.logger.error(err)
    app.logger.error(traceback.format_stack())
    sys.exit(-1)


######################################################################
## APP ENDPOINTS

@app.route('/')
def hello_world():
    return "It's alive!"

@app.route('/MSPathFinderT', methods=['PUT'])
def runMSPathFinderT() :
    app.logger.debug(str(request))
    app.logger.debug('request.form: %s'+str(request.form))
    app.logger.debug('request.files keys: %s' % str([x for x in request.files.keys()]))

    # get parameters from request
    mzmlFile = __validateParamFileName(request.args.get('mzmlFile', ''))
    if not mzmlFile:
        return __invalidParameterResponse('mzmlFile', request.args.get('mzmlFile', ''))
    fastaFile = __validateParamFileName(request.args.get('fastaFile', ''))
    if not fastaFile:
        return __invalidParameterResponse('fastaFile', request.args.get('fastaFile', ''))
    modsFile = __validateParamFileName(request.args.get('modsFile', ''))
    if not modsFile:
        modsFile = app.config['MODS_FILE']

    # create new subdirectory where mzml file is to store outputs
    datadir = tempfile.mkdtemp(prefix='mspf_'+date.today().strftime("%Y-%m-%d")+'_', dir=os.path.dirname(mzmlFile))
    # datadir = os.path.dirname(mzmlFile)

    minCharge = int(request.args.get('minCharge', default=app.config['DEFAULT_MIN_CHARGE']))
    maxCharge = int(request.args.get('maxCharge', default=app.config['DEFAULT_MAX_CHARGE']))
    minMass = int(request.args.get('minMass', default=app.config['DEFAULT_MIN_MASS']))
    maxMass = int(request.args.get('maxMass', default=app.config['DEFAULT_MAX_MASS']))
    minLength = int(request.args.get('minLength', default=app.config['DEFAULT_MIN_LENGTH']))
    maxLength = int(request.args.get('maxLength', default=app.config['DEFAULT_MAX_LENGTH']))
    minFragCharge = int(request.args.get('minFragCharge', default=app.config['DEFAULT_MIN_FRAG_CHARGE']))
    maxFragCharge = int(request.args.get('maxFragCharge', default=app.config['DEFAULT_MAX_FRAG_CHARGE']))
    precursorTol = int(request.args.get('t', default=app.config['DEFAULT_PRECURSOR_TOLERANCE']))
    fragTol = int(request.args.get('f', default=app.config['DEFAULT_FRAGMENT_ION_TOLERANCE']))
    dbSearchMode = int(request.args.get('tda', default=app.config['DEFAULT_DATABASE_SEARCH_MODE']))
    actMethod = int(request.args.get('act', default=app.config['DEFAULT_ACTIVIATION_METHOD']))


    job = runFullProcess.s(datadir, mzmlFile, fastaFile, modsFile, minCharge, maxCharge, minMass, maxMass, minLength, maxLength,
                   minFragCharge, maxFragCharge, precursorTol, fragTol, dbSearchMode, actMethod, zipResults=False, resultsInStatus=True)
    asRes = job.delay()
    # asRes.save(clry.backend)
    return jsonify(taskID=asRes.id)


@app.route('/MSPathFinderTForm', methods=['POST'])
def runMSPathFinderTFromForm() :
    app.logger.debug(str(request))
    app.logger.debug('request.form: %s'+str(request.form))
    app.logger.debug('request.files keys: %s' % str([x for x in request.files.keys()]))

    tmpdir = tempfile.mkdtemp(prefix = 'mspf_', dir=app.config['UPLOAD_DIRECTORY'])

    mzmlFile = __saveUploadedFile(request.files, 'mzmlFile', tmpdir)
    fastaFile = __saveUploadedFile(request.files, 'fastaFile', tmpdir)
    # modsFile = __saveUploadedFile(request.files, 'modsFile', tmpdir)
    modsFile = app.config['MODS_FILE']

    # get parameters from form fields
    minCharge = int(request.form.get('minCharge', default=app.config['DEFAULT_MIN_CHARGE']))
    maxCharge = int(request.form.get('maxCharge', default=app.config['DEFAULT_MAX_CHARGE']))
    minMass = int(request.form.get('minMass', default=app.config['DEFAULT_MIN_MASS']))
    maxMass = int(request.form.get('maxMass', default=app.config['DEFAULT_MAX_MASS']))
    minLength = int(request.form.get('minLength', default=app.config['DEFAULT_MIN_LENGTH']))
    maxLength = int(request.form.get('maxLength', default=app.config['DEFAULT_MAX_LENGTH']))
    minFragCharge = int(request.form.get('minFragCharge', default=app.config['DEFAULT_MIN_FRAG_CHARGE']))
    maxFragCharge = int(request.form.get('maxFragCharge', default=app.config['DEFAULT_MAX_FRAG_CHARGE']))
    precursorTol = int(request.form.get('t', default=app.config['DEFAULT_PRECURSOR_TOLERANCE']))
    fragTol = int(request.form.get('f', default=app.config['DEFAULT_FRAGMENT_ION_TOLERANCE']))
    dbSearchMode = int(request.form.get('tda', default=app.config['DEFAULT_DATABASE_SEARCH_MODE']))
    actMethod = int(request.form.get('act', default=app.config['DEFAULT_ACTIVIATION_METHOD']))


    job = runFullProcess.s(tmpdir, mzmlFile, fastaFile, modsFile, minCharge, maxCharge, minMass, maxMass, minLength, maxLength,
                   minFragCharge, maxFragCharge, precursorTol, fragTol, dbSearchMode, actMethod)
    asRes = job.delay()
    app.logger.debug("result backend: %s"%str(asRes.backend))
    # asRes.save(clry.backend)
    app.logger.info("taskID = %s"%asRes.id)
    return jsonify(taskID=asRes.id)


@app.route('/MSPathFinderTForm', methods=['GET'])
def uploadForm() :
    app.logger.debug(str(request))
    return '''
     <!doctype html>
     <title>Run MSPathFinderT</title>
     <h1>Run MSPathFinderT</h1>
     <p>Caveat: a very simple UI to test the REST endpoint. Use at your own risk! A zip file of results will be 
     downloaded when processing is complete.</p>
     <form method=post enctype=multipart/form-data>
       <label for=mzmlFile>mzML File: </label><input type=file id=mzmlFile name=mzmlFile><br>
       <label for=fastaFile>fasta File: </label><input type=file id=fastaFile name=fastaFile><br>
       <label for=modsFile>Modifications File: </label><input type=file id=modsFile name=modsFile><br>
       <label for=minCharge>Min Charge: </label><input type=text id=minCharge name=minCharge value=2></br>
       <label for=maxCharge>Max Charge: </label><input type=text id=maxCharge name=maxCharge value=20></br>
       <label for=minMass>Min Mass: </label><input type=text id=minMass name=minMass value=3000></br>
       <label for=maxMass>Max Mass: </label><input type=text id=maxMass name=maxMass value=50000></br>
       <label for=minLength>Min Length: </label><input type=text id=minLength name=minLength value=21></br>
       <label for=maxLength>Max Length: </label><input type=text id=maxLength name=maxLength value=300></br>
       <label for=minFragCharge>Min Frag Charge: </label><input type=text id=minFragCharge name=minFragCharge value=1></br>
       <label for=maxFragCharge>Max Frag Charge: </label><input type=text id=maxFragCharge name=maxFragCharge value=10></br>
       <label for=t>Precursor Tolerance: </label><input type=text id=t name=t value=10></br>
       <label for=f>Fragment Ion Tolerance: </label><input type=text id=f name=f value=10></br>
       <label for=tda>Database Search Mode: </label><input type=text id=tda name=tda value=1></br>
       <label for=act>Activation Method: </label><input type=text id=act name=act value=6></br>
       <input type=submit value=Submit>
     </form>
      '''

@app.route('/status/<taskID>', methods=['GET'])
def taskStatus(taskID):
    app.logger.debug("task status (%s)"%taskID)
    try:
        res = AsyncResult(taskID, app=clry)
        app.logger.debug("AsyncResult: %s"%str(res))
        if res.state and res.state == 'SUCCESS':
            outputDirName = res.get()
            if outputDirName:
                datadir = app.config['DATA_DIRECTORY']
                if outputDirName.startswith(datadir):
                    outputDirName = outputDirName[len(datadir):]
                if outputDirName.startswith('/'):
                    outputDirName = outputDirName[1:]
                return jsonify({'state':res.state, 'files':outputDirName})
            else:
                return jsonify({'state': res.state})
        else:
            if isinstance(res.info, BaseException):
                return jsonify({'state':res.state, 'info':str(res.info)})
            else:
                return jsonify({'state':res.state, 'info':res.info})
    except Exception as err:
        app.logger.error(err)
        app.logger.error(traceback.format_exc())
        return __unexpectedErrorResponse(err)

@app.route('/results/<taskID>', methods=['GET'])
def downloadResults(taskID):
    app.logger.debug("download results for task %s"%taskID)
    try:
        # res = runFullProcess.AsyncResult(taskID)
        res = AsyncResult(taskID, app=clry)
        if res.state == SUCCESS :
            zipFileName = res.get()
            app.logger.debug('res.__class__: %s'%res.__class__)
            app.logger.debug("res.get(): %s"%zipFileName)
            app.logger.debug('res.info: %s'%res.info)
            app.logger.debug('res.result: %s'%res.result)
            if zipFileName and os.path.isfile(zipFileName):
                return send_file(zipFileName)
            else:
                app.logger.error('Cannot find output of task %s: file %s is missing'%(str(taskID), str(zipFileName)))
                return __knownErrorResponse('Error: output file not found for task %sa'%str(taskID))
        else:
            app.logger.warning('No results for task %s (status is not SUCCESS)' % str(taskID))
            return __knownErrorResponse('Task is either not complete yet or failed. Use /status/<taskID> to get task status.')
    except Exception as err:
        app.logger.error(err)
        app.logger.error(traceback.format_exc())
        return __unexpectedErrorResponse(err)


def __saveUploadedFile(requestFiles, key, dirname) :
    # get mzML input file
    if key not in request.files:
        app.logger.error('Missing %s in uploaded files'%key)
        return __knownErrorResponse("No file provided (%s)" % key)
    file = request.files[key]
    if file.filename == '':
        app.logger.error('Upload attempt with no filename (%s)'%key)
        return __knownErrorResponse("No file provided (%s)" % key)
    if file and __allowedFile(file.filename):
        filename = secure_filename(file.filename)
        app.logger.info('Uploading file %s to %s' % (filename, dirname))
        savedName = os.path.join(dirname, filename)
        file.save(savedName)
    elif file and not __allowedFile(file.filename) :
        app.logger.error('Invalid file extension for %s: valid extensions are %s' %(key,app.config['ALLOWED_UPLOAD_EXTENSIONS']))
        return __knownErrorResponse('Invalid file extension for %s: valid extensions are %s' % (key, app.config['ALLOWED_UPLOAD_EXTENSIONS']))

    return savedName

def __validateParamFileName(filename):
    if not filename or len(filename) == 0:
        return False
    datadir = app.config['DATA_DIRECTORY']
    fullname = os.path.join(datadir, filename)
    if not os.path.isfile(fullname):
        return False
    return fullname

def __allowedFile(filename):
    return '.' in filename and \
       filename.rsplit('.', 1)[1].lower() in app.config['ALLOWED_UPLOAD_EXTENSIONS']

def __invalidParameterResponse(paramName, paramValues):
    res = jsonify(message='Invalid %s value(s) (%s)' % (str(paramName), ', '.join(map(str, paramValues))), code=110)
    res.status_code = 400
    return res

def __knownErrorResponse(message) :
    res = jsonify(message=message)
    res.status_code = 400
    return res

def __unexpectedErrorResponse(err):
    traceback.print_tb(err.__traceback__)
    res = jsonify(message='\n'.join(map(str, err.args)))
    res.status_code = 500
    return res
