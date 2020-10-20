import os
import shutil
import sys
import re
import tempfile
import time
from flask import Flask, request, Response, send_file
from flask.json import jsonify, loads
from flask.logging import default_handler
import logging
import traceback
from werkzeug.utils import secure_filename
import subprocess
import zipfile

from mspf_flask.config.log_config import *

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

@app.route('/MSPathFinderT', methods=['POST'])
def runMSPathFinderT() :
    app.logger.debug(str(request))
    app.logger.debug('request.form: %s'+str(request.form))
    app.logger.debug('request.files keys: %s' % str([x for x in request.files.keys()]))

    tmpdir = tempfile.mkdtemp(dir=app.config['UPLOAD_DIRECTORY'])

    mzmlFile = __saveUploadedFile(request.files, 'mzmlFile', tmpdir)
    fastaFile = __saveUploadedFile(request.files, 'fastaFile', tmpdir)
    modsFile = __saveUploadedFile(request.files, 'modsFile', tmpdir)

    # get parameters from form fields
    minCharge = int(request.form.get('minCharge', default=app.config['DEFAULT_MIN_CHARGE']))
    maxCharge = int(request.form.get('maxCharge', default=app.config['DEFAULT_MAX_CHARGE']))
    minMass = int(request.form.get('minMass', default=app.config['DEFAULT_MIN_MASS']))
    maxMass = int(request.form.get('maxMass', default=app.config['DEFAULT_MAX_MASS']))
    minLength = int(request.form.get('minLength', default=app.config['DEFAULT_MIN_LENGTH']))
    maxLength = int(request.form.get('maxLength', default=app.config['DEFAULT_MAX_LENGTH']))
    minFragCharge = int(request.form.get('minFragCharge', default=app.config['DEFAULT_MIN_FRAG_CHARGE']))
    maxFragCharge = int(request.form.get('maxFragCharge', default=app.config['DEFAULT_MAX_FRAG_CHARGE']))

    try:
        pbfFile = __runPbfGen(tmpdir, mzmlFile)
        ms1ftFile = __runProMex(tmpdir, os.path.basename(pbfFile), minCharge=int(minCharge), maxCharge=int(maxCharge),
                                minMass=int(minMass), maxMass=int(maxMass))
        mzidOutputFile, tsvOutputFile = __runMSPathFinderT(tmpdir, os.path.basename(pbfFile),
                                                           os.path.basename(ms1ftFile), os.path.basename(fastaFile),
                                                           os.path.basename(modsFile),
                                                           minLength=minLength, maxLength=maxLength, minCharge=minCharge,
                                                           maxCharge=maxCharge, minMass=minMass,maxMass=maxMass,
                                                           minFragCharge=minFragCharge, maxFragCharge=maxFragCharge)

        #return pbfFile+'\n'+ms1ftFile

        # zip results together and return
        zipFileName = os.path.join(tmpdir, 'MSPathFinderT_results.zip')
        zipObj = zipfile.ZipFile(zipFileName, 'w')
        zipObj.write(pbfFile, os.path.basename(pbfFile))
        zipObj.write(ms1ftFile, os.path.basename(ms1ftFile))
        zipObj.write(mzidOutputFile, os.path.basename(mzidOutputFile))
        zipObj.write(tsvOutputFile, os.path.basename(tsvOutputFile))
        zipObj.close()
        app.logger.debug("Finished")
        return send_file(zipFileName)

    except Exception as err:
        print(str(err)) #prints to console / apache error log
        app.logger.error(str(err))
        app.logger.error(traceback.format_stack())
        return __unexpectedErrorResponse(err)

    # finally :
        ## TODO: delete tmpdir and all files in it
        # shutil.rmtree(tmpdir)


@app.route('/MSPathFinderT', methods=['GET'])
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
       <label for=maxCharge>Max Charge: </label><input type=text id=maxCharge name=maxCharge value=60></br>
       <label for=minMass>Min Mass: </label><input type=text id=minMass name=minMass value=3000></br>
       <label for=maxMass>Max Mass: </label><input type=text id=maxMass name=maxMass value=50000></br>
       <label for=minLength>Min Length: </label><input type=text id=minLength name=minLength value=21></br>
       <label for=maxLength>Max Length: </label><input type=text id=maxLength name=maxLength value=300></br>
       <label for=minFragCharge>Min Frag Charge: </label><input type=text id=minFragCharge name=minFragCharge value=1></br>
       <label for=maxFragCharge>Max Frag Charge: </label><input type=text id=maxFragCharge name=maxFragCharge value=10></br>
       <input type=submit value=Submit>
     </form>
      '''


def __runPbfGen(dirname, mzmlFileName):
    app.logger.info("Running PbfGen")
    bname = os.path.splitext(os.path.basename(mzmlFileName))[0]
    cmd = ['wine', os.path.join(app.config['EXE_DIR'], 'PbfGen.exe'), '-s %s'%mzmlFileName]
    app.logger.debug("PbfGen command: "+str(cmd))
    # https://janakiev.com/blog/python-shell-commands/

    # need to execute this from the directory where the mzml file exists
    process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                               universal_newlines=True, cwd=dirname)
    stdout, stderr = process.communicate() #waits for process to complete
    if len(stderr) > 0 :
        app.logger.error('PbfGen error: '+stderr)
        raise Exception('PbfGen error: '+stderr)

    # get output filename
    # outputFile = os.path.join(dirname, re.sub('mzML$', 'pbf', mzmlFileName))
    # if not os.path.isfile(outputFile):
    #     files = [x for x in os.listdir(dirname) if re.match('.*pbf$', x)]
    #     if len(files) == 0:
    #         app.logger.error('PbfGen error: no output file found')
    #         raise Exception('PbfGen error: no output file found')
    #     if len(files) == 1:
    #         outputFile = os.path.join(dirname, files[0])
    #     if len(files) > 1:
    #         app.logger.error('PbfGen error: multiple output files found')
    #         raise Exception('PbfGen error: multiple output files found')
    try:
        outputFile = __getOutputFileName(dirname, bname, 'pbf', 'PbfGen')
    except Exception as err:
        app.logger.error('PbfGen stdout: \n'+stdout)
        app.logger.error('PbfGen stderr: \n' + stderr)
        raise err

    app.logger.debug("PbfGen output: %s"%outputFile)
    return outputFile


def __runProMex(dirname, pbfFileName, minCharge, maxCharge, minMass, maxMass):
    app.logger.info("Running ProMex")
    bname = os.path.splitext(os.path.basename(pbfFileName))[0]
    cmd = ['wine', os.path.join(app.config['EXE_DIR'], 'ProMex.exe'), '-i %s'%pbfFileName,
           '-minCharge %d'%minCharge, '-maxCharge %d'%maxCharge, '-minMass %d'%minMass,
           '-maxMass %d'%maxMass, '-score n', '-csv n', '-featureMap n']
    app.logger.debug("ProMex command: "+str(cmd))
    # https://janakiev.com/blog/python-shell-commands/

    # need to execute this from the directory where the mzml file exists
    process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                               universal_newlines=True, cwd=dirname)
    stdout, stderr = process.communicate() #waits for process to complete

    # ProMex always throws this error which can be ignored:
    # Failed to find library for L"Windows.Foundation.Diagnostics.AsyncCausalityTracer"
    if len(stderr) > 0 and not re.match('.*AsyncCausalityTracer.*', stderr):
        app.logger.error('ProMex error: '+stderr)
        raise Exception('ProMex error: '+stderr)

    # get output filename
    # outputFile = os.path.join(dirname, re.sub('pbf$', 'ms1ft', pbfFileName))
    # if not os.path.isfile(outputFile):
    #     files = [x for x in os.listdir(dirname) if re.match('.*ms1ft$', x)]
    #     if len(files) == 0:
    #         app.logger.error('ProMex error: no output file found')
    #         app.logger.error('ProMex stdout:\n'+stdout)
    #         app.logger.error('ProMex stderr:\n' + stderr)
    #         raise Exception('ProMex error: no output file found')
    #     if len(files) == 1:
    #         outputFile = os.path.join(dirname, files[0])
    #     if len(files) > 1:
    #         app.logger.error('ProMex error: multiple output files found')
    #         raise Exception('ProMex error: multiple output files found')
    try:
        outputFile = __getOutputFileName(dirname, bname, 'ms1ft', 'ProMex')
    except Exception as err:
        app.logger.error('ProMex stdout: \n'+stdout)
        app.logger.error('ProMex stderr: \n' + stderr)
        raise err

    app.logger.debug("ProMex output: %s"%outputFile)
    return outputFile

def __runMSPathFinderT(dirname, pbfFileName, ms1ftFileName, fastaFileName, modsFile, minLength, maxLength,
                       minCharge, maxCharge, minMass, maxMass, minFragCharge, maxFragCharge):
    app.logger.info("Running MSPathFinderT")
    bname = os.path.splitext(os.path.basename(pbfFileName))[0]
    cmd = ['wine', os.path.join(app.config['EXE_DIR'], 'MSPathFinderT.exe'), '-s %s'%pbfFileName,
           '-feature %s'%ms1ftFileName, '-d %s'%fastaFileName, '-o .', '-t 10', '-f 10', '-m 1', '-tda 0',
           '-minLength %d'%minLength, '-maxLength %d'%maxLength,
           '-minCharge %d'%minCharge, '-maxCharge %d'%maxCharge,
           '-minMass %d'%minMass, '-maxMass %d'%maxMass,
           '-minFragCharge %d'%minFragCharge, '-maxFragCharge %d'%maxFragCharge,
           '-mod %s'%modsFile]

    #-d Amanda.fasta -o .\ -t 10 -f 10 -m 1 -tda 0 -minLength 21 -maxLength 300 -minCharge 2 -maxCharge 20 -minFragCharge 1 -maxFragCharge 10 -minMass 3000 -maxMass 5000 -mod MSPathFinder_Mods.txt
    app.logger.debug("MSPathFinderT command: "+str(cmd))

    # need to execute this from the directory where the mzml file exists
    process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                               universal_newlines=True, cwd=dirname)
    stdout, stderr = process.communicate() #waits for process to complete

    # MSPathFinderT always throws this error which can be ignored:
    # Failed to find library for L"Windows.Foundation.Diagnostics.AsyncCausalityTracer"
    if len(stderr) > 0 and not re.match('.*AsyncCausalityTracer.*', stderr):
        app.logger.error('MSPathFinderT error: '+stderr)
        raise Exception('MSPathFinderT error: '+stderr)

    # get output filenames
    try :
        mzidOutputFile = __getOutputFileName(dirname, bname, 'mzid', 'MSPathFinderT')
    except Exception as err:
        app.logger.error('MSPathFinderT stdout: \n'+stdout)
        app.logger.error('MSPathFinderT stderr: \n' + stderr)
        raise err

    app.logger.debug("MSPathFinderT mzid output: %s"%mzidOutputFile)

    tsvOutputFile = __getOutputFileName(dirname, bname, 'tsv', 'MSPathFinderT')
    app.logger.debug("MSPathFinderT tsv output: %s"%tsvOutputFile)

    return mzidOutputFile, tsvOutputFile

def __saveUploadedFile(requestFiles, key, dirname) :
    # get mzML input file
    if key not in request.files:
        app.logger.error('Missing %s in uploaded files'%key)
        return __illegalArgumentResponse("No file provided (%s)"%key)
    file = request.files[key]
    if file.filename == '':
        app.logger.error('Upload attempt with no filename (%s)'%key)
        return __illegalArgumentResponse("No file provided (%s)"%key)
    if file and __allowedFile(file.filename):
        filename = secure_filename(file.filename)
        app.logger.info('Uploading file %s to %s' % (filename, dirname))
        savedName = os.path.join(dirname, filename)
        file.save(savedName)
    elif file and not __allowedFile(file.filename) :
        app.logger.error('Invalid file extension for %s: valid extensions are %s' %(key,app.config['ALLOWED_UPLOAD_EXTENSIONS']))
        return __illegalArgumentResponse('Invalid file extension for %s: valid extensions are %s' % (key, app.config['ALLOWED_UPLOAD_EXTENSIONS']))

    return savedName

def __getOutputFileName(dirname, basename, extension, module):
    """
    Extension should not have '.'
    """
    outputFile = os.path.join(dirname, basename+"."+extension)

    if not os.path.isfile(outputFile):
        files = [x for x in os.listdir(dirname) if re.match('.*'+extension+'$', x)]
        if len(files) == 0:
            app.logger.error('%s error: no output %s file found'%(module, extension))
            raise Exception('%s error: no %s output file found'%(module, extension))
        if len(files) == 1:
            outputFile = os.path.join(dirname, files[0])
        if len(files) > 1:
            app.logger.error('%s error: multiple %s output files found'%(module, extension))
            raise Exception('%s error: multiple %s output files found'%(module, extension))
    return outputFile

def __allowedFile(filename):
    return '.' in filename and \
       filename.rsplit('.', 1)[1].lower() in app.config['ALLOWED_UPLOAD_EXTENSIONS']

def __invalidParameterResponse(paramName, paramValues):
    res = jsonify(message='Invalid %s value(s) (%s)' % (str(paramName), ', '.join(map(str, paramValues))), code=110)
    res.status_code = 400
    return res

def __illegalArgumentResponse(message) :
    res = jsonify(message=message)
    res.status_code = 400
    return res

def __unexpectedErrorResponse(err):
    traceback.print_tb(err.__traceback__)
    res = jsonify(message='\n'.join(map(str, err.args)))
    res.status_code = 500
    return res
