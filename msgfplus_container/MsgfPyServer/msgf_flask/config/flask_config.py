import os 
d = os.path.dirname(os.path.realpath(__file__))

class Config(object):
    DEBUG = False
    ALLOWED_UPLOAD_EXTENSIONS = set(['mzml', 'fasta', 'txt'])  # put extensions in lower case
    MAX_CONTENT_LENGTH = 8 * 1024 * 1024 #max file upload size is 8 MB
    UPLOAD_DIRECTORY = '/tmp'
    DATA_DIRECTORY = '/data'