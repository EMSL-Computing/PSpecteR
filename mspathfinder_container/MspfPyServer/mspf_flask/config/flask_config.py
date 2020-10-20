import os
d = os.path.dirname(os.path.realpath(__file__))

class Config(object):
    DEBUG = False
    ALLOWED_UPLOAD_EXTENSIONS = set(['mzml', 'fasta', 'txt'])  # put extensions in lower case
    MAX_CONTENT_LENGTH = 8 * 1024 * 1024 #max file upload size is 8 MB
    UPLOAD_DIRECTORY = '/tmp'
    DATA_DIRECTORY = '/data'
    EXE_DIR = '/wineprefix64/drive_c/InformedProteomics_1.0.7017'
    MODS_FILE = '/flask/MSPathFinder_Mods.txt'

    DEFAULT_MIN_CHARGE = 2
    DEFAULT_MAX_CHARGE = 20
    DEFAULT_MIN_MASS = 2000
    DEFAULT_MAX_MASS = 50000
    DEFAULT_MIN_LENGTH = 21
    DEFAULT_MAX_LENGTH = 300
    DEFAULT_MIN_FRAG_CHARGE = 1
    DEFAULT_MAX_FRAG_CHARGE = 10
    DEFAULT_PRECURSOR_TOLERANCE = 10
    DEFAULT_FRAGMENT_ION_TOLERANCE = 10
    DEFAULT_DATABASE_SEARCH_MODE = 1 # shuffled decoy database
    DEFAULT_ACTIVIATION_METHOD = 6  # unknown

    # find out about other params: score, maxThreads, t, f, m, tda, mod
