import logging
from logging.config import dictConfig

LOG_FORMAT = {'format': '%(asctime)s [%(levelname)s] %(message)s (%(filename)s:%(lineno)s)'}

CONSOLE_HANDLER = {
            'level':'DEBUG',
            'class':'logging.StreamHandler',
            'formatter': 'standard'
        }

FILE_HANDLER = {
            'level': 'INFO',
            'class': 'logging.FileHandler',
            'filename': 'ranking_server.log',
            'formatter': 'standard'
        }


# Logging config variables common to both console and file
COMMON_LOGGING_CONFIG  = {
    'version': 1,
    'disable_existing_loggers': False,
    'formatters': {
        'standard': LOG_FORMAT
    },
    # 'handlers': {
    #     'console': CONSO
    # },
    'root': {
        'handlers':['console'],
        'propagate': True,
        'level':'DEBUG'
    }
}

FILE_LOGGING_CONFIG  = {
    'version': 1,
    'disable_existing_loggers': False,
    'formatters': {
        'standard': LOG_FORMAT
    },
    'handlers': {
        'logfile': {
            'level': 'DEBUG',
            'class': 'logging.FileHandler',
            'filename': 'ranking_server.log'
        }
    },
    'root': {
        'propagate': True,
        'level':'DEBUG'
    }
}

LOGGER = logging.getLogger('root')


