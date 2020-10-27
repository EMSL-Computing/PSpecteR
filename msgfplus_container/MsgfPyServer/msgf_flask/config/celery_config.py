import os 
class Config:
    redis_port = os.getenv('REDIS_PORT', '6379')
    broker_url='redis://localhost:%s/0'%redis_port
    result_backend='redis://localhost:%s/0'%redis_port
    result_serializer = 'json'
    task_ignore_result = False