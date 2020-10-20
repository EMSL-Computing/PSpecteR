class Config:
    broker_url='redis://localhost:6379/0'
    result_backend='redis://localhost:6379/0'
    EXE_DIR='/wineprefix64/drive_c/InformedProteomics_1.0.7017'
    result_serializer = 'json'
    task_ignore_result = False
