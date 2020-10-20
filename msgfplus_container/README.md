This Docker container runs MS-GF+ (simplified to MSGF) through a python flask app with redis and celery. The functionality is exposed via web service endpoints described below. Currently this container is using the jar file from the MS-GF+ Release v2019.07.03 (https://github.com/MSGFPlus/msgfplus/releases/tag/v2019.07.03). 

### Usage
To build the container:  
`docker build -t msgf:latest .`

To run the container locally (the endpoints below will be found at http://localhost:6800/)  
`docker run -p 6800:5000 msgf`  
or  
`docker run -p 6800:5000 -v [local_directory]:/data msgf`


### Web services

#### /MSGF [PUT]
This endpoint is designed to work with input data files that are in a volume that is shared between the host and container. The three input files (mass spec file, database file, and parameters file) should be provided via path names *relative to the shared volume*. The host volume must be mapped to the `/data` directory in the container (see second run command above).

Parameters: 
<pre>
    Parameter name          Description
    -----------------------------------------------------
    mzmlFile                relative path to mass spectrometry data file
    fastaFile               relative path to database file
    paramsFile              relative path to parameters file
</pre>

All parameters should be passed in the URL in name=value form, for example:
http://hostname:6800/MSGF?mzmlFile=data/example.mzML&fastaFile=data/example.fasta&paramsFile=data/example.txt
Output files will be created in the same directory as the provided mzml file.

Due to the length of time these analysis take, the endpoint will return a `taskID` and then continue to process in the background. The returned `taskID` can be used with the /status and /results endpoints below.

Example output:  
`{"taskID": "aabab14f-9b1d-4c33-abdc-23303b60dc70"}`


#### /MSGFForm [POST]
For small input files, this endpoint accepts the upload of an mzML file, a fasta file, and a parameters file. All files should be passed in the body of the request as form-data. 

Due to the length of time these analysis take, the endpoint will return a `taskID` and then continue to process in the background. The returned `taskID` can be used with the /status and /results endpoints below.

Example output:  
`{"taskID": "aabab14f-9b1d-4c33-abdc-23303b60dc70"}`


#### /MSGFForm [GET]
This endpoint is intended only for debugging purposes and provides a web UI for calling the MSGF POST endpoint.


#### /status/&lt;taskID&gt; [GET]
This endpoint is used to get the status of a submitted job. It will return the state (PENDING, STARTED, SUCCESS, FAILED).

Example output:  
`{"info":{"stage":"MSGF"},"state":"STARTED"}`  
or  
`{"files":["example.mzid"],"state":"SUCCESS"}`


#### /results/&lt;taskID&gt; [GET]
For `MSGFForm` jobs (where the input files are uploaded), after the job is complete, this endpoint can be used to download the mzid file. If this endpoint is called with a `taskID` that has not completed it will return an error message.
