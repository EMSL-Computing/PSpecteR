This Docker container runs MSPathFinderT (along with PbfGen and ProMex) in wine (a windows emulator for linux). The functionality is exposed via web service endpoints described below. Currently this container is using the executables from InformedProteomics release 1.0.7017 (https://github.com/PNNL-Comp-Mass-Spec/Informed-Proteomics/releases). 

### Usage
To build the container:  
`docker build -t mspathfindert:latest .`

To run the container locally (the endpoints below will be found at http://localhost:5000/)  
`docker run -p 5000:5000 mspathfindert `  
or  
`docker run -p 5000:5000 -v [local_directory]:/data mspathfindert `


### Web services

#### /MSPathFinderT [PUT]
This endpoint is designed to work with input data files that are in a volume that is shared between the host and container. The three input files (mass spec file, database file and modifications files) should be provided via path names *relative to the shared volume*. The host volume must be mapped to the `/data` directory in the container (see second run command above).

Parameters: 
<pre>
    Parameter name          Description
    -----------------------------------------------------
    mzmlFile                relative path to mass spectrometry data file
    fastaFile               relative path to database file
    modsFile                relative path to modifications file
    minCharge               minimum precursor ion charge
    maxCharge               maximum precursor ion charge
    minMass                 minimum sequence mass in Da
    maxMass                 maximum sequence mass in Da
    minLength               minimum sequence length
    maxLength               maximum sequence length
    minFragCharge           minimum fragment ion charge
    maxFragCharge           maximum fragment ion charge
    t                       precursor tolerance
    f                       fragment ion tolerance
    tda                     database search mode
    act                     activation method
</pre>

All parameters should be passed in the URL in name=value form, for example:
http://hostname:5000/MSPathFinderT?mzmlFile=data/example.mzML&fastaFile=data/example.fasta&modsFile=data/MSPathFinder_Mods.txt&minCharge=2&maxCharge=20&minMass=3000&maxMass=50000&minLength=21&maxLength=300&minFragCharge=1&maxFragCharge=10&t=10&f=10&tda=1&act=6.

Output files will be created in the same directory as the provided mzml file.

Due to the length of time these analysis take, the endpoint will return a `taskID` and then continue to process in the background. The returned `taskID` can be used with the /status and /results endpoints below.

Example output:  
`{"taskID": "aabab14f-9b1d-4c33-abdc-23303b60dc70"}`


#### /MSPathFinderTForm [POST]
For small input files, this endpoint accepts the upload of an mzML file, a fasta file, a modifications file, and associated parameters and runs PbfGen, ProMex and MSPathFinderT. All files and parameters should be passed in the body of the request as form-data. 

The parameters are the same as those shown above, with the exception that the `mzmlFile`, `fastaFile` and `modsFile` should be files not paths.

Due to the length of time these analysis take, the endpoint will return a `taskID` and then continue to process in the background. The returned `taskID` can be used with the /status and /results endpoints below.

Example output:  
`{"taskID": "aabab14f-9b1d-4c33-abdc-23303b60dc70"}`


#### /MSPathFinderTForm [GET]
This endpoint is intended only for debugging purposes and provides a web UI for calling the MSPathFinderT POST endpoint.


#### /status/&lt;taskID&gt; [GET]
This endpoint is used to get the status of a submitted job. It will return the state (PENDING, STARTED, SUCCESS, FAILED) and if the job is in progress with will return the stage (PbfGen, ProMex or MSPathFinderT).

Example output:  
`{"info":{"stage":"MSPathFinderT"},"state":"STARTED"}`  
or  
`{"files":["example.ms1ft","example.mzid","example_IcTarget.tsv"],"state":"SUCCESS"}`


#### /results/&lt;taskID&gt; [GET]
For `MSPathFinderTForm` jobs (where the input files are uploaded), after the job is complete, this endpoint can be used to download a zip file containing the results. This includes the ms1ft, mzid and tsv files created by ProMex and MSPathFinderT. If this endpoint is called with a `taskID` that has not completed it will return an error message.
