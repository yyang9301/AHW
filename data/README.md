This folder contains the processed BRAN and ERA data necessary for the project. It also contains the MHW results as well as the data necessary to fit the SOM.

The folder "/BRAN" contains the processed BRAN data.

The folder "/ERA" contains the processed ERA-Interim data.

The folder "/SACTN" contains the MHW results.

The folder "/SOM" contains the data packets for each event. These are turned into single vectors and all combined before being fed to the SOM to determine the correct nodes for each event.

The file "download.BRAN.R" contains the code used to download BRAN data from their XML server.