This folder contains the processed BRAN and ERA data necessary for the project. It also contains the MHW results as well as the data necessary to fit the SOM.

The folder "/BRAN" contains the processed BRAN data.

The folder "/ERA" contains the processed ERA-Interim data.

The folder "/SACTN" contains the MHW results.

The folder "/SOM" contains the data packets for each event. These are turned into single vectors and all combined before being fed to the SOM to determine the correct nodes for each event.

The file "download.BRAN.R" contains the code used to download BRAN data from their XML server.

The files "BRAN_anom.Rdata" and "ERA_anom.Rdata" contain the synoptic air (ERA) or sea (BRAN) state data during each event in a large wide data frame.

The file "som_model_pci.Rdata" contains the results of the SOM run on the processed data.

The file "node_all_anom.Rdata" is an index containing the node to which each event has been assigned.

The file "node_means.Rdata" is a long data frame that contains the synoptic data necessary to make figures visualising the nodes.