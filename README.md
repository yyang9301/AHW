# AHW
The scripts and data used to determine common air-sea states during MHWs along the coast of South Africa.

## File description
* The raw text of the paper may be found in "LaTeX/Schlegel_et_al.tex"
* The code used to calculate the MHWs and fit the SOM may be found in "2.Model_fitting.R"
* The code used to create the figures seen in the paper may be found in "3.Figures.R"
* The code used to create the tables seen in the paper may be found in "4.Tables.R"
* The code used to generate the specific values given in the text may be found in "5.Text.R"
* The "data" folder contains the processed data used in the analyses as well as the output of "2.Model_fitting.R"
* The "func" folder contains custom made functions used in the analyses
* The "graph" folder contains the files necessary to create maps as well as the output of "3.Figures.R"
* The "setupParams" folder contains the SACTN site list and analysis period data
* The "LaTeX" folder contains the files used to compile "/LaTeX/...", the body of the text
* The "old" folder contains scripts and data from previous versions of the analysis/ methodology

## 2016/09/18
* All for loops removed from extreme event calculation work flow
* A for loop is still used to "grow" the SAWS data, allowing for it to be compared against each SACTN time series
* All possible extreme events combinations etc. have been calculated as per current agreement on methodology

## 2016/09/19
* All for loops removed from the co-occurrence calculations
* Co-occurrence calculated for all combinations of SACTN and SAWS sites including tmean, tmin and tmax

## 2016/09/20
* Corrected error in calculation of co-occurrence proportions
* Produced a medley of initial figures to visualise first round of results

## 2016/09/28
* Removed for loop used to "grow" the SAWS data
* All extreme events re-calculated with same results

## 2016/09/29
* Calculated extreme events for SAWS time series based on a static analysis period (1981 - 2010)
* Changed file naming convention for extreme event results to differentiate the results calculated with the static period
* Created function that calculates distance AND bearing between any two sites
* Calculated distance and bearing between all possible sites within and among datasets

## 2016/09/30
* Improved co-occurrence workflow to allow for calculation of extreme events between AND within datasets
* Expanded results of co-occurrence analysis to also include the statistics of the co-occurring events
* No longer constraining co-occurrence rates by set periods of days
* Now the difference in days between the latest occurring events is recorded

## 2016/10/03
* Calculated latest occurring events between AND within all sites AND datasets
* Added distance, bearing and coastal section metrics for compared sites
* Created a .Rmd file to explain the co-occurrence step of the work-flow

## 2016/10/18
* Created figures for the largest (int_cum) heat wave and cold spell for each time series
* Explored relationships between SAWS and SACTN heat waves occurring within 7 and 2 days of one another

## 2016/10/20
* A small bug was detected in the original SACTN data set
* Improved screening methods were implemented at the source to smooth this issue
* These improved methods were applied to the load step in this project as well
* All scripts were re-run with the newly screened data and all calculations/ figures compiled anew
* The final co-occurrence rates are ever so slightly higher than previously

## 2016/10/22
* Fix issues in which missing SACTN values were saved as NaN instead of NA
* This had no effect on the calculations

## 2016/10/31
* Wind data for Port Nolloth added to project
* Function created that can calculate mean daily wind vectors
* First draft of a figure that shows wind vectors with temperatures

## 2016/11/01
* Worked out a couple of preliminary techniques for how best to visualise air, sea and wind data together during co-occurrence

## 2016/11/02
* The preliminary round of air, sea, wind figures

## 2016/11/29
* Updated exploratory analyses
* More work done with wind data
* Two new reports written up and knitted

## 2017/02/07
* Script created to download BRAN data
* These data are saved in an external folder as they are to multitudinous for GitHub
* First steps taken for the creation of the LaTeX version of the manuscript

## 2017/02/08
* Function created to load BRAN data into R in long format
* Additional text added to manuscript

## 2017/02/13
* Function created to load ERA Interim data into R in long format
* First proof of concept figure for synoptic air-sea view during events created

## 2017/02/14
* Reanalysis data processing pipeline improved
* Wind vectors visualised

## 2017/02/17
* Summary data boxes added to synoptic air-sea figure
* Rough draft of introduction complete

## 2017/02/20
* Outline of synoptic air-sea figure complete

## 2017/02/23
* Calculation of daily climatologies for air-sea state
* The data frames are not stored here as they are too large

## 2017/02/23
* Temperature anomalies added to synoptic figure

## 2017/02/25
* Current and wind anomalies added

## 2017/02/28
* Daily climatologies for girded reanalysis temperatures have now all been smoothed
* This allows for smoother temperature anomaly values to be calculated

## 2017/03/01
* Cosmetic adjustments to synoptic figure

## 2017/03/01
* Synoptic figure output now also saves the temp and uv values in a separate list for SOMs

## 2017/03/02
* All synoptic figures created

## 2017/03/08
* Proof of concept for SOMs

## 2017/03/09
* SOM code automated
* Initial results visualised

## 2017/03/10
* Seasonality added to output
* Individual SOMs run for each coastal section

## 2017/03/13
* All functions created for SOM work flow moved to a separate script
* Alternative rescaling function for SOM results added

## 2017/03/16
* Ran hierarchical cluster analysis for air-sea states during MHWs
* Began investigation into clustering of air-sea states for daily climatologies

## 2017/03/16
* Hierarchical cluster analysis for air-sea state during daily climatologies

## 2017/03/30
* First step towards running the three agreed upon cluster analyses on all data

## 2017/04/03
* More work towards the cluster analyses

## 2017/04/08
* More work towards the cluster analyses

## 2017/04/10
* All variables that could affect the results of a clustering technique have been analysed

## 2017/04/11
* Updates to cluster results code

## 2017/05/11
* Now using principal component initialisation (PCI) rather than random initialisation (RI) for SOMs
* All SOM modelling in "LaTeX/cluster_results.Rmd" updated accordingly

## 2017/05/12
* "LaTeX/cluster_results.Rmd" completed

## 2017/05/15
* Additions to methods section

## 2017/05/16
* Edits to intro and methods sections
* Additions to methods and results sections
* Began cleaning workflow for publication

## 2017/05/17
* "1.Data_assembly.R" complete
* Removed data from GitHub not cleared for public access
* Began moving unused scripts and data to the "old" folder

## 2017/05/18
* Began "2.Model_assembly.R"
* More files moved to "old" folder

## 2017/05/19
* Re-created data packets for use with SOMs and synoptic figures

## 2017/05/22
* Smoothing out Git push issues
* Complete documentation provided for all files

## 2017/05/23
* More modeling, figures, and one table
* Additional intro, methods, and results edits

## 2017/05/24
* References for clustering added to methods section
* More results written up

## 2017/06/05
* Writing for results, discussion and conclusion added

## 2017/06/08
* Table 2 modified
* Two new figures included
* Additional writing in manuscript

## 2017/06/09
* Minor edits
* First draft complete

## 2017/06/13
* First round of co-author edits

## 2017/06/15
* More literature added in support of the introduction

## 2017/07/11
* Additional co-author comments incorporated into text

## 2017/07/18
* Code for Figure 1 added

## 2017/07/19
* All comments for the Introduction section incorporated

## 2017/07/20
* Progress made on methods section

## 2017/07/21
* Comments for methods section wrapped up

## 2017/07/26
* Phasing out BRAN in favour of AVISO and OISST
* Recalculated ERA daily clims based on new longer period
* Calculated OISST daily clims

## 2017/07/27
* More phasing out of BRAN
* Calculated AVISO daily clims

## 2017/07/28
* Analysis with new OISST+AVISO data complete
* New data packets created
* New synoptic atlas figures created
* New SOM figures created