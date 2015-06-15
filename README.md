# DatathonM2M
Code, data and help for datathon M2M

This repo contains the code, data and results for Datathon M2M
### To start.....
* All files needed for the solution are in the code subdirectory. You have to download the complete code directory and open the RStudio project file.

### Description
* Files ending in R contain R code. The name indicates the model used. 
* I have deleted previous versions of files to make easier to navigate through the code
* Files ending in Rdat contain R objects. You have to load it using load("file.Rdat") if you want to load the object in your R session.
* The ppt file contains the results presentation
* The excel file contains intermediate results obtained from the different models.
* The Rproj file and .Rdata contain the RStudio project and environment data
* The csv files contain the original data and are not included here because github doesn't allow files >100MB

### Rdat files Details
* The more important Rdat file is dat.ini.Rdat and sim.uwgh2_4_xtra.all.new.Rdat  which contain the support and input data already transformed in wide format.
* There is an Rdat file, for each model fitted, starting with "hr". These files contain the loss function(rendimiento) results per SIM when using the test sets. Besides these files, there are additional Rdat files storing intermediate R objects.

### R files Details
* loaddat.R and preparedat.R are the pipeline to create the variables sim.p and sim.u
* the variable sim.uwg which contains all the fully transformed data in wide format can be loaded using load("sim.uwgh2_4_xtra.all.new.Rdat")  or created using sim.uwg=towidef2h(valsims) which calls a function in the file: librarycode.R
* eda.R contains code for exploratory data analysis
* ranksamples.R has also code for exploratory data analysis that tried to cluster the SIMs according with the prediction results.
* playaround.R contains generic code
* mixoutputs.R have code for ensembling the results from several classifiers
* Most of R files have a name which shows the model implemented in the file. All models have several chunks of code, where each chunk is associated to the use of an specific validation method. The final chunk is always the chunk to produce the final result for submission (mainly for the ARIMAX and ARIMA models for which I have produced a final submission file)

