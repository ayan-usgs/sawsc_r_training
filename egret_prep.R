library(dataRetrieval)
library(EGRET)


# R package for analysis of long term changes in 
# water quality and streamflow.

# Acquires data from various sources
# Places data in a useable fromat
# Routines to process and visualize the data

# WRTDS - Weighted Regressions on Time, Discharge and Season
# Used to describe long-term trends in both concentration and flux


# Gather discharge data:
site_id <- '01491000'
start_date <- ''  # empty string means earliest date
end_date <- '2011-08-25'
# 00631 is the parameter code for nutrients
parameter_cd <- '00631'
sample <- readNWISSample(site_id,
                         parameter_cd,
                         start_date,
                         end_date)
# convert dates to character and find the min
min_start_date <- min(as.character(sample$Date))
# get discharge data
daily <- readNWISDaily(site_id, '00060', min_start_date, end_date)

# use read NWISInfo to update names that will appear in plots
INFO <- readNWISInfo(site_id, parameter_cd, interactive=FALSE)
INFO$shortName <- "Choptank River near Greensburo, MD"

# join sample data with discharge data
eList <- mergeReport(INFO, daily, sample)

# take a quick look at sample data
boxConcMonth(eList)
boxQTwice(eList)
plotConcTime(eList)
plotConcQ(eList)
multiPlotDataOverview(eList)

# run a model estimation
# concentration as function of discharge and time
eList <- modelEstimation(eList)


# check model results
plotConcTimeDaily(eList)
plotFluxTimeDaily(eList)
plotConcPred(eList)
plotFluxPred(eList)
plotResidPred(eList)
plotResidQ(eList)
plotResidTime(eList)
boxResidMonth(eList)
boxConcThree(eList)

#Require Daily + INFO:
plotConcHist(eList)
plotFluxHist(eList)

# Multi-line plots:
date1 <- "2000-09-01"
date2 <- "2005-09-01"
date3 <- "2009-09-01"
qBottom<-100
qTop<-5000
plotConcQSmooth(eList, date1, date2, date3, qBottom, qTop, 
                concMax=2,qUnit=1)
q1 <- 10
q2 <- 25
q3 <- 75
centerDate <- "07-01"
yearEnd <- 2009
yearStart <- 2000
plotConcTimeSmooth(eList, q1, q2, q3, centerDate, yearStart, yearEnd)

# Multi-plots:
fluxBiasMulti(eList)

#Contour plots:
clevel <- seq(0,2,0.5)
maxDiff <- 0.8
yearStart <- 2000
yearEnd <- 2010

plotContours(eList, yearStart,yearEnd,qBottom,qTop, 
             contourLevels = clevel,qUnit=1)
plotDiffContours(eList, yearStart,yearEnd,
                 qBottom,qTop,maxDiff,qUnit=1)
# modify this for your own computer file structure
savePath<-"/Users/ayan/Desktop/" 
saveResults(savePath, eList)