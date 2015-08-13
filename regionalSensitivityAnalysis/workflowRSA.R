#code to analyse parameter sensitivity analysis 
#Input data are behavioural parameter sets and associated
#model signatures/or performance metric values. these input values may already be constrained, or
#result from monte carlo sampling. 

library(ggplot2)

#change to your own working directory
setwd("E:/Dropbox/code_tools/Sensitivity_Analysis/R/")

source("SAFunctions.R")

#parameters correspond to the columns in the exampleData.txt input file
nSig <- 2 #number of signatures used
nPar <- 8  #number of model parameters

#actual measured signature values 
RC <- 0.553 #runoff coefficient
VR <- 0.18  #variance ratio
sigVal <- cbind(RC,VR)

##### call in  simulation results from file
    store <- data.frame(read.table("exampleData.txt", header = TRUE))
    #input data format:each row represents a different parameter set
    #first Nsig columns are signatures
    #following nPar columns are parameters

    #retained parameter sets are considered behavioural as they fall within a pre-specified window
    #that surrounds the measured signature values - in this case RC and VR
   
    #names of parameters
    parnames <- colnames(store)[(nSig+1):(nPar+nSig)]

    #parameter priors used in calibration for the parameter sets - can be called in from file
    #these are used to set the range of the plots
    pmin <- c(1, 1, 0, -3, -3, 0, 0, 0)
    pmax <- c(1000, 7, 20, 3, 3, 1, 7, 2000)
    pRange <- cbind(pmin,pmax)

#####calculate sensitivity analysis behavioural thresholds

    #Sensitivity analysis is conducted for different behavioural thresholds.
    #these thresholds are defined based on distance from the measured signature value. 
    nThresh <- 6 #number of thresholds used
    maxbound <- 0.3 #this is the maximum bound used in sampling as a percentage of the signature value
    #maxbound should be chosen based on that used in calibration to return the behavioural parameter sets.
    #or thresholds can be chosen individually for different signatures.
    
    #by default, the workflow uses all provides signatures (performance metrics) in determining the behavioural 
    #parameter sets. in order to look at sensitivity for each signature separately, store needs to be subsetted
    #accordingly and nSig changed to 1.

  
    thresh <- vector(length = nThresh)
    for(i in 1:nThresh) thresh[i] = maxbound - (maxbound/nThresh)*(i-1)

    minWin <- matrix(ncol = nSig, nrow = nThresh)
    maxWin <- matrix(ncol = nSig, nrow = nThresh)
    
    for(j in 1:nThresh){
      #find behavioural models for the threshold across signatures used
      count <- 1
      for(k in 1:nSig){
        minWin[j, count] <- sigVal[1,k]*(1-thresh[j])
        maxWin[j, count] <- sigVal[1,k]*(1+thresh[j])
        count <- count + 1
      }
    }

##### pass input data to functions to get SA results
    
        #calculate CDFs for each parameter and threshold
        #function retuns a list of length nPar. each list element
        #is a table of parameter CDFs for different behavioural thresholds
        parThreshCDFs <- parCDFsens(store, minWin, maxWin, nSig, pRange)

        #produce CDF plots for each threshold
        #returns a list of length nPar plots
        parPlots <- plotparCDFs(parThreshCDFs)
        
            #print plots...
              parPlots[[1]]
              parPlots[[2]]
              #.....
              parPlots[[8]]
                
        #calculate distance from uniform prior distribution for each threshold
        #returns maxium difference between prior and posterior CDFs
        pardiffs <- diffparCDFs(parThreshCDFs)
        
        #plot distances from uniform prior to compare sensitivity
        #returns cdf diff plot
        cdfdiffplot <- plotparDiffs(pardiffs)
        
            #print plot
            cdfdiffplot
        
        #produce par-coord plot as function of thresholds
        
            #create matrix of parameters and a column of associated thresholds
            #temp will only contain signatures used, as with minWin and maxWin
            resStore <- matrix(ncol = (nThresh + 1), nrow = resCDF)
            Threshold <- vector(length = length(store[,1]))
            nSig <- 2
            
            for(j in 1:nThresh){
                tempThresh <- vector(length = length(store[,1]))
                for(k in 1:nSig){
                  tt <- which(store[,k] >= minWin[j,k] & store[,k] <= maxWin[j,k]) 
                  tempThresh[tt] <- tempThresh[tt] + 1
                }
                indexes <- which(tempThresh == nSig)
                Threshold[indexes] <- j
            }
            ppData <- cbind(store,Threshold)
            ppData <- ppData[,-c(1:nSig)]
            ppData <- ppData[ppData$Threshold > 0,]
               
            #produce the parallel coordinate plot for data and thresholds
            pcplot <- parCordPlot(ppData)
                   
        #parameter interactions as a function of threshold too for that window  
        parInter <- parInteractions(store, minWin, maxWin, nSig)
        
        #plot parameter interactions as a function of threshold applied
        #fraction of total interactions to retain - plotter's preference. e.g. 0.1 retains top 10% 
        # of most interacting parameters
        thresh <- 0.5 
        parInterPlot <- plotParInteraction(parInter, thresh)

            #plot parInterPlot
            parInterPlot
        
        
       
