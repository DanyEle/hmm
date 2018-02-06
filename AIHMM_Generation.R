#Daniele Gadler, Andrea Janes, Barbara Russo
#Free University of Bolzano-Bozen 2016-2017

#Barbara 
#pathToData <- "~/TeamDropBox/Dropbox/Thesis/R/Datasets/WindowsPhone_ID.csv"
#Daniele

#What the dataset path is
#pathToData <- "C:/Users/Daniele/Documents/Workplace_Damevski/Datasets_Daniele/WindowsPhone_WITH_ID.csv" #
#How columns are separated from one another
SEPARATOR_DATASET = ";"
#Set this to TRUE this if the dataset has no "SequenceIDs" column
MARK_SEQUENCE_IDS = FALSE

#TRUE = For Damevski's dataset or other datasets, in case we may need to carry out some pre-processing in other functions / files
LOAD_CUSTOM_SEQUENCES = TRUE


main <- function()
{
  k <- 1
  
  sequences_global <<- load_custom_sequences_if_needed() 
 
  while(k <= 5)
  {
    #sink, not paste
    #sink(paste("hmm_with_", k, "_interesting_sequences.txt", sep=""))
    loopOverSequenceSet(pathToData, k)
    k <- k + 1
    print(paste("Done with k = ", k, sep=""))
    
  }
  
}




loopOverSequenceSet <- function(pathToData, k){
      print(format(Sys.time(), "%a %b %d %X %Y"))
	    print(paste("Initializing the process..."))
      initialisedProcess<-initializeHMM(pathToData)
      sequencesIDs<<-initialisedProcess[[1]]
      symbols<-initialisedProcess[[2]]
      #Do not compute theta if you want to freely pass it
      theta<<-initialisedProcess[[3]]
      HMMTrained<<-initialisedProcess[[4]]      
      #create two lists: a list of sequences [[1]] and the corresponding list of IDs [[2]]. The list of seqeunces is also sorted (and accordingly the list of IDs)
      sortedSequencesIDs <- sortSequencesWithIDs(sequencesIDs)
      sortedSequences <<- sortedSequencesIDs[[1]]
      print("Computing loglikelihood for ALL DATA state")
      #compute the loglikelihood of the model with one state
      library("hmm.discnp")
      LogLikInit = logLikHmm(sortedSequences, list(Rho=t(HMMTrained$emissionProbs), tpm = HMMTrained$transProbs, ispd = HMMTrained$startProbs ) )
      print("init log likelihood")
      print(LogLikInit)
      print(format(Sys.time(), "%a %b %d %X %Y"))
   	  print("Generating theta-frequent sequences...")
       	  	
      thetaFrequentSequences<-getThetaFrequentSequences(sortedSequences, theta)  
      
      #build the unconstrained model from scratch . The loglikelihood is always the same.  
      require(HMM)  
  	  print("Building an unconstrained model from scratch") 
  	  HMMInit = initHMM(States=c("state 1", "state 2"), symbols)
  	  unconstrainedHMM <- trainBaumWelch(HMMInit, as.vector(sequencesIDs[[1]]))
      #data of unconstrained model
      EmissMatrixUnconst = unconstrainedHMM$emissionProbs
      TransMatrixUnconst = unconstrainedHMM$transProbs
      StartProbsUnconst = unconstrainedHMM$startProbs 
      LogLikUnconst = logLikHmm(sortedSequences, list(Rho=t(EmissMatrixUnconst), tpm =  TransMatrixUnconst, ispd = StartProbsUnconst ) )
      print(paste("Loglik of unconstrained model with same nr of states: ", LogLikUnconst))
            
      #By observing each top interesting sequence per time, which is not already contained in the existing states, extract the symbols for the next state.  Build the newState model and compare the loglikelihood with the previous model. Loop until the loglikelihood is not increasing.	
      continue=TRUE
      i<-1
      LogLikCur<<-LogLikInit
      
      while(continue){
        
      print(format(Sys.time(), "%a %b %d %X %Y"))
	  	print(paste("Generating probable sequences..."))

	  	thetaProbableSequences <- getThetaProbableSequences(HMMTrained, theta)
  
    	print(format(Sys.time(), "%a %b %d %X %Y"))
    	print("Generating and sorting interesting sequences")

    	#it returns: [[1]]=conditionType [[2]] interesting sequences [[3]] interestingness. TODO: extract the uninteresting sequences
    	interestingSequences<-computeAllSequencesInterestingness(thetaFrequentSequences, thetaProbableSequences, HMMTrained,theta)
      	      	
    	#sort interesting sequences from which to select the symbols. It returns: [[1]]=conditionType (1 or 2) [[2]] interesting sequences [[3]] interestingness
    	sortedInterestingSequences<-sortSequencesByInterstingness(interestingSequences)     	
      print(format(Sys.time(), "%a %b %d %X %Y"))
    	print("Building Model with one more state and the following symbols")
      	
    	#select the symbols in toMoveSymbols that are in ALL DATA state. This is the simplest case in which the expert decides to move symbols only from ALL DATA. 
  		allDataEP<-HMMTrained$emissionProbs[1,]    
    	#The first sequence that has non-empty intersection with ALL DATA state. It returns the a vector of symbols   
		  intersection<-as.vector(sapply(sortedInterestingSequences[[2]],function(x)intersect(unlist(x),names(allDataEP[which(!allDataEP==0)]))))
		  q<-unlist(lapply(intersection,`all.equal`,character(0)))		
		  
		  
		  #toMoveSymbols_1 <-unlist(intersection[which(!q==TRUE)[[1]]])
		  #toMoveSymbols_2 <-unlist(intersection[which(!q==TRUE)[[2]]]) 
		  #toMoveSymbols<-union(toMoveSymbols_1,toMoveSymbols_2) 	
		  #we can do it for any number. take the xth top interesting sequence.
	    #toMoveSymbols_x <-unlist(intersection[which(!q==TRUE)[[]]]) 
		  
		  toMoveSymbols <- selectSymbolsTopKInterestingSequences(intersection, q, k)

		  
    	#toMoveSymbols <- toMoveSymbols_1
    	
    	print(toMoveSymbols)   
    	
    	newState<-paste("State_",i)     	
    	#Build the HMM with the new states and the symols to move in. It returns: [[1]] the HMM with new state, [[2]] the symbols to move (toMoveSymbols), [[3]] the nr of states, and [[4]]the Model Loglikelihood 
    	sequences<-sequencesIDs[[1]]
    	newStateHMMConstrained <-newStateHMMTrainingConstrained(newState, toMoveSymbols, HMMTrained, sortedSequences, sequences, LogLikUnconst)  
    	
    	LogLikConstrained<-newStateHMMConstrained[[4]]

    	#compare the current loglikelihood with previsous model's one. The loglikelihood should increase with iterations otherwise the process stops. 
    	# It returns: continue = FALSE or TRUE to be used for looping
    	#continue = FALSE --> Do not continue! Model quality has degraded or no interesting sequences have been found
    	#continue = TRUE --> Do continue! Model quality has increased and interesting sequences have been found
    	
    	#Check if a continue was already present, resulting from the log-likelihood of the newly constructed and constrained model being lower than
    	#the log-likelihood of an unconstrained model
    	continue <- newStateHMMConstrained[[5]]
    	if(continue == TRUE)
    	{
    	  #Compare current log-likelihood with the one of the previous iteration
    	  continue <- compareModelLogLikelihoodAtIteration(LogLikCur, LogLikConstrained)
    	  
    	}
    	
    	
    	print(format(Sys.time(), "%a %b %d %X %Y"))
    	print(continue)
    	
      	if(continue == TRUE)
      	{
      	  HMMTrained<<-newStateHMMConstrained[[1]]
      	  movedSymbols<<-newStateHMMConstrained[[2]]
      	  print(movedSymbols)
      	  nrStates<<-newStateHMMConstrained[[3]]
      	  LogLikCur <<-newStateHMMConstrained[[4]]
      	  print(LogLikCur)
      	  i<-i+1
      	  print(i)	
      	}
      }  
      #print(movedSymbols) 
      #print(HMMTrained) 
      print(paste("Final Log Likelihood = ", LogLikCur))
      print(paste("Log Likelihood unconstrained = ", LogLikUnconst))
      print(paste("Final Amount of states = " , nrStates))
      displaySymbolsPerState(HMMTrained)
      
      print(paste("DONE in function for k=", k))
}


compareModelLogLikelihoodConstrained<-function(LogLikConstrained, LogLikUnconstrained){
  continue<-FALSE
  # the next model must have loglikelihood > than the one of the current model 
  if(LogLikConstrained > LogLikUnconstrained)
  {
    continue=TRUE
    print("log likelihood constrained HMM, same amount of states. ")
    print(nextLogLikelihood)
    print("log likelihood unconstrained HMM")
    print(curLogLikelihood)
  }
  return(continue)
}

