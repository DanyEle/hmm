#Daniele Gadler, Andrea Janes, Barbara Russo
#Free University of Bolzano-Bozen 2016-2017

#pathToData is used only from the script not making use of Damevski et al's dataset
#pathToData <- "~/TeamDropBox/Dropbox/Thesis/R/Datasets/WindowsPhone_ID.csv"

#How columns are separated from one another
SEPARATOR_DATASET = ";"
#Set this to TRUE this if the dataset has no "SequenceIDs" column
MARK_SEQUENCE_IDS = FALSE

#TRUE = For Damevski's dataset or other datasets, in case we may need to carry out some pre-processing in other functions / files
LOAD_CUSTOM_SEQUENCES = TRUE

AMOUNT_WORKERS = 8



#Experimental Features
#Find all sequences containing SU [Symbols Undesirable]
FILTER_SU = FALSE
#Filter all sequences containing the top 50% most frequent symbols contained in all sequences containing SU
FILTER_MOST_FREQUENT_SU  = FALSE




main <- function()
{
  k <- 1
  
  #sink(paste("hmm_loading_dataset.txt"))
  #DANIELE LOADED BEFOREHAND
  sequences_global <<- load_custom_sequences_if_needed() 
  #sequences_global <<- sequences_marked
  #sink()
  
 
  while(k <= 5)
  {
    sink(paste("hmm_with_", k, "_interesting_sequences.txt", sep=""))
    
    
    loopOverSequenceSet(DATA_PATH, k)
    k <- k + 1
    print(paste("Now with k = ", k, sep=""))
    
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
      
      #DANIELE LOAD BEFOREHAND. Can do this once, load it in memory, then no longer need to do it. Doesn't take too long actually?
      ##sortedSequencesIDs <<- sortedSequencesIDs
      sortedSequencesIDs <- sortSequencesWithIDs(sequencesIDs)
      sortedSequences <<- sortedSequencesIDs[[1]]
      #sortedSequencesBeforeFiltering <- sortedSequences
      #EXPERIMENTAL FUNCTIONS. Not really necessary.
      #sortedSequences <<- filter_sequences_with_SU_if_needed(sortedSequences)
      #sortedSequences <<- filter_sequences_with_most_frequent_symbols(sortedSequences, sortedSequencesBeforeFiltering)
      print("Computing loglikelihood for ALL DATA state")
      #compute the loglikelihood of the model with one state
      library("hmm.discnp")
      LogLikInit = logLikHmm(sortedSequences, list(Rho=t(HMMTrained$emissionProbs), tpm = HMMTrained$transProbs, ispd = HMMTrained$startProbs ) )
      print("init log likelihood")
      print(LogLikInit)
      print(format(Sys.time(), "%a %b %d %X %Y"))
   	  print("Generating theta-frequent sequences...")
   	  
   	  #GOAL: parallelize theta frequent sequences. 
   	  
   	  #Firstly, split the sorted sequences
   	  partitions_sampleObs = find_partitions_for_sorted_sequences(sortedSequences, AMOUNT_WORKERS)
   	  #  sequences_marked_split = mcmapply(mark_debug_sessions_with_ID, sampleObs = partitions, index = indexes, mc.cores = AMOUNT_WORKERS) 

   	  parts_theta_frequent_sequences = mcmapply(getThetaFrequentSequences, sortedSequences = partitions_sampleObs, theta=theta, mc.cores = AMOUNT_WORKERS )
   	  
   	  #now combine the different parts found.
   	  thetaFrequentSequences = combine_partitions_theta_freq_sequences(parts_theta_frequent_sequences)
   	  
   	  #plain old sequential part
      #thetaFrequentSequencesSeq <-getThetaFrequentSequences(sortedSequences, theta)  
      
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
            
      #By observing each top interesting sequence per time, which is not already contained in the existing states, e
      #xtract the symbols for the next state.  Build the newState model and compare the loglikelihood with the previous model. Loop until the loglikelihood is not increasing.	
      continue=TRUE
      i<-1
      LogLikCur<<-LogLikInit
      
      while(continue)
      {
          
        print(format(Sys.time(), "%a %b %d %X %Y"))
  	  	print(paste("Generating probable sequences..."))
  
  	  	thetaProbableSequences <- getThetaProbableSequences(HMMTrained, theta)
    
      	print(format(Sys.time(), "%a %b %d %X %Y"))
      	print("Generating and sorting interesting sequences")
  
      	#it returns: [[1]]=conditionType [[2]] interesting sequences [[3]] interestingness. TODO: extract the uninteresting sequences
      	interestingSequences<-computeAllSequencesInterestingness(thetaFrequentSequences, thetaProbableSequences, HMMTrained,theta)
        	      	
      	#sort interesting sequences from which to select the symbols. It returns: [[1]]=conditionType (1 or 2) [[2]] interesting sequences [[3]] interestingness
      	sortedInterestingSequences<-sortSequencesByInterestingness(interestingSequences)     	
      	
        print(format(Sys.time(), "%a %b %d %X %Y"))
      	print("Building Model with one more state and the following symbols")
        	
      	#select the symbols in toMoveSymbols that are in ALL DATA state. This is the simplest case in which the expert decides to move symbols only from ALL DATA. 
    		allDataEP<-HMMTrained$emissionProbs[1,]    
      	#The first sequence that has non-empty intersection with ALL DATA state. It returns the a vector of symbols   
  		  intersection<-as.vector(sapply(sortedInterestingSequences[[2]],function(x)intersect(unlist(x),names(allDataEP[which(!allDataEP==0)]))))
  		  q<-unlist(lapply(intersection,`all.equal`,character(0)))		
  		  
  		  
  		  toMoveSymbols <- selectSymbolsTopKInterestingSequences(intersection, q, k)
  
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

filter_sequences_with_SU_if_needed <- function(sortedSequences)
{
  if(LOAD_CUSTOM_SEQUENCES)
  {
    if(FILTER_SU)
    {
      symbols_undesirable = c("Edit.Undo","View.NavigateBackward","Edit.SelectionCancel","Edit.Redo",
                              "View.Source Not Available",
                              "TeamFoundationContextMenus.PendingChangesPageChangestoInclude.TfsContextPendingChangesPageUndo",
                              "Debug.Restart","Build.Cancel","View.Source Not Found","File.TfsUndoCheckout","View.No Symbols Loaded",
                              "ClassViewContextMenus.ClassViewProject.TfsContextUndoCheckout",
                              "ClassViewContextMenus.ClassViewProject.SourceControl.TfsContextUndoCheckout",
                              "View.No Source Available","TestExplorer.CancelTests","TeamFoundationContextMenus.BuildExplorer.RetryBuild",
                              "TeamFoundationContextMenus.SourceControlHistoryChangesets.TfsContextHistoryRollbackChangesInThisVersion",
                              "TeamFoundationContextMenus.SourceControlExplorer.TfsContextExplorerRollback")
      
      #ONE LINER. Get the sequences containing of the undesirable symbols
      sequences_with_SU = sortedSequences[(which(sapply(sortedSequences, function(x) any(symbols_undesirable %in% x))==TRUE))]
      return(sequences_with_SU)
    }
  }
  
  return(sortedSequences)
}


#Input: sortedSequences that have already been filtered out by function filter_sequences_with_SU_if_needed
filter_sequences_with_most_frequent_symbols <- function(sortedSequences, sortedSequencesBeforeFiltering)
{
  if(LOAD_CUSTOM_SEQUENCES)
  {
    if(FILTER_MOST_FREQUENT_SU)  
    {
      #get all the unique symbols present in the sortedSequences
      unique_symbols = unique(unlist(sortedSequences))
      
      amount_symbols_in_sequences = sum(table(unlist(sortedSequences)))

      #compute frequency of unique_symbols in the sequences containing the SU
      table_frequency_symbols = table(unlist(sortedSequences))/ amount_symbols_in_sequences
      
      #the median separates top 50% symbols from lower 50% symbols
      median = median(table_frequency_symbols)
      
      #now take the top 50% most frequent symbols in the table
      top_most_frequent_symbols = which(table_frequency_symbols >= median)
      
      #And now take all the sequences that contain ALL most frequent symbols found
      sequences_with_all_frequent_symbols = sortedSequencesBeforeFiltering[(which(sapply(sortedSequencesBeforeFiltering, function(x) all(top_most_frequent_symbols %in% x))==TRUE))]
      
      return(sequences_with_all_frequent_symbols)
      
    }
  }
  
  return(sortedSequences)
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

