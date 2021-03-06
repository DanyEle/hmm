#Daniele Gadler, Andrea Janes, Barbara Russo
#Free University of Bolzano-Bozen 2016-2018

#Find all sequences containing SU [Symbols Undesirable]
#FILTER_SU = FALSE
#Filter all sequences containing the top 50% most frequent symbols contained in all sequences containing SU
#FILTER_MOST_FREQUENT_SU  = FALSE





#amount_workers = INT. parallelism degree (i.e: amount of cores to be used)
#output_to_file = BOOLEAN. TRUE - Output to a file
#                          FALSE - Output to the console
#dataset_index = INT. 1 = Michael Mairegger's dataset of mobile interactions
#                     2 = Damevski et al.'s dataset for visual studio IDE interactions
#                     https://github.com/abb-iss/DeveloperInteractionLogs/tree/master
#                     3 = ALMA dataset for radio-telescope Java classes' interactions
#input_dataset = LIST. Contains the dataset (s) to use in the analysis, or the folder where datasets are contained, as in the case of Damevski et al'.
#Example invocation for (2):
#generate_hmm_workers(8, FALSE, 2, "Datasets_Damevski")
#Example invocation for (3):
#generate_hmm_workers(1, FALSE, 3, c("LogSourceInformation.txt", "LogEventSet.txt", "LogRuntimeInformation.txt"))

#Used to run the initialization phase, followed by the iterative phase with the amount of workers needed
generate_hmm_workers <- function(amount_workers=1, output_to_file=NULL, dataset_index=NULL, input_dataset=NULL)
{
    source("Common_Functions.R")
    success = validate_input_arguments(amount_workers, output_to_file, dataset_index, input_dataset)  
    if(success == TRUE)
    {
      print("Valid input arguments passed")
    }
    else if(success == FALSE)
    {
      print("Proper usage: generate_hmm_workers(N, BOOLEAN, INT[1 MM | 2 = Damevsi | 3 ALMA], String/List)")
      print("Proper usage: generate_hmm_workers(amount_workers, output_to_file, dataset_index, input_dataset")
      stop("Stopping execution")
    }
  
    print(paste("Starting initialization phase for amount of workers = ", amount_workers))
    #let's get rid of this global variable, shall we?
    #AMOUNT_WORKERS <<- amount_workers
    
    #In the initialization phase, we pre-process the dataset and create the initial HMM based on the found observations,
    #passing the so generated HMM to the iterative phase.
    if(output_to_file == TRUE)
    { #%X for the time
      sink(paste("hmm_initialization_phase_", format(Sys.time(), "%a %b %d %Y.txt"), sep=""))
    }
    
    #NB: Only the initialization phase differs from dataset to dataset. The iterative phase is equal for any dataset
    init = initialization_phase(amount_workers, dataset_index, input_dataset)  
    
    if(output_to_file == TRUE)
    {
      print("Initialization phase completed")
    }
    
    #Uncomment just for debugging the iterative phase
    # HMMTrained = init[[1]]
    # thetaFrequentSequences = init[[2]]
    # theta = init[[3]]
    # logLikCur = init[[4]]
    # sequences = init[[5]]
    # sortedSequences = init[[6]]
    # LogLikUnconst = init[[7]]
    # library("parallel")
    # library("HMM")
    # library("hmm.discnp")
    
    k <- 1
    while(k <= 5)
    {
      if(output_to_file == TRUE)
      {
        #%X for the time
        sink(paste("hmm_iterative_phase_", k, "_interesting_sequences_", format(Sys.time(), "%a %b %d %Y.txt"), sep=""))
      }
      iterative_phase(amount_workers, k, init[[1]], init[[2]], init[[3]], init[[4]], init[[5]], init[[6]], init[[7]] )
      k <- k + 1
      print(paste("Now with k = ", k, sep=""))
    }
} 

validate_input_arguments <- function(amount_workers, output_to_file, dataset_index, input_dataset)
{
  success = FALSE
  
  #Firstly, validate input
  if(!(amount_workers %% 1 == 0))
  {
    print("You must pass a valid integer number as amount_workers")
  }
  else if(is.null(output_to_file))
  {
    print("You must pass either TRUE or FALSE as output_to_file")
  }
  else if(!(dataset_index == 1  || dataset_index == 2 || dataset_index == 3))
  {
    print("You must pass either '1' or '2' or '3' as dataset_index")
  }
  else if(is.null(dataset_index))
  {
    print("You must pass either TRUE or FALSE as output_to_file")
  }
  else if (is.null(input_dataset))
  {
    print("You must pass a valid input_dataset")
  }
  else
  {
    success = TRUE;
  }
  
  return(success)
}


iterative_phase <- function(amount_workers, k, HMMTrained, thetaFrequentSequences, theta, LogLikCur, sequences, sortedSequences, LogLikUnconst)
{
   
   continue=TRUE
    i = 1
  
    while(continue)
    {
      print(format(Sys.time(), "%a %b %d %X %Y"))
	  	print(paste("Generating theta- probable sequences..."))

	  	#Good old sequential version
	  	thetaProbableSequences <- getThetaProbableSequences(HMMTrained, theta)
	  	
    	print(format(Sys.time(), "%a %b %d %X %Y"))
    	print("Generating and sorting interesting sequences")

    	#it returns: [[1]]=conditionType [[2]] interesting sequences [[3]] interestingness. 
    	#Parallel version fixed, now working properly
    	interestingSequencesParts<-computeAllSequencesInterestingnessParallel(thetaFrequentSequences,thetaProbableSequences,HMMTrained,theta, amount_workers)
    	
    	#No interesting sequences identified --> Stop!
    	print(paste("AMOUNT OF INT. SEQUENCES", length(interestingSequencesParts[1, ][[1]])))
    	
    	if(length(interestingSequencesParts[1, ][[1]]) == 0 )
    	{
    	  print(HMMTrained)
    	  print(paste("Stopping process for k = ", k, ". No interesting sequences identified. "))
    	  break
    	}
    	
    	interestingSequences <- combine_partitions_interesting_sequences(interestingSequencesParts)
    	#Good old sequential version
    	#interestingSequencesSequential <-computeAllSequencesInterestingness(thetaFrequentSequences,thetaProbableSequences,HMMTrained,theta)
      	      	
    	#sort interesting sequences from which to select the symbols. It returns: [[1]]=conditionType (1 or 2) [[2]] interesting sequences [[3]] interestingness
    	sortedInterestingSequences<-sortSequencesByInterestingness(interestingSequences)   
    	
      print(format(Sys.time(), "%a %b %d %X %Y"))
    	print("Building Model with one more state and the following symbols")
      	
    	#select the symbols in toMoveSymbols that are in ALL DATA state. This is the simplest case in which the expert decides to move symbols only from ALL DATA. 
  		allDataEP<-HMMTrained$emissionProbs[1,]    
    	#The first sequence that has non-empty intersection with ALL DATA state. It returns the a vector of symbols   
  		#intersection contains the interesting sequences without the symbols already constrained on current states. NB: may contain symbols with length 0
		  intersection<-as.vector(sapply(sortedInterestingSequences[[2]],function(x)intersect(unlist(x),names(allDataEP[which(!allDataEP==0)]))))
		  show_top_k_interesting_sequences(intersection, k)
		  #q contains only the sequences with valid length ( > 0)
		  q<-unlist(lapply(intersection,`all.equal`,character(0)))	
		  
		  moveSymbolsContinue <- selectSymbolsTopKInterestingSequences(intersection, q, k, HMMTrained)
		  toMoveSymbols = moveSymbolsContinue[[1]]
		  continue = moveSymbolsContinue[[2]]
		  
      if(continue == TRUE)		  
      {
      	newState<-paste("State_",i)     	
      	#Build the HMM with the new states and the symols to move in. It returns: [[1]] the HMM with new state, [[2]] the symbols to move (toMoveSymbols), [[3]] the nr of states, and [[4]]the Model Loglikelihood 
      	
      	#sequences<-sequencesIDs[[1]]
      	newStateHMMConstrained <-newStateHMMTrainingConstrained(newState, toMoveSymbols, HMMTrained, sortedSequences, sequences$sample, LogLikUnconst)  
      	
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
      }
    	
    	print(format(Sys.time(), "%a %b %d %X %Y"))
    	print(continue)
    	
      	if(continue == TRUE)
      	{
      	  HMMTrained<-newStateHMMConstrained[[1]]
      	  movedSymbols<-newStateHMMConstrained[[2]]
      	  print(movedSymbols)
      	  nrStates<-newStateHMMConstrained[[3]]
      	  LogLikCur <-newStateHMMConstrained[[4]]
      	  print(LogLikCur)
      	  i<-i+1
      	  print(i)	
      	}
    }  
    #print(movedSymbols) 
    print("Final HMM before degrading is:")
    print(HMMTrained) 
    print(paste("Final Log Likelihood = ", LogLikCur))
    print(paste("Log Likelihood unconstrained = ", LogLikUnconst))
    print(paste("Final Amount of states = " , length(HMMTrained$States)))
    displaySymbolsPerState(HMMTrained)
    
    print(paste("DONE in function for k =", k))
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

#Input: a dataframe containingsequences by alphabetical (lexicographical order)
#Output: a vector containing the duration of each sequence based on the first message in it
compute_sequences_duration <- function(sequences)
{
  
}



