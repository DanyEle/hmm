#Daniele Gadler, Andrea Janes, Barbara Russo
#Free University of Bolzano-Bozen 2016-2017

#Barbara 
#pathToData <- "~/TeamDropBox/Dropbox/Thesis/R/Datasets/WindowsPhone_ID.csv"
#Daniele
pathToData <- "C:/Users/Daniele/Documents/Workplace_Damevski/Datasets_Daniele/WindowsPhone_WITH_ID.csv" 
#IMPORTANT! Dataset needs to have the following two headers:
#"sample","SequenceID"
#"sample": for the actions performed
#SequenceID (optional): for marking the sequence an action belongs to. If the SequenceID is not present,
#it can be added by uncommenting the mark_sequences_of_actions_with_id function

#sink(paste("Manager_Validation_OneYear_again.txt"))
SEPARATOR_DATASET = ";"
#Set this to TRUE this if the dataset has no "SequenceIDs" column
MARK_SEQUENCE_IDS = FALSE

#To be used for Damevski's dataset, in case we may need to carry out some pre-processing in other functions / files
LOAD_CUSTOM_SEQUENCES = FALSE


k <- 5



#To be executed line by line, following the instructions contained within the file
#Alternatively, can run via command line like:
#loopOverSequenceSet(DATA_PATH, k)
loopOverSequenceSet <- function(pathToData, k){
      print(format(Sys.time(), "%a %b %d %X %Y"))
	    print(paste("Initializing the process..."))
	    sequences_global <<- load_custom_sequences_if_needed() 
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
      
      #Need to change k?
      #k <- 2
      #ITERATION_BEGIN
      #while(continue){
        
      print(format(Sys.time(), "%a %b %d %X %Y"))
	  	print(paste("Generating probable sequences..."))

	  	thetaProbableSequences <- getThetaProbableSequences(HMMTrained, theta)
  
    	print(format(Sys.time(), "%a %b %d %X %Y"))
    	print("Generating and sorting interesting sequences")

    	#it returns: [[1]]=conditionType [[2]] interesting sequences [[3]] interestingness. TODO: extract the uninteresting sequences
    	interestingSequences<-computeAllSequencesInterestingness(thetaFrequentSequences, thetaProbableSequences, HMMTrained,theta)
      	      	
    	#sort interesting sequences from which to select the symbols. It returns: [[1]]=conditionType (1 or 2) [[2]] interesting sequences [[3]] interestingness
    	sortedInterestingSequences<-sortSequencesByInterstingness(interestingSequences)   
    	print("Showing Interesting sequences")
    	
    	displayTopKInterestingSequences(sortedInterestingSequences, k)

      print(format(Sys.time(), "%a %b %d %X %Y"))
    	print("Building Model with one more state and the following symbols")
      	
    	#select the symbols in toMoveSymbols that are in ALL DATA state. This is the simplest case in which the expert decides to move symbols only from ALL DATA. 
  		allDataEP<-HMMTrained$emissionProbs[1,]    
    	#The first sequence that has non-empty intersection with ALL DATA state. It returns the a vector of symbols   
		  intersection<-as.vector(sapply(sortedInterestingSequences[[2]],function(x)intersect(unlist(x),names(allDataEP[which(!allDataEP==0)]))))
		  q<-unlist(lapply(intersection,`all.equal`,character(0)))		
		  
		  #SKIP!!! Pick symbols from sequences manually
		  #toMoveSymbols <- selectSymbolsTopKInterestingSequences(intersection, q, k)

		  #ASK THE MANAGER for the indices of interesting sequences that should be put in the following vector
		  toMoveSymbols = c("Article: Search", "Article: open", "Navigate to ArticleStockChangeView", "Store Item")
		  AllSymbolsArray = colnames(HMMTrained$emissionProbs)
		  allSymbolsMatched = validatePickedSymbols(toMoveSymbols, AllSymbolsArray)
		  
		  
      #symbols in picked interesting sequences will be output.
		  #toMoveSymbols <- selectSymbolsPickedInterestingSequences(intersection, q,listIndecesInteresting)
		  
    	#ASK THE MANAGER WHAT THE NEW STATE NAME SHOULD BE
    	newStateName = "Change location of article in stock"
    	
    	newState<- newStateName  	
    	#Build the HMM with the new states and the symols to move in. It returns: [[1]] the HMM with new state, [[2]] the symbols to move (toMoveSymbols), [[3]] the nr of states, and [[4]]the Model Loglikelihood 
    	sequences<-sequencesIDs[[1]]
    	newStateHMMConstrained <-newStateHMMTrainingConstrained(newState, toMoveSymbols, HMMTrained, sortedSequences, sequences, LogLikUnconst)  
    	
    	LogLikConstrained<-newStateHMMConstrained[[4]]

    	# It returns: continue = FALSE or TRUE to be used for looping
    	#continue = FALSE --> Do not continue! Model quality has degraded or no interesting sequences have been found
    	#continue = TRUE --> Do continue! Model quality has increased and interesting sequences have been found
    	continue <- compareModelLogLikelihoodAtIteration(LogLikCur, LogLikConstrained)
    	
    	print(format(Sys.time(), "%a %b %d %X %Y"))
    	print(continue)
    	
    	#update only if log-lik has increased!
    	if(continue == TRUE)
    	{
       	HMMTrained<<-newStateHMMConstrained[[1]]
       	movedSymbols<<-newStateHMMConstrained[[2]]
       	print(movedSymbols)
       	nrStates<<-newStateHMMConstrained[[3]]
       	LogLikCur <<-newStateHMMConstrained[[4]]
       	print(LogLikCur)
    	}
     	
     	print(paste("Iteration ", i , " has finished."))
     	
     	print(paste("Need to continue process",  continue))
     	i<-i + 1
     	print(i)	
     	#GO BACK UP!
      #}  
     	#ITERATION_END--> go to ITERATION_BEGIN if continue == TRUE
      #print(movedSymbols) 
     	
     	
     	#if continue == FALSE, show resulting matrix
     	
      print(format(Sys.time(), "%a %b %d %X %Y"))
      print(HMMTrained) 
      print(paste("Final Log Likelihood = ", LogLikCur))
      print(paste("Log Likelihood unconstrained = ", LogLikUnconst))
      print(paste("Final Amount of states = " , nrStates))
      displaySymbolsPerState(HMMTrained)
      
      print("Done!")
}


displayTopKInterestingSequences <- function(sortedInterestingSequences, k)
{
  #print them all
  for(i in 1:k)
  {
    print("------------------------------")
    print(paste("[" , i , "]"))
    print(sortedInterestingSequences[[2]][[i]])
    print(paste("Score = ", sortedInterestingSequences[[3]][[i]]))
  }
}


displayTopKInterestingSequences <- function(sortedInterestingSequences, k)
{
  #print them all
  for(i in 1:k)
  {
    print("------------------------------")
    print(paste("[" , i , "]"))
    print(sortedInterestingSequences[[2]][[i]])
    print(paste("Score = ", sortedInterestingSequences[[3]][[i]]))
  }
}



#make sure the picked symbols do exist.
validatePickedSymbols <- function(toMoveSymbols, AllSymbolsArray)
{
  
  for(i in 1:length(toMoveSymbols))
  {
    doesSymbolHaveMatching = FALSE
    
    for(j in 1:length(AllSymbolsArray)) 
    {
      SymbolToMove = toMoveSymbols[i]
      SymbolToCheck = AllSymbolsArray[j]
      
      if(SymbolToMove == SymbolToCheck)
      {
        doesSymbolHaveMatching = TRUE
      }
    }
    if(doesSymbolHaveMatching == FALSE)
    {
      print(paste(SymbolToMove, " is wrong."))
    }
    else
    {
      print(paste(SymbolToMove, " is correct"))
      
    }
  }
}


selectSymbolsPickedInterestingSequences <- function(intersection, q, listIndecesInteresting)
{
  toMoveSymbolsUnion <- c()
  for(i in 1:length(listIndecesInteresting))
  {
    indexInList = listIndecesInteresting[i]
    
    toMoveSymbolsCur <- unlist(intersection[which(!q==TRUE)[[indexInList]]])
    toMoveSymbolsUnion <- union(toMoveSymbolsUnion, toMoveSymbolsCur)
  }
  return(toMoveSymbolsUnion)
}






