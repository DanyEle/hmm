#Daniele Gadler, Andrea Janes, Barbara Russo
#Free University of Bolzano-Bozen 2016-2018



initialization_phase <- function()
{
  #Initialization begin
  #sequences_loaded_list_partitions[[1]] = sequences_loaded. (All sequences loaded)
  #sequences_loaded_list_partitions[[2]] = partitions_sequences_loaded partitions of all different sequences that have been loaded
  sequences_loaded_list_partitions <- load_marked_sequences_damevski() 
  
  
  sequences_loaded = sequences_loaded_list_partitions[[1]]
  partitions_sequences_loaded = sequences_loaded_list_partitions[[2]]
  
  print(format(Sys.time(), "%a %b %d %X %Y"))
  print(paste("Initializing the process..."))
  initialisedProcess<-initializeHMM(pathToData, sequences_loaded)
  #sequenceIDs is also sequences_global$sample
  
  sequences<-initialisedProcess[[1]]
  
  symbols<-initialisedProcess[[2]]
  #Do not compute theta if you want to freely pass it
  theta<-initialisedProcess[[3]]
  
  HMMTrained<-initialisedProcess[[4]]
  
  #create two lists: a list of sequences [[1]] and the corresponding list of IDs [[2]]. The list of seqeunces is also sorted (and accordingly the list of IDs)
  #DANIELE LOAD BEFOREHAND. Can do this once, load it in memory, then no longer need to do it. 
  list_partitions_sequences = find_list_partitions_given_data_frame_partitions(partitions_sequences_loaded)
  
  #Not really parallel. Only uses 1 worker, but multiple partitions. 
  sortedSequencesIDs <- sortSequencesWithIDs(list_partitions_sequences)
  
  #WAS GLOBAL!
  sortedSequences <- sortedSequencesIDs[[1]]
  
  #theta = length(sortedSequences)
  #sortedSequencesBeforeFiltering <- sortedSequences
  #EXPERIMENTAL FUNCTIONS. Not really necessary.
  #sortedSequences <<- filter_sequences_with_SU_if_needed(sortedSequences)
  #sortedSequences <<- filter_sequences_with_most_frequent_symbols(sortedSequences, sortedSequencesBeforeFiltering)
  
   print("Computing loglikelihood for ALL DATA state")
  #compute the loglikelihood of the model with one state
  library("hmm.discnp")
  #Generates a warning, but no problem!
  LogLikInit = logLikHmm(sortedSequences, list(Rho=t(HMMTrained$emissionProbs), tpm = HMMTrained$transProbs, ispd = HMMTrained$startProbs ) )
  print("init log likelihood")
  print(LogLikInit)
  print(format(Sys.time(), "%a %b %d %X %Y"))
  print("Generating theta-frequent sequences...")
  
  #Firstly, split the sorted sequences
  #Firstly, take all sortedSequences and find the start and end indexes to split them
  start_end_indexes = return_partition_of_data_structure(length(sortedSequences), 1) #AMOUNT_WORKERS
  partitions_sequences_loaded = find_partitions_for_sequences_given_start_end(sortedSequences, start_end_indexes)
  
  
  #To run with a large amount of workers(?)
  remove(sequences_loaded_list_partitions)
  remove(sequences_loaded)
  remove(sortedSequencesIDs)
  gc()
  #Causes lots of cache faults (i.e: high RAM demand) when employing many workers
  parts_theta_frequent_sequences = mcmapply(getThetaFrequentSequences, sortedSequences = partitions_sequences_loaded, theta=theta, mc.cores = 1)
  #parts_theta_frequent_sequences = mcmapply(getThetaFrequentSequences, sortedSequences = partitions_sequences_loaded, theta=theta, mc.cores = AMOUNT_WORKERS)
  
  #now combine the different parts found.
  thetaFrequentSequences = combine_partitions_sequences(parts_theta_frequent_sequences)
  
  #plain old sequential version
  #thetaFrequentSequencesSeq <-getThetaFrequentSequences(sortedSequences, theta)  
  #build the unconstrained model from scratch . The loglikelihood is always the same.  
  require(HMM)  
  print("Building an unconstrained model from scratch") 
  HMMInit = initHMM(States=c("state 1", "state 2"), symbols)
  
  #NO NEED FOR THIS - ALREADY LOADED IN MEMORY
  unconstrainedHMM <- trainBaumWelch(HMMInit, as.vector(sequences[[1]]))
  #data of unconstrained model
  EmissMatrixUnconst = unconstrainedHMM$emissionProbs
  TransMatrixUnconst = unconstrainedHMM$transProbs
  StartProbsUnconst = unconstrainedHMM$startProbs 
  #Generates a warning, but no problem!
  LogLikUnconst = logLikHmm(sortedSequences, list(Rho=t(EmissMatrixUnconst), tpm =  TransMatrixUnconst, ispd = StartProbsUnconst ) )
  print(paste("Loglik of unconstrained model with same nr of states: ", LogLikUnconst))
  #Was global!!  
  LogLikCur<-LogLikInit
  
  
  return(list(HMMTrained, thetaFrequentSequences, theta, LogLikCur, sequences, sortedSequences, LogLikUnconst))
}


find_list_partitions_given_data_frame_partitions <- function(partitions_sequences_loaded)
{ 
  return(lapply(1:ncol(partitions_sequences_loaded), function(i) partitions_sequences_loaded[, i]))
}



find_partitions_for_sequences_given_start_end <- function(sortedSequences, start_end_indexes)
{
  #Beautiful one liner to create partitions out of all sortedSequences passed over
   return(lapply(1:length(start_end_indexes[[1]]), function(i) sortedSequences[start_end_indexes[[1]][i]:start_end_indexes[[2]][i]]))
}







#Put all the different partitions together
combine_partitions_sequences <-function(parts_sequences)
{
  library(purrr)
  list_return = list()
  list_return[[1]] = flatten(parts_sequences[1, ])
  list_return[[2]] = flatten(parts_sequences[2, ])
  list_return[[3]] = flatten(parts_sequences[3, ])
  
  
  return(list_return)
}


#Put all the different partitions together
combine_partitions_interesting_sequences <-function(interestingSequencesParts)
{
  library(purrr)
  list_return = list()
  list_return[[1]] = unlist(flatten(interestingSequencesParts[1, ]))
  list_return[[2]] = flatten(interestingSequencesParts[2, ])
  list_return[[3]] = unlist(flatten(interestingSequencesParts[3, ]))
  
  print(paste("Amount of interesting sequences found is ", length(list_return[[3]])))
  
  return(list_return)
}






displaySymbolsPerState <- function(HMMTrained)
{
  #for all states
  for(i in 1:length(HMMTrained$States))
  {
    if(i == 1)
    {
      print(paste("All Data State has symbols:"))
      
    }
    else
    {
      print(paste("State ", i, " has symbols:"))
      
    }
    h = 1
    #for all symbols
    for(j in 1:length(HMMTrained$emissionProbs[i, ]))
    {
      #if valid symbol
      if(HMMTrained$emissionProbs[i, j] != 0)
      {
        #print the corresponding symbol name
        print(paste("[", h, "]", colnames((HMMTrained$emissionProbs))[j]))
        h = h + 1
      }
    }
  }
}



#To pre-process the dataset and train HMM. It computes theta
initializeHMM <- function(pathToData, sequences_loaded)
{   
  #old code used for the Gadler et al.'s paper. No need for pre-processing in that case.
  #SampleData <- read.csv(pathToData, sep=SEPARATOR, header=T)
  #sequences <- mark_sequences_of_actions_with_ID(SampleData)
  
  sequences = sequences_loaded
  #Compute theta. Solved issue with theta being computed based on the max. sequence ID
  
  K = length(unique(sequences$SequenceID))
  #just for seeing if it runs faster
  MinLengthSave = 2
  theta = MinLengthSave / K
  print("theta is ")
  print(theta)
  
  #initialize HMM with "All data" state
  states = c("All Data")
  #Get M as the number of unique symbols. do not use levels as it memorise the removed symbols: use unique
  symbols <-unique(sequences$sample)
  M<-length(symbols)
  
  #now generate a Baum-Welch trained HMM with the values passed.
  # select only the column for the symbols
  print("Training HMM ALL DATA")
  require(HMM)  
  observations <- as.vector(sequences$sample)
  HMMInit = initHMM(states, symbols) 
  
  HMMTrained <- trainBaumWelch(HMMInit, observations)
  
  return(list(sequences,symbols,theta,HMMTrained))
}



################## This is to train the HMM model

#Train the HMM model after initialization
#Input: HMMInit: an HMM initialized with the initHMM function, and the vector of sequences
#Output: An HMM with states, startprobs, transprobs, emissionprobs trained by the baum-welch algorithm.
trainBaumWelch <- function(HMMInit, observations){
  print("Running Baum-Welch algorithm")
  baumWelchAlgHMM <- baumWelchFixed(HMMInit, as.vector(observations))
  #print("Finishing Baum-Welch algorithm")
  
  #re-assign start probabilities
  HMMInit$startProbs = baumWelchAlgHMM$hmm$startProbs
  #re-assign transition probabilities
  HMMInit$transProbs = baumWelchAlgHMM$hmm$transProbs
  #re-assign emission probabilities
  HMMInit$emissionProbs = baumWelchAlgHMM$hmm$emissionProbs 
  
  return (HMMInit)
}

baumWelchFixed <- function (hmm, observations, maxIterations = 3, delta = 1e-09, pseudoCount = 0) {
  tempHmm <- hmm
  tempHmm$transProbs[is.na(hmm$transProbs)] = 0
  tempHmm$emissionProbs[is.na(hmm$emissionProbs)] = 0
  
  diff = c()
  for (i in 1:maxIterations){
    bw <- baumWelchRecursionFixed(tempHmm, observations)
    Tr <- bw$TransitionMatrix
    E <- bw$EmissionMatrix
    Tr[!is.na(hmm$transProbs)] = Tr[!is.na(hmm$transProbs)] + pseudoCount
    E[!is.na(hmm$emissionProbs)] = E[!is.na(hmm$emissionProbs)] + pseudoCount
    Tr <- (Tr/apply(Tr, 1, sum))
    E <- (E/apply(E, 1, sum))
    
    print(i)
    
    d <- sqrt(sum((tempHmm$transProbs - Tr)^2)) + sqrt(sum((tempHmm$emissionProbs - E)^2))
    diff <- c(diff, d)
    tempHmm$transProbs = Tr
    tempHmm$emissionProbs = E
    
    if (d < delta){
      break
    }
  }
  tempHmm$transProbs[is.na(hmm$transProbs)] = NA
  tempHmm$emissionProbs[is.na(hmm$emissionProbs)] = NA
  return(list(hmm = tempHmm, difference = diff))
}

#Modification of the original algorithm to consider one state
baumWelchRecursionFixed <- function (hmm, observations){
  TransitionMatrix = hmm$transProbs
  TransitionMatrix[, ] = 0
  EmissionMatrix = hmm$emissionProbs
  EmissionMatrix[, ] = 0
  f = forward(hmm, observations)
  b = backward(hmm, observations)
  probObservations = f[1, length(observations)]
  
  #Daniele Gadler & Prof. Russo's check: this prevents errors from arising in case of an HMM with a single state
  if(length(hmm$States) > 1){
    for (i in 2:length(hmm$States)) {
      j = f[i, length(observations)]
      if (j > -Inf) {
        probObservations = j + log(1 + exp(probObservations -   j))
      }
    }
  }
  
  for (x in hmm$States) {
    for (y in hmm$States) {
      temp = f[x, 1] + log(hmm$transProbs[x, y]) + log(hmm$emissionProbs[y, 
                                                                         observations[1 + 1]]) + b[y, 1 + 1]
      for (i in 2:(length(observations) - 1)) {
        j = f[x, i] + log(hmm$transProbs[x, y]) + log(hmm$emissionProbs[y, 
                                                                        observations[i + 1]]) + b[y, i + 1]
        if (j > -Inf) {
          temp = j + log(1 + exp(temp - j))
        }
      }
      temp = exp(temp - probObservations)
      TransitionMatrix[x, y] = temp
    }
  }
  for (x in hmm$States) {
    for (s in hmm$Symbols) {
      temp = -Inf
      for (i in 1:length(observations)) {
        if (s == observations[i]) {
          j = f[x, i] + b[x, i]
          if (j > -Inf) {
            temp = j + log(1 + exp(temp - j))
          }
        }
      }
      temp = exp(temp - probObservations)
      EmissionMatrix[x, s] = temp
    }
  }
  return(list(TransitionMatrix = TransitionMatrix, EmissionMatrix = EmissionMatrix))
}


#Sort sequences and their IDs. used in getThetaFrequentSequences
#Input: a data frame with multiple vectors (sequences, IDS) 
#Output: a list of two lists of vectors (sequences and IDs)
sortSequencesWithIDs <- function(list_partitions_sequences){
  print(format(Sys.time(), "%a %b %d %X %Y"))
  print("Sorting sequences and IDs")	
  #Doesn't fit in cache --> Causes too many cache faults. Parallel version with only 1 thread actually performs better!
  library("parallel")
  library("purrr")
  #convert partitions into data frame
  parts_lists = mcmapply(generateListsforSequences, sequences=list_partitions_sequences, mc.cores=1)
  
  sequencesLists = flatten(parts_lists[1, ])
  sequencesLists1 = flatten(parts_lists[2, ])
  sequencesLists2 = flatten(parts_lists[3, ])
  
  #Normalise the lenght of each list element to the max sequence length. It creates NA to fill the length. 
  #This is used to convert our lists in data.frame. length(sequenceLenghts) gives the number of sequences. 
  #It removes the null elements, i.e. i=3
  
  #get the amount of elements in every single element of the list "sequencesLists", i.e: the sequence length
  sequenceLengths <- sapply(sequencesLists, nrow)  
  dataNormalized1 <- as.data.frame(do.call(rbind,lapply(sequencesLists1, `length<-`,max(unlist(sequenceLengths)))),stringsAsFactors=FALSE)
  dataNormalized2 <- as.data.frame(do.call(rbind,lapply(sequencesLists2, `length<-`,max(unlist(sequenceLengths)))))
  
  #order sequences and IDs
  r<-do.call(order,as.list(dataNormalized1))
  myDataFrameS<-dataNormalized1[r,]
  myDataFrameID<-dataNormalized2[r,]
  newDataFrameS<-sapply(myDataFrameS,as.vector)
  newDataFrameID<-sapply(myDataFrameID,as.vector)
  colnames(newDataFrameS)<-NULL
  colnames(newDataFrameID)<-NULL
  
  #remove NA values form each sequence	
  orderedSequences<-list()
  orderedIDs<-list()
  
  for (i in 1:nrow(myDataFrameS))
  {
    orderedSequences[[i]]<-newDataFrameS[i,][-which(is.na(newDataFrameS[i,]))]
    orderedIDs[[i]]<-newDataFrameID[i,][-which(is.na(newDataFrameID[i,]))]
  }
  names(orderedSequences)<-rep("sequence",length(orderedSequences))
  names(orderedIDs)<-rep("sequenceID",length(orderedIDs))	  
  
  return (list(orderedSequences, orderedIDs)) 
}





generateListsforSequences <- function(sequences)
{
  sequences = as.data.frame(sequences)
  
  #This function was used to generate the memory addresses used in the analysis to understand why this function couldn't run with multiple cores
  #Namely, it keeps jumping to different memory locations 
  #sequencesLists = lapply(unique(sequences$SequenceID), function(sequenceId) tracemem(sequenceId))
  
  #sequencesLists = lapply(unique(sequences$SequenceID), function(sequenceId) tracemem(sequenceId))
  sequencesLists = lapply(unique(sequences$SequenceID), function(sequenceId) sequences[sequences$SequenceID==sequenceId, ]   )
  #for every element in the sequencesList, get the corresponding sample as a characer
  sequencesLists1 = lapply(sequencesLists, function(sequenceListElement) as.character(sequenceListElement$sample) )
  #And get the sequence ID as well
  sequencesLists2 = lapply(sequencesLists, function(sequenceListElement) sequenceListElement$SequenceID  )
  
  return (list(sequencesLists, sequencesLists1, sequencesLists2))
}


#Compute the frequency of a prefix "OHat". "t" is the length of OHat, i is index of the sequence containing OHat, and sortedSequences is the list of two lists: sorted sequences and their IDs list. Used only in generateThetaFrequentSequences()
computePrefixFrequency <- function(sortedSequences, i, OHat, t){
  n = 1
  j = i - 1
  if(j == 0)
  {
    j = 1
  }
  if(!j==0){
    numberOfSequences <- length(sortedSequences)
    
    #select first t elements
    Oj <-sortedSequences[[j]]
    OjHat <-head(Oj,t)
    
    #check sequences before the current analyzed one. THE PAPER IS WRONG HERE
    #Daniele: introduced i > 1 and j > 1 check to prevent going out of lower bounds if checking backwards.
    #Hei! Daniele in questo modo non conti il caso j==1 che invece va contato (per es. i=2 e j=1); ho coretto il codice come sotto
    while(j > 0 & isTRUE(all.equal(OjHat, OHat) == TRUE) ) {
      n = n + 1
      j = j - 1
      if(j>0){
        Oj <-sortedSequences[[j]]
        OjHat <-head(Oj,t)
      }
    }
  }
  j = i + 1
  if(j<=numberOfSequences){
    #update first element
    Oj <-sortedSequences[[j]]
    OjHat <-head(Oj,t)
    
    #check sequences after
    while(j <= numberOfSequences & isTRUE(all.equal(OjHat, OHat) == TRUE) ) {
      n = n + 1 
      j = j + 1
      if(j<=numberOfSequences){
        Oj <-sortedSequences[[j]]
        OjHat <-head(Oj,t)
      }
    }
  }
  return (n / numberOfSequences)
}

#Generate theta frequent sequences and their frequencies. It calls computePrefixFrequency().
#Input: list of two lists: the ordered sequences list and the respective IDs list
#Output: a list with two lists, so in a form of matrix:  
#list 1: A list with of theta-frequent sequences found
#list 2: A list with the frequency of the theta-frequent sequence
#list 3(actually not even necessary): A list with the ORIGINAL sequence in the sortedSequence to that the frequent sequences correspond
getThetaFrequentSequences <- function(sortedSequences,theta){
  numberOfSequences <- length(sortedSequences)  
  MinLengthSave = 2
  
  #Previous Prefix length
  Tp = 0
  
  #Index for saving the output.
  h = 1
  ThetaFrequentSequences = list()
  ThetaFrequentFrequencies = list()
  ThetaFrequentIDs=list()
  
  #i is the index for the overall list of vectors, so of the sequence. NOT ANYMORE AS NOT ALL THE SEQUENCE IDs ARE NOW IN THE SAMPLE.
  #ERROR: for goes to numberOfSequences - 1 (it was +1) AND OHat IS COMPUTED ON Tc NOT IN Tc
  for (i in 1:(numberOfSequences-1))
    {
    #Current Prefix Length
    Tc = 0
    #j is the index within the vector. 
    j = 1    
    OPrev<-sortedSequences[[i]]
    OCur<-sortedSequences[[i+1]]
    # ERROR FIXED: MISSING "=" in the while
    while((as.character(OPrev[j])) == (as.character(OCur[j])) & (j <= (min(length(OPrev), length(OCur))))){
      # print(j)
      Tc = Tc + 1
      j = j + 1 
    }
    
    #WHAT IF USING LIBRAY "qualV" and the FUNCTION LCS?
    #Longest sequence found, great! now let's compute its frequency.
    if (Tp != Tc){
      #this is how to get the prefix
      OHat = head(OPrev, Tc)
      
      if (Tc > 0){
        
        #print(i)
        #print(sortedSequences[[i]])
        f = computePrefixFrequency(sortedSequences, i, OHat, Tc)
      }else{
        f = 0
      }
      
      #Save theta frequent sequences
      if (length(OHat) >= MinLengthSave & f > 0 &  f >= theta){        
        ThetaFrequentSequences[[h]] = OHat
        ThetaFrequentFrequencies[[h]] = f
        ThetaFrequentIDs[[h]] = sortedSequences[[i]]
        h = h + 1
      }
      Tp = Tc
    }
  }
  ReturnThetaValues = list(ThetaFrequentSequences,ThetaFrequentFrequencies,ThetaFrequentIDs)
  
  return(ReturnThetaValues)
}

#This is algorithm 3 to generate theta interesting probable sequences. You must substitute "print" with "save"

############### Algorithm 3. Generate Probable sequences ###################


#used in generateProbableSquences. It creates twoglobal list variables (<<-) ThetaProbableProbabilities, ThetaProbableSequences. It generates all the sequences with prob>theta not only the longest.
generateHMMSequencesIterationParallel <- function(HMMTrained, sequence, forwardProb, theta, symbols, index) 
{
  results_list = list()
  
  if(length(sequence)==1){
    forwardProbSum <- sum(exp(forwardProb[, 1]))
  }else{forwardProbSum <- sum(exp(forwardProb[, length(sequence)]))}
  
  if(forwardProbSum > theta){
    #saved as global variables
    #Sys.sleep(0.01)
    #print(sequence)
    #thetaProbableSequences[[length(thetaProbableSequences)+1]] <<- sequence
    #print(sequence)
    #print(thetaProbableSequences)
    #thetaProbableProbabilities[[length(thetaProbableProbabilities)+1]] <<- forwardProbSum
    #index = index + 1
    results_list[[length(results_list) + 1 ]] = sequence
    
    #the probability decreases with the addition of new symbols. Sooner or later the prob will be less than theta. When this happens a new combination of 	symbols starts: see the output 	
    for (i in 1:length(symbols)) 
    {
      sequenceIterative <- c(sequence, symbols[i])
      forwardProb = forward(HMMTrained, sequenceIterative)
      #this was the mistake. We need just sum up the last colum ("temp") each time
      #forwardProbSum <- sum(exp(logForwardProb[, temp]))
      #browser()
      results_list = append(results_list, generateHMMSequencesIterationParallel(HMMTrained, sequenceIterative, forwardProb, theta, symbols, index))
      print(results_list)
    }    
  }
  
  return(results_list)
  
}  




#VERIFIED. REMOVED "Observation" and "sortedSequences" as parameters
computeSequenceInterestingness <- function(sequence, thetaFrequentSequences, thetaProbableSequences, HMMTrained, thetaSequencesSetDiffData, thetaSequencesSetDiffModel, thetaSequencesIntersection, theta){
  #now, we need to distinguish among 3 cases:
  IndexDataNotModel = Position(function(x) identical(x, sequence), thetaSequencesSetDiffData, nomatch = 0)
  IndexIntersection = Position(function(x) identical(x, sequence), thetaSequencesIntersection, nomatch = 0)
  IndexModelNotData = Position(function(x) identical(x, sequence), thetaSequencesSetDiffModel, nomatch = 0)
  
  #9a. It's in theta-frequent, but not in the model. Hence, we need to compute the probability of the sequence.
  if (IndexDataNotModel > 0){
    #Frequency of the considered sequence. As "Position" returns the first position there is no need to remove duplicates
    FrequencyIndex = Position(function(x) identical(x, sequence), thetaFrequentSequences[[1]], nomatch = 0)
    Frequency = thetaFrequentSequences[[2]][[FrequencyIndex]]
    
    #Compute the forward probability
    if(length(sequence)==1){
      # use c(sequence,sequence)as a trick to avoid subscripts out of bound. then take the first column only
      forwardProb = forward(HMMTrained, c(sequence,sequence))		
    }else{
      forwardProb = forward(HMMTrained, sequence)		
    }
    Probability <- sum(exp(forwardProb[, length(sequence)]))
    
    #Probability = computeSequenceProbability(HMMTrained, sequence, HMMTrained$States)
    
    #Compute interestingness
    Interestingness = Frequency - Probability
    
    #check theta 
    if(Interestingness >= theta){
      #1 stand for case 9a.
      return (c(1, Interestingness))
    }
    else
    {
      return(0)
    }
  }
  #9b. It's both in the data(theta-frequent) and in the model (theta-probable)
  else if(IndexIntersection > 0){
    #Frequency of the considered observation. Take it from the existing vector.
    FrequencyIndex = Position(function(x) identical(x, sequence), thetaFrequentSequences[[1]], nomatch = 0)
    Frequency = thetaFrequentSequences[[2]][[FrequencyIndex]]
    
    #Probability is already calculated in theta-probable sequences in this case. no need to do it again.
    IndexInThetaProbable = Position(function(x) identical(x, sequence), thetaProbableSequences[[1]], nomatch = 0)
    Probability = thetaProbableSequences[[2]][[IndexInThetaProbable]]
    #finally, we got the score for this sequence.
    Interestingness = Frequency - Probability
    
    if(Interestingness >= theta){
      #2 stand for case 9b.
      return (c(2, Interestingness))
    }
    else
    {
      return(0)
    }
  }
  #9c. It's in the model (Theta-probable), but not in the data(theta-frequent)
  #well supported by the model, but dooes not appear in data. 
  else if(IndexModelNotData > 0){
    
    IndexInThetaProbable = Position(function(x) identical(x, sequence), thetaProbableSequences[[1]], nomatch = 0)
    Probability = thetaProbableSequences[[2]][[IndexInThetaProbable]]
    Interestingness =  - Probability
    #2 stand for case 9b. uninteresting sequences
    return (c(3, Interestingness))
  }
}

#x = input dataset. n = amount of partitions
chunk2 <- function(x,n) {
  split(x, cut(seq_along(x), n, labels = FALSE)) 
}


#Courtesy of Dr. Massimo Torquati for the initial code skeleton and idea for good partitioning
#Input: length_data_structure = amount of rows of data strucure (or its length)
#       nw = amount of workers
#Output: list containing [[1]] = a vector of start indexes for partitions
#                        [[2]] = a vector of end indexes for partitions
return_partition_of_data_structure <- function(length_data_structure, nw)
{
  size = floor(length_data_structure / nw)
  more =  (length_data_structure %% nw) 
  start = 1
  stop = 0
  
  start_partitions = c()
  end_partitions = c()
  
  for(i in 1:nw)
  {
    start_local = stop + 1
    start = stop
    stop  = start + size + return_more(more) 
    
    print(paste("i = " , i , "start = ", start_local, " end = ", stop, " size = ", stop - start))
    
    more = more - 1
    start_partitions[i] = start_local
    end_partitions[i] = stop
  }
  
  return(list(start_partitions, end_partitions))
}

return_more <- function(more)
{
  if(more > 0)
  {
    return(1)
  }
  else
  {
    return(0)
  }
}
  


#Input: ThetaProbableValuesnoDups: List containing non-duplicated theta-probable sequences[[2]] and their corresponding probability[[1]]
#       ThetaFrequentValuesnoDups: List containing non-duplicated theta-frequent sequences[[2]] and their corresponding frequency[[1]]
#Output: the score for all input sequences
computeAllSequencesInterestingnessParallel <- function(thetaFrequentSequences, thetaProbableSequences, HMMTrained, theta, amount_workers){	
  #Partition ThetaSequencesUnion and process each chunk separately
  # set operations
  thetaSequencesUnion = union(thetaFrequentSequences[[1]], thetaProbableSequences[[1]])
  
  start_and_indexes = return_partition_of_data_structure(length(thetaSequencesUnion), amount_workers)

  list_partitions_theta_union = find_partitions_for_sequences_given_start_end(sortedSequences = thetaSequencesUnion, start_and_indexes)
  
  #Theta-Frequent - Theta-Probable
  thetaSequencesSetDiffData = setdiff(thetaFrequentSequences[[1]], thetaProbableSequences[[1]])
  #Theta-Probable - Theta-Frequent
  thetaSequencesSetDiffModel = setdiff(thetaProbableSequences[[1]], thetaFrequentSequences[[1]])
  #Theta-Probable intersectin with Theta-Frequent (viceversa holds too)
  thetaSequencesIntersection = intersect(thetaProbableSequences[[1]], thetaFrequentSequences[[1]])
  
  partitions_interestingness = mcmapply(compute_interestingness_per_partition, partition_theta_union = list_partitions_theta_union, 
                                       thetaFrequentSequences=replicate(amount_workers, thetaFrequentSequences, FALSE), 
                                        thetaProbableSequences=replicate(amount_workers, thetaProbableSequences, FALSE),
                                       thetaSequencesSetDiffData=replicate(amount_workers ,thetaSequencesSetDiffData, FALSE),
                                       thetaSequencesSetDiffModel=replicate(amount_workers, thetaSequencesSetDiffModel, FALSE),
                                       thetaSequencesIntersection=replicate(amount_workers, thetaSequencesIntersection, FALSE), 
                                       theta=replicate(amount_workers, theta, FALSE), 
                                       HMMTrained = replicate(amount_workers, HMMTrained, FALSE), mc.cores = amount_workers)
  
  return(partitions_interestingness)
  
  #Fine, we have our partitions setup now. Pass the partitions to the function which will compute the interestingness
}

compute_interestingness_per_partition <- function(partition_theta_union, thetaFrequentSequences, thetaProbableSequences,
                                                  thetaSequencesSetDiffData, thetaSequencesSetDiffModel, thetaSequencesIntersection, theta, HMMTrained)
{
  #stores the case from that the score was computed.
  conditionTypes = vector()
  #stores sequences whose score has been computed
  interestingSequences = list()
  #stores the scores for the sequences.
  interestingnessValues = vector()
  h = 1
  
  print(length(partition_theta_union))
  #loop through all sequences that belong either to Theta-Frequent or to Theta-Probable.
  for(i in 1:length(partition_theta_union)){
    sequence = partition_theta_union[[i]]
    conditionInterestingness = computeSequenceInterestingness(sequence, thetaFrequentSequences, thetaProbableSequences, HMMTrained, thetaSequencesSetDiffData, thetaSequencesSetDiffModel, thetaSequencesIntersection, theta)   
    #number corresponding to the condition met.
    conditionType = conditionInterestingness[1]
    #interestingness of sequence
    interestingness = conditionInterestingness[2]    
    #non-intersting seuences are not returned
    if(!conditionType %in% c(0,3)){
      conditionTypes[h] = conditionType
      interestingSequences[[h]] = sequence
      interestingnessValues[h] = interestingness
      h = h + 1
    }
  }
  return (list(conditionTypes, interestingSequences, interestingnessValues))
}


sortSequencesByInterestingness <- function(interestingSequences){
  r<-order(interestingSequences[[3]])
  newInterestingConditionTypes<-interestingSequences[[1]][r]
  newInterestingSequences<-interestingSequences[[2]][r]
  newInterestingInterestingness<-interestingSequences[[3]][r]
  return (list(newInterestingConditionTypes, newInterestingSequences, newInterestingInterestingness))
}

selectSymbolsTopKInterestingSequences <- function(intersection, q, k, HMMTrained)
{
  #Apply a reduce to the intersection
  amount_valid_symbols = length(unlist(intersection))
  #print(amount_valid_symbols)
  #print(unlist(intersection))

  if(amount_valid_symbols == 0)
  {
    print("Stopping process. No new symbols in the interesting sequences can be constrained to the new state. ")
    print(HMMTrained)
    return(list(intersection, FALSE))
  }
  else
  {
    toMoveSymbolsUnion <- c()   
    
    for(i in 1:k)
    {
      #Check if i is going out of range
      if(i >= length(unlist(intersection)))
      {
        print("Stopping process. Not enough new symbols present in the top k-interesting sequences")
        print(HMMTrained)
        return(list(intersection, FALSE))
      }
      print(paste("i = ", i, " length = ", length(unlist(intersection))))
      
      
      toMoveSymbolsCur <- unlist(intersection[which(!q==TRUE)[[i]]])
      toMoveSymbolsUnion <- union(toMoveSymbolsUnion, toMoveSymbolsCur)
    }
    return(list(toMoveSymbolsUnion, TRUE))
  }
}


#TO RUN. Update emission probabilities moving symbols in a new state
updateEmissionMatrix<-function(newState, toMoveSymbols, HMMTrained){
  symbols<-as.vector(HMMTrained$Symbols)
  states<-HMMTrained$States
  toMoveSymbols<-as.vector(unlist(toMoveSymbols))
  newEmissionMatrix<-matrix(ncol=length(symbols), nrow=length(states)+1, dimnames=list(c(states,newState),symbols))
  notMovedSymbols<-as.vector(setdiff(symbols, toMoveSymbols))
  t<-runif(length(toMoveSymbols))
  r<-t/sum(t)  
  #set to zero emission values for toMoveSymbols in ALL DATA (sink) state
  newEmissionMatrix[1, toMoveSymbols]<-0  
  #define emission values for toMoveSymbols in the newState (last row)
  newEmissionMatrix[nrow(newEmissionMatrix), toMoveSymbols]<-r
  #set to zero all the other emission values
  newEmissionMatrix[nrow(newEmissionMatrix),notMovedSymbols]<-0
  #normalise emission values for the not moved symbols in the ALL DATA (sink) state
  newEmissionMatrix[1,notMovedSymbols]<-HMMTrained$emissionProb[1,notMovedSymbols]/sum(HMMTrained$emissionProb[1, notMovedSymbols])
  #assign the values to the new emission matrix
  if(!nrow(HMMTrained$emissionProb)==1){
    newEmissionMatrix[2:(nrow(newEmissionMatrix)-1),symbols]<-HMMTrained$emissionProb[2:(nrow(newEmissionMatrix)-1),symbols]}else{   
    } 
  return(newEmissionMatrix)
}

newStateHMMTrainingConstrained <-function(newState, toMoveSymbols, HMMTrained, sortedSequences, sequences, LogLikUnconst){
  #Adding a new state 
  #build the emission matrix to move the new symbols from the sink state (ALL DATA) to the new state
  print("New state in constrained HMM:")
  print(newState)
  print("Symbols to move in constrained HMM")
  print(toMoveSymbols)
  newEmissionProb<-updateEmissionMatrix(newState, toMoveSymbols, HMMTrained)  
  #initialisation of a new HMM with the augmented set of states and the constrained emission matrix
  states = HMMTrained$States  
  symbols = HMMTrained$Symbols
  updatedInitHMM<-initHMM(c(states,newState), symbols,  emissionProbs= newEmissionProb)
  print("States:")
  print(c(states,newState))
  #optimise the HMM model
  constrainedTrainedHMM <-trainBaumWelch(updatedInitHMM, as.vector(sequences)) 
  #compute the loglikelihood and verify it is greter or equal to the unconstrained model with the same number of states
  modelPerformance = computeModelLogLikelihood(sortedSequences, sequences, constrainedTrainedHMM, LogLikUnconst, HMMTrained)
  
  # modelPerformance = computeModelLogLikelihood(sortedSequences, sequences, symbols, constrainedTrainedHMM)
  nrStates<- length(c(states, newState))
  return(list(constrainedTrainedHMM, toMoveSymbols, nrStates, modelPerformance[[3]], modelPerformance[[1]]))
}

#requires package "hmm.discnp"
#To be used in case the unconstrained emission matrix is build from scratch. Loglikhood does not change with no states: run it once for all
computeModelLogLikelihood <- function(sortedSequences, sequences, constrainedTrainedHMM, LogLikUnconst, HMMTrained){
  continue<-FALSE
  library("hmm.discnp")
  
  #data of constrained model
  EmissMatrixConst = constrainedTrainedHMM$emissionProbs
  TransMatrixConst = constrainedTrainedHMM$transProbs
  StartProbsConst = constrainedTrainedHMM$startProbs
  symbols<-constrainedTrainedHMM$Symbols
  states<-constrainedTrainedHMM$States
  #LogLikHmm requires list of sequences
  LogLikConst = logLikHmm(sortedSequences, list(Rho=t(EmissMatrixConst), tpm = TransMatrixConst, ispd = StartProbsConst ) )
  
  print(LogLikConst)
  print(LogLikUnconst)
  
  # the constrained model must have loglikelihood >= than the one of the unconstrained model 
  if(LogLikConst >= LogLikUnconst){
    continue=TRUE
    print("The Log-Likelihood is no-worse than the unconstrained model") 
    print(constrainedTrainedHMM)
  }else{
    #TODO: print HMM.
    print("Stopping process. The log-likelihood is worse than the unconstrained model. The previous model was the best one so far.Printing it here:")
    print(HMMTrained)
    continue=FALSE
    #stop("Stopping process. The log-likelihood is worse than the unconstrained model.")    
  }
  return(list(continue, constrainedTrainedHMM, LogLikConst)) 
}

#Compare log-likelihood at current iteration with log-likelihood in next iteration
compareModelLogLikelihoodAtIteration<-function(logLikCur, logLikNext){
  continue<-FALSE
  # the next model must have loglikelihood > than the one of the current model 
  if(logLikNext > logLikCur)
  {
    continue=TRUE
    print("log likelihood next HMM, more states ")
    print(logLikNext)
    print("log likelihood current HMM")
    print(logLikCur)
  }
  print("log likelihood next HMM, more states ")
  print(logLikNext)
  print("log likelihood current HMM")
  print(logLikCur)
  
  return(continue)

}



#sequential code here as back, just in case
getThetaProbableSequences<-function(HMMTrained, theta){
  #Theta-Probable sequences as global variables
  thetaProbableProbabilities <<- list()
  thetaProbableSequences <<- list()
  
  index <<- 1
  
  #initialize the index of the two global list variables ThetaProbableProbabilities, ThetaProbableSequences
  #populate the global list variables ThetaProbableProbabilities, ThetaProbableSequences.
  
  
  generateProbableSequences(HMMTrained, theta)
  
  #put them together into a single list.
  thetaProbableValues = list(thetaProbableSequences, thetaProbableProbabilities)
  return(thetaProbableValues)
}   

#Daniele has fixed the data format (as.vector) for symbols 10.04.2017, verified:CORRECT
generateProbableSequences<-function(HMMTrained, theta){
  symbols<-as.vector(HMMTrained$Symbols)
  M <- length(symbols)
  print(M)
  inputSymbolsIndexes = c(1:M)
  
  sapply(inputSymbolsIndexes, function(j)
  {
    print(j)
    # this is the first step from the default "start" state. We create a sequence of length two just to compute the probability of the first time step in a sequence. Such probability does not change by changing the second element of the sequence (symbols[1] or sumbols[2] etc)  	
    init <- symbols[j]
    sequence <- c(init, symbols[1])
    forwardProb = forward(HMMTrained, sequence)
    # this is the inductive case
    generateHMMSequencesIteration(HMMTrained, init, forwardProb, theta, symbols)
  })
}



#used in generateProbableSquences. It creates twoglobal list variables (movedSymbols) ThetaProbableProbabilities, ThetaProbableSequences. It generates all the sequences with prob>theta not only the longest.
generateHMMSequencesIteration <- function(HMMTrained, sequence, forwardProb, theta, symbols) {	
  if(length(sequence)==1){
    forwardProbSum <- sum(exp(forwardProb[, 1]))
  }else{forwardProbSum <- sum(exp(forwardProb[, length(sequence)]))}
  
  if(forwardProbSum > theta){
    #saved as global variables
    thetaProbableSequences[[index]] <<- sequence
    thetaProbableProbabilities[[index]] <<- forwardProbSum
    index <<- index + 1
    
    indexes_symbols = 1:length(symbols)
    sapply(indexes_symbols, function(i){
      sequenceIterative <- c(sequence, symbols[i])
      library("HMM")
      forwardProb = forward(HMMTrained, sequenceIterative)
      generateHMMSequencesIteration(HMMTrained, sequenceIterative, forwardProb, theta, symbols)
    })
  }
} 



#
#VERIFIED. REMOVED "Observation" and "sortedSequences" as parameters
computeSequenceInterestingness <- function(sequence, thetaFrequentSequences, thetaProbableSequences, HMMTrained, thetaSequencesSetDiffData, thetaSequencesSetDiffModel, thetaSequencesIntersection, theta){
  #now, we need to distinguish among 3 cases:
  IndexDataNotModel = Position(function(x) identical(x, sequence), thetaSequencesSetDiffData, nomatch = 0)
  IndexIntersection = Position(function(x) identical(x, sequence), thetaSequencesIntersection, nomatch = 0)
  IndexModelNotData = Position(function(x) identical(x, sequence), thetaSequencesSetDiffModel, nomatch = 0)
  
  #9a. It's in theta-frequent, but not in the model. Hence, we need to compute the probability of the sequence.
  if (IndexDataNotModel > 0){
    #Frequency of the considered sequence. As "Position" returns the first position there is no need to remove duplicates
    FrequencyIndex = Position(function(x) identical(x, sequence), thetaFrequentSequences[[1]], nomatch = 0)
    Frequency = thetaFrequentSequences[[2]][[FrequencyIndex]]
    
    #Compute the forward probability
    if(length(sequence)==1){
      # use c(sequence,sequence)as a trick to avoid subscripts out of bound. then take the first column only
      forwardProb = forward(HMMTrained, c(sequence,sequence))		
    }else{
      forwardProb = forward(HMMTrained, sequence)		
    }
    Probability <- sum(exp(forwardProb[, length(sequence)]))
    
    #Probability = computeSequenceProbability(HMMTrained, sequence, HMMTrained$States)
    
    #Compute interestingness
    Interestingness = Frequency - Probability
    
    #check theta 
    if(Interestingness >= theta){
      #1 stand for case 9a.
      return (c(1, Interestingness))
    }
    else
    {
      return(0)
    }
  }
  #9b. It's both in the data(theta-frequent) and in the model (theta-probable)
  else if(IndexIntersection > 0){
    #Frequency of the considered observation. Take it from the existing vector.
    FrequencyIndex = Position(function(x) identical(x, sequence), thetaFrequentSequences[[1]], nomatch = 0)
    Frequency = thetaFrequentSequences[[2]][[FrequencyIndex]]
    
    #Probability is already calculated in theta-probable sequences in this case. no need to do it again.
    IndexInThetaProbable = Position(function(x) identical(x, sequence), thetaProbableSequences[[1]], nomatch = 0)
    Probability = thetaProbableSequences[[2]][[IndexInThetaProbable]]
    #finally, we got the score for this sequence.
    Interestingness = Frequency - Probability
    
    if(Interestingness >= theta){
      #2 stand for case 9b.
      return (c(2, Interestingness))
    }
    else
    {
      return(0)
    }
  }
  #9c. It's in the model (Theta-probable), but not in the data(theta-frequent)
  #well supported by the model, but dooes not appear in data. 
  else if(IndexModelNotData > 0){
    
    IndexInThetaProbable = Position(function(x) identical(x, sequence), thetaProbableSequences[[1]], nomatch = 0)
    Probability = thetaProbableSequences[[2]][[IndexInThetaProbable]]
    Interestingness =  - Probability
    #2 stand for case 9b. uninteresting sequences
    return (c(3, Interestingness))
  }
}


#Input: ThetaProbableValuesnoDups: List containing non-duplicated theta-probable sequences[[2]] and their corresponding probability[[1]]
#       ThetaFrequentValuesnoDups: List containing non-duplicated theta-frequent sequences[[2]] and their corresponding frequency[[1]]
#Output: the score for all input sequences
computeAllSequencesInterestingness <- function(thetaFrequentSequences, thetaProbableSequences, HMMTrained, theta){	
  #stores the case from that the score was computed.
  conditionTypes = vector()
  #stores sequences whose score has been computed
  interestingSequences = list()
  #stores the scores for the sequences.
  interestingnessValues = vector()
  h = 1
  
  # set operations
  thetaSequencesUnion = union(thetaFrequentSequences[[1]], thetaProbableSequences[[1]])
  #Theta-Frequent - Theta-Probable
  thetaSequencesSetDiffData = setdiff(thetaFrequentSequences[[1]], thetaProbableSequences[[1]])
  #Theta-Probable - Theta-Frequent
  thetaSequencesSetDiffModel = setdiff(thetaProbableSequences[[1]], thetaFrequentSequences[[1]])
  #Theta-Probable intersectin with Theta-Frequent (viceversa holds too)
  thetaSequencesIntersection = intersect(thetaProbableSequences[[1]], thetaFrequentSequences[[1]])
  
  print(length(thetaSequencesUnion))
  #loop through all sequences that belong either to Theta-Frequent or to Theta-Probable.
  for(i in 1:length(thetaSequencesUnion)){
    sequence = thetaSequencesUnion[[i]]
    conditionInterestingness = computeSequenceInterestingness(sequence, thetaFrequentSequences, thetaProbableSequences, HMMTrained, thetaSequencesSetDiffData, thetaSequencesSetDiffModel, thetaSequencesIntersection, theta)   
    #number corresponding to the condition met.
    conditionType = conditionInterestingness[1]
    #interestingness of sequence
    interestingness = conditionInterestingness[2]    
    #non-intersting seuences are not returned
    if(!conditionType %in% c(0,3)){
      conditionTypes[h] = conditionType
      interestingSequences[[h]] = sequence
      interestingnessValues[h] = interestingness
      h = h + 1
    }
  }
  return (list(conditionTypes, interestingSequences, interestingnessValues))
}


###FUNCTIONS FOR PRE-PROCESSING
load_names_size_datasets <- function(folder_datasets)
{
  all_datasets = list.files(path=folder_datasets)
  
  #Array of all unique developers
  datasets_names = c()
  #Array of all unique messages corresponding to one developer
  datasets_sizes = c()
  print(paste("Getting all datasets' names and sizes in the folder ", folder_datasets))
  i = 1
  for (dataset in all_datasets)
  {
    dataset_name = paste(folder_datasets,"/", dataset, sep="")
    datasets_names[i] <- dataset_name
    datasets_sizes[i] <- file.size(dataset_name)
    i = i + 1
  }
  
  return(list(datasets_names, datasets_sizes))
}

sort_datasets_names_by_size <- function(names_size_datasets)
{
  names = names_size_datasets[[1]]
  sizes = names_size_datasets[[2]]
  
  #Sort the datasets' names according to their size in decreasing order
  names_sorted = sort(names)[ order(sizes, decreasing = TRUE)]
  
  return(names_sorted)
}

find_indices_for_partitions <- function(partitions)
{
  indices = list()
  index = 1
  for(i in 1:length(partitions))
  {
    indices[i] = index
    index = index + 10000000
  }
  return(indices)
}




