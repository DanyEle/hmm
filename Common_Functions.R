
#Input: the set of sortedSequences
#       the amount of workers onto that the work is gonna be parallelized
#Output: X partitions, where sortedSequences is divided into X chunks. X = amount_workers passed as input.
find_partitions_for_sorted_sequences <- function(sortedSequences, amount_workers)
{
  #didn't find any automatic way, so let's do it by hand..
  
  partitions = list()
  partition_size = ceiling(length(sortedSequences) / amount_workers)
  
  start = 1
  
  for(i in 1:amount_workers)
  {
    partition_start = start
    
    partition_end = i * partition_size
    #if going slightly beyond the actual length (because of ceiling function), then cut it
    if(partition_end >= length(sortedSequences))
    {
      partition_end = length(sortedSequences)
    }
    start = partition_end + 1
    #Great, found the start and end indexes. Now actually form the partitions from them.
    partitions[[i]] = sortedSequences[partition_start:partition_end]
    
    print(paste(" i = ", i, " start = ", partition_start, " end = ", partition_end))
  }
  
  return(partitions)
}


#Input: the set of sortedSequences
#       the amount of workers onto that the work is gonna be parallelized
#Output: X partitions, where sortedSequences is divided into X chunks. X = amount_workers passed as input.
find_partitions_for_sequences<- function(sequences, amount_workers)
{
  #didn't find any automatic way, so let's do it by hand..
  
  #Contains list of all the different data frames.
  partitions = list() #Can we have a list of data frames? Yes
  partition_size = ceiling(length(sequences$sample) / amount_workers)
  
  start = 1
  
  for(i in 1:amount_workers)
  {
    partition_start = start
    
    partition_end = i * partition_size
    #if going slightly beyond the actual length (because of ceiling function), then cut it
    if(partition_end >= length(sequences$sample))
    {
      partition_end = length(sequences$sample)
    }
    start = partition_end + 1
    #Great, found the start and end indexes. Now actually form the partitions from them.
    partitions[[i]] = data.frame(sequences$sample[partition_start:partition_end], sequences$timestamp[partition_start:partition_end]
                                 ,sequences$developer[partition_start:partition_end], sequences$SequenceID[partition_start:partition_end]
                                )
    colnames(partitions[[i]]) <- c("sample","timestamp", "developer", "SequenceID")
    
    print(paste(" i = ", i, " start = ", partition_start, " end = ", partition_end))
  }
  
  return(partitions)
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

  #Firstly normalize and remove weird content  
  for(i in 1:ncol(interestingSequencesParts))
  {
    #Check if the current one has invalid size (i.e: it does not own any interesting sequences)
    #Then, "normalize it"
    if(length(interestingSequencesParts[, i][[1]]) == 0)
    {
      interestingSequencesParts[, i][[2]] = list()
    }
  }
  
  interesting_sequences = combine_partitions_sequences(interestingSequencesParts)
  
  return(interesting_sequences)
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
initializeHMM <- function(pathToData)
{   
  
  #alternative code in case the set does not contain sequence IDs
  
  if(LOAD_CUSTOM_SEQUENCES == FALSE)
  {
    if(MARK_SEQUENCE_IDS == TRUE)
    {
      #Will add one column on the left in any case, containing an index per action
      SampleData <- read.csv(pathToData, sep=SEPARATOR_DATASET, header=T)
      #Actually identify the actions themselves now
      #mark the sequence ID if not already done. Output: Dataframe with all sequences in column 1 and their sequence ID on column 2
      sequences <- mark_sequences_of_actions_with_ID(SampleData)
      
    }
    else if(MARK_SEQUENCE_IDS == FALSE)
    {
      #use the following dataset if you already called mark_sequences_of_actions_with_ID. Sample has two columns: sample and ID. It contains all symbols including "Start" and "End"
      sequences<- read.csv(pathToData, sep=SEPARATOR_DATASET, header=T)
      # sequences contains columns "sample" and "SequenceID" 
      
    }
    
    #Good, data frame successfully loaded. Now remove irrelevant symbols
    
    
    print("Removing irrelevant symbols ")
    #pre-processing symbols: remove symbols that are not relevant. Output: sequences (with ID and cleaned from irrelevant symbols),symbols, theta, HMMTrained
    SymbolsToRemove = c("Navigate Back to HubPage", "Navigate to HubPage", "Start", "End")
    
    newSample<-sequences
    for(x  in SymbolsToRemove){
      temp<-newSample[-which(newSample$sample==x),]
      newSample<-temp
    }
    
    sequences = newSample
    
  }
  else if(LOAD_CUSTOM_SEQUENCES == TRUE)
  {
    sequences <- sequences_global
    
  }
  
  #Compute theta. Solved issue with theta being computed maxed on the max. sequence ID
  
  #FIXED ZE BUG
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

#Create a dataframe with sequences and their IDs
mark_sequences_of_actions_with_ID<-function(sample){
  sampleObs<-data.frame(sample)
  prova<-grep("Start", sampleObs$sample, invert=F)
  t<-0
  temp<-c(0*1:nrow(sampleObs))
  for(i in 1:(nrow(sampleObs))){
    if(i %in% prova){t<-t+1
    temp[i]<-t
    }else{t<-t
    temp[i]<-t}	
  }
  sampleObs$SequenceID<-temp
  return(sampleObs)	
}

load_custom_sequences_if_needed <- function()
{
  if(LOAD_CUSTOM_SEQUENCES == TRUE)
  {
    sequences = load_marked_sequences()
    return(sequences)
    
  }
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

################## This is Algorithm 2

#Sort sequences and their IDs. used in getThetaFrequentSequences
#Input: a data frame with multiple vectors (sequences, IDS) 
#Output: a list of two lists of vectors (sequences and IDs)
sortSequencesWithIDs <- function(sequences){
  print(format(Sys.time(), "%a %b %d %X %Y"))
  print("Sorting sequences and IDs")	
  #create three lists from sequences: one global, one for seqeunces and one for sequence'IDs
  
  print(length(unique(sequences$SequenceID)))

  #Fine, let's process the sequences in parallel. Fatto 30, facciam 31!
  #list_partitions_sequences = list()
  
  library(purrr)
  #list_partitions_sequences = find_partitions_for_sequences(sequences, amount_workers)
  #Doesn't fit in cache --> Causes too many cache faults. Parallel version with only 1 thread actually performs better!
  
  parts_lists = mcmapply(generateListsforSequences, sequences=list_partitions_sequences, mc.cores=1)
  
  sequencesLists = flatten(parts_lists[1, ])
  sequencesLists1 = flatten(parts_lists[2, ])
  sequencesLists2 = flatten(parts_lists[3, ])
  
  #good old sequential version
  #allListsSequences = generateListsforSequences(sequences)
  #sequencesListsSeq = allListsSequences[[1]]
  #sequencesLists1Seq = allListsSequences[[2]]
  #sequencesLists2Seq = allListsSequences[[3]]
  
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
  sequencesLists = lapply(unique(sequences$SequenceID), function(sequenceId) sequences[sequences$SequenceID==sequenceId, ])
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

chunk2 <- function(x,n) {
  split(x, cut(seq_along(x), n, labels = FALSE)) 
}




#Input: ThetaProbableValuesnoDups: List containing non-duplicated theta-probable sequences[[2]] and their corresponding probability[[1]]
#       ThetaFrequentValuesnoDups: List containing non-duplicated theta-frequent sequences[[2]] and their corresponding frequency[[1]]
#Output: the score for all input sequences
computeAllSequencesInterestingnessParallel <- function(thetaFrequentSequences, thetaProbableSequences, HMMTrained, theta, amount_workers){	
  #stores the case from that the score was computed.
  conditionTypes = vector()
  #stores sequences whose score has been computed
  interestingSequences = list()
  #stores the scores for the sequences.
  interestingnessValues = vector()
  h = 1
  
  #Partition ThetaSequencesUnion and process each chunk separately
  
  
  partitions_theta_union = chunk2(thetaSequencesUnion, amount_workers)
  list_partitions_theta_union = list()
  
  for(i in 1:length(partitions_theta_union))
  {
    list_partitions_theta_union[[i]] = partitions_theta_union[[i]]
  }
  
  # set operations
  thetaSequencesUnion = union(thetaFrequentSequences[[1]], thetaProbableSequences[[1]])
  #Theta-Frequent - Theta-Probable
  thetaSequencesSetDiffData = setdiff(thetaFrequentSequences[[1]], thetaProbableSequences[[1]])
  #Theta-Probable - Theta-Frequent
  thetaSequencesSetDiffModel = setdiff(thetaProbableSequences[[1]], thetaFrequentSequences[[1]])
  #Theta-Probable intersectin with Theta-Frequent (viceversa holds too)
  thetaSequencesIntersection = intersect(thetaProbableSequences[[1]], thetaFrequentSequences[[1]])
  
  
  partitions_interestingness = mcmapply(compute_interestingness_per_partition, partition_theta_union = list_partitions_theta_union, 
                                       thetaFrequentSequences=replicate(amount_workers, thetaFrequentSequences, FALSE), 
                                        thetaProbableSequences=replicate(amount_workers, thetaProbableSequences, FALSE),thetaSequencesSetDiffData=replicate(amount_workers ,thetaSequencesSetDiffData, FALSE),
                                       thetaSequencesSetDiffModel=replicate(amount_workers, thetaSequencesSetDiffModel, FALSE),
                                       thetaSequencesIntersection=replicate(amount_workers, thetaSequencesIntersection, FALSE), theta=theta, mc.cores = amount_workers)
  
  return(partitions_interestingness)
  
  #Fine, we have our partitions setup now. Pass the partitions to the function which will compute the interestingness
}

compute_interestingness_per_partition <- function(partition_theta_union, thetaFrequentSequences, thetaProbableSequences,
                                                  thetaSequencesSetDiffData, thetaSequencesSetDiffModel, thetaSequencesIntersection, theta)
{
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

selectSymbolsTopKInterestingSequences <- function(intersection, q, k)
{
  toMoveSymbolsUnion <- c()   
  
  for(i in 1:k)
  {
    toMoveSymbolsCur <- unlist(intersection[which(!q==TRUE)[[i]]])
    toMoveSymbolsUnion <- union(toMoveSymbolsUnion, toMoveSymbolsCur)
  }
  
  return(toMoveSymbolsUnion)
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



#DANIELE: keep the sequential code here as back, just in case
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



#used in generateProbableSquences. It creates twoglobal list variables (<<-) ThetaProbableProbabilities, ThetaProbableSequences. It generates all the sequences with prob>theta not only the longest.
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


##DANIELE DESPERATION:



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
