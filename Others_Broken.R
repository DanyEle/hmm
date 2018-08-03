
run_experiments_workers <- function()
{
  times_return = list()
  
  for(j in 1:10)
  {
    i = 1
    
    amount_workers = c(1, 2, 4, 8, 16, 32, 50, 64)
    sequential_times = c()
    parallel_times = c()
    overall_times = c()
    
    for(amount in amount_workers)
    {
      print(paste("Starting for ", amount))
      run_experiments_workers <- function()
      {
        times_return = list()
        
        for(j in 1:10)
        {
          i = 1
          
          amount_workers = c(1, 2, 4, 8, 16, 32, 50, 64)
          sequential_times = c()
          parallel_times = c()
          overall_times = c()
          
          for(amount in amount_workers)
          {
            print(paste("Starting for ", amount))
            AMOUNT_WORKERS <<- amount
            SEQUENTIAL_TIME <<- 0
            PARALLEL_TIME <<- 0
            
            gc()
            init = initialization_phase()  
            seq_time =  as.numeric(SEQUENTIAL_TIME, units="secs")
            par_time =  as.numeric(PARALLEL_TIME, units="secs")
            
            #print(paste("Workers: ", amount ))
            print(paste(" j = " , j,  " amount of workers = ", amount ,"Sequential time = ", as.numeric(seq_time, units="secs"), "Parallel time", as.numeric(par_time, units="secs")))
            
            parallel_times[i] = par_time
            sequential_times[i] = seq_time
            #Where is medieval times?
            overall_times[i] = par_time + seq_time
            
            i = i + 1
          }
          
          times_return[[j]] = list(sequential_times, parallel_times, overall_times)
          
        }
        
        return(times_return)
        
      }
      
      
      
      run_experiment_theta_frequent <- function(amount_workers)
      {
        AMOUNT_WORKERS <<- amount_workers
        runtimes_list = c()
        
        for(i in 1:10)
        {
          
          #Firstly, split the sorted sequences
          #Firstly, take all sortedSequences and find the start and end indexes to split them
          #start_end_indexes = return_partition_of_data_structure(length(sortedSequences), j)
          #partitions_sequences_loaded = find_partitions_for_sequences_given_start_end(sortedSequences, start_end_indexes)
          
          start_time = Sys.time()
          
          sequences_loaded_list_partitions <- load_custom_sequences_if_needed() 
          
          
          # parts_theta_frequent_sequences = mcmapply(getThetaFrequentSequences, sortedSequences = partitions_sequences_loaded, theta=theta, mc.cores = j)
          
          end_time = Sys.time()
          time_spent = end_time - start_time
          
          runtimes_list[i] = time_spent
          
          print(paste(" i = " , i,  " amount of workers = ", amount_workers ,"Time spent = ", as.numeric(time_spent, units="secs")))
          
        }
        print(runtimes_list)
        
        
      }
      
      
      run_experiment_train_baum_welch <- function()
      {
        sink("experiment_train_baum_welch_two_states.txt")
        
        runtimes_list = c()
        for(i in 1:10)
        {
          start_time = Sys.time()
          unconstrainedHMM <- trainBaumWelch(HMMInit, as.vector(sequences[[1]]))
          end_time = Sys.time()
          seq_time =  as.numeric((end_time - start_time), units="secs")
          
          runtimes_list[i] = seq_time
          print(paste("for i = ", i , " time = ", seq_time))
        }
        
        return(runtimes_list)
      }
      
      
      
      
      
      
      
      getThetaProbableSequencesParallel<-function(HMMTrained, theta){
        #Theta-Probable sequences as global variables
        symbols<-as.vector(HMMTrained$Symbols)
        column_names = c(1:length(symbols))
        thetaProbableProbabilities <<- list()
        #thetaProbableSequences <<- list()
        #thetaProbableSequences <<- data.frame(matrix(ncol = length(symbols)))
        #result will need to be put back into a list though!
        
        #initialize the index of the two global list variables ThetaProbableProbabilities, ThetaProbableSequences
        #populate the global list variables ThetaProbableProbabilities, ThetaProbableSequences.
        thetaProbableSequences = generateProbableSequencesParallel(HMMTrained, theta)
        
        #put them together into a single list.
        #thetaProbableValues = list(thetaProbableSequences, thetaProbableProbabilities)
        return(thetaProbableProbabilities)
      }   
      
      
      create_HMMs <- function(M, HMMTrained)
      {
        output_list = list()
        for(i in 1:M)
        {
          output_list[[i]] = HMMTrained
        }
        return(output_list)
      }
      
      
      
      #Daniele has fixed the data format (as.vector) for symbols 10.04.2017, verified:CORRECT
      generateProbableSequencesParallel<-function(HMMTrained, theta){
        symbols<-as.vector(HMMTrained$Symbols)
        M <- length(symbols)
        print(M)
        
        n = 10
        partitions = identify_partitions_symbols(n, symbols)
        
        HMMsTrained = create_HMMs(length(partitions), HMMTrained)
        
        
        all_theta_probable_sequences = mcmapply(process_symbols_of_partition, HMMTrained = HMMsTrained, theta=theta, symbols_partition = partitions, mc.cores = 8) 
        
        #somehow, also need to combine the result
        
        return(all_theta_probable_sequences)
      }
      
      #TEST: process_symbols_of_partition(HMMTrained, theta, partition, 1)
      
      process_symbols_of_partition <- function(HMMTrained,  theta, symbols_partition)
      {
        symbols_partition = array(unlist(symbols_partition))
        
        #print(theta)
        #print(HMMTrained)
        #print(symbols_partition)
        
        theta_probable_sequences_partition = list()
        
        for (j in 1:length(symbols_partition)) 
        {
          #Every worker gets a chuck of the symbols.
          print(j)
          # this is the first step from the default "start" state. We create a sequence of length two just to compute the probability of the first time step in a sequence. Such probability does not change by changing the second element of the sequence (symbols[1] or sumbols[2] etc)  	
          init <- symbols_partition[j]
          
          #browser()
          sequence <- c(init, symbols_partition[1])
          
          
          forwardProb = forward(HMMTrained, sequence)
          # this is the inductive case
          theta_probable_sequences_one_symbol = append(theta_probable_sequences_partition, generateProbableSequences_sequential_no_global(HMMTrained, init))
        }
        
        return(theta_probable_sequences_partition)
      }
      
      #Daniele has fixed the data format (as.vector) for symbols 10.04.2017, verified:CORRECT
      generateProbableSequences_sequential_no_global<-function(HMMTrained, theta){
        all_theta_probable_sequences = list()
        symbols<-as.vector(HMMTrained$Symbols)
        M <- length(symbols)
        print(M)
        for (j in 1:M) {
          print(j)
          # this is the first step from the default "start" state. We create a sequence of length two just to compute the probability of the first time step in a sequence. Such probability does not change by changing the second element of the sequence (symbols[1] or sumbols[2] etc)  	
          init <- symbols[j]
          print(init)
          sequence <- c(init, symbols[1])
          #print(sequence)
          Sys.sleep(2)
          forwardProb = forward(HMMTrained, sequence)
          # this is the inductive case
          all_theta_probable_sequences = append(all_theta_probable_sequences, generateHMMSequencesIteration(HMMTrained, init, forwardProb, theta, symbols))
        }
        
        return(all_theta_probable_sequences)
        
        #print(result)
        
      }
      
      
      
      #Input: M: size of the data structure to partition
      #       n: amount of workers that will process the work in parallel
      identify_partitions_symbols <- function(n, symbols)
      {
        M = length(symbols)
        partitions =  list() 
        
        delta = round(M / n, digits=0)
        
        start = 1
        end = delta
        continue = TRUE
        i = 0
        while(i < n && continue == TRUE)
        {
          end = (delta * (i + 1)) + 1
          
          start = (delta * i)  + 2
          if(i == 0)
          {
            start = 1
          }
          if(i == n - 1)
          {
            end = M
            continue = FALSE
          }
          
          i = i + 1
          
          print(i)
          print(paste(start, end))
          partitions[[i]] = as.list(symbols[start:end])
          
        }
        
        return(partitions)
        
      }
      
      
      
      
      
      
      #Daniele has fixed the data format (as.vector) for symbols 10.04.2017, verified:CORRECT
      generateProbableSequencesNewParallel<-function(HMMTrained, theta, amount_workers){
        symbols<-as.vector(HMMTrained$Symbols)
        M <- length(symbols)
        print(M)
        sequencesReturn = list()
        
        
        
        for (j in 1:M) 
        {
          print(j)
          # this is the first step from the default "start" state. We create a sequence of length two just to compute the probability of the first time step in a sequence. Such probability does not change by changing the second element of the sequence (symbols[1] or sumbols[2] etc)  	
          init <- symbols[j]
          sequence <- c(init, symbols[1])
          forwardProb = forward(HMMTrained, sequence)
          # this is the inductive case thetaProbableSequences[[j]] =
          thetaProbableSequence = generateHMMSequencesIterationNewParallel(HMMTrained, init, forwardProb, theta, symbols,  list())
          
          sequencesReturn[[length(sequencesReturn) + 1]] =  thetaProbableSequence
          
          print(paste("OUTPUT" , thetaProbableSequence))
        }
        
        #unlist possibly?
        return(sequencesReturn)
      }
      
      
      #used in generateProbableSquences. It creates twoglobal list variables (<<-) ThetaProbableProbabilities, ThetaProbableSequences. It generates all the sequences with prob>theta not only the longest.
      generateHMMSequencesIterationNewParallel <- function(HMMTrained, sequence, forwardProb, theta, symbols,  thetaProbableSequences)
      {	
        
        #print(sequence)
        
        if(length(sequence)==1)
        {
          forwardProbSum <- sum(exp(forwardProb[, 1]))
        }
        else
        {
          forwardProbSum <- sum(exp(forwardProb[, length(sequence)]))
        }
        if(forwardProbSum > theta)
        {
          if(length(sequence) > 0)
          {
            #Need a way to concatenate all the difference sequences and pass them over.
            #Somehow, need to save this
            sequences_list[length(sequences_list)] = sequence
            
          }
          #saved as global variables
          #the probability decreases with the addition of new symbols. Sooner or later the prob will be less than theta.
          #When this happens a new combination of 	symbols starts: see the output 	
          foreach (i=1:length(symbols)) %dopar%
          {
            sequenceIterative <- c(sequence, symbols[i])
            forwardProb = forward(HMMTrained, sequenceIterative)
            #this was the mistake. We need just sum up the last colum ("temp") each time
            #forwardProbSum <- sum(exp(logForwardProb[, temp]))
            generateHMMSequencesIterationNewParallel(HMMTrained, sequenceIterative, forwardProb, theta, symbols, thetaProbableSequences)
            #thetaProbableSequeEnces[[length(thetaProbableSequences) + 1]] =
          } 
          
          #thetaProbableSequences[[length(thetaProbableSequences) + 1]] = sequence
          #print(paste("Here, it is ", sequence))
        }
        
      }  
      
      
      
      
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
      
      
      ##FROM DAMEVSKI_PREPROCESSING.R
      
      load_marked_sequences_old <- function()
      {
        #1: Filter extremely rare messages (messages occurring in less than 3% of the developers)
        #[[1]] = all unique developers
        #[[2]] = all unique messages corresponding to the developer
        # print("Starting sequential")
        # print(Sys.time())
        #Sequential 
        # list_devs_messages = load_unique_messages_for_developers(DATA_PATH)
        #Array of messages, sequential
        # all_unique_messages = load_all_existing_unique_messages(INFO_PATH, list_devs_messages)
        #No rare messages found for the full dataset. No need to invoke this function as well. 
        #messages_to_remove = find_extremely_rare_messages(list_devs_messages, all_unique_messages) #NOT USED!
        # messages_to_remove = c()
        #2: Remove cursor movement messages (too frequent) and rare messages identified above from the full dataset loaded
        #We may omit passing messages_to_remove, because no rare messages were actually identified.
        #Also load the timestamp and the developer id, alongside the message
        #datasetAndPositions[[1]] = overall dataset containing all the observations. 
        #datasetAndPositions[[2]] = contains start and end position of each dataset
        # dataset_and_positions = load_and_filter_dataset(list_devs_messages, messages_to_remove = messages_to_remove, DATA_PATH)
        # sampleObs = dataset_and_positions[[1]]
        # startEndVectors = dataset_and_positions[[2]]
        #As long as the amount of datasets used
        # start_indexes_datasets = startEndVectors[[1]]
        # end_indexes_datasets = startEndVectors[[2]]
        # partitions = create_partitions_for_workers(start_indexes_datasets, end_indexes_datasets, sampleObs)
        # indexes = find_indices_for_partitions(partitions)
        # sequences_marked_split = mcmapply(mark_debug_sessions_with_ID, sampleObs = partitions, index = indexes, mc.cores = AMOUNT_WORKERS) 
      AMOUNT_WORKERS <<- amount
      SEQUENTIAL_TIME <<- 0
      PARALLEL_TIME <<- 0
      
      gc()
      init = initialization_phase()  
      seq_time =  as.numeric(SEQUENTIAL_TIME, units="secs")
      par_time =  as.numeric(PARALLEL_TIME, units="secs")
      
      #print(paste("Workers: ", amount ))
      print(paste(" j = " , j,  " amount of workers = ", amount ,"Sequential time = ", as.numeric(seq_time, units="secs"), "Parallel time", as.numeric(par_time, units="secs")))
      
      parallel_times[i] = par_time
      sequential_times[i] = seq_time
      #Where is medieval times?
      overall_times[i] = par_time + seq_time
      
      i = i + 1
    }
    
    times_return[[j]] = list(sequential_times, parallel_times, overall_times)
    
  }
  
  return(times_return)
  
}



run_experiment_theta_frequent <- function(amount_workers)
{
  AMOUNT_WORKERS <<- amount_workers
  runtimes_list = c()
  
  for(i in 1:10)
  {
    
    #Firstly, split the sorted sequences
    #Firstly, take all sortedSequences and find the start and end indexes to split them
    #start_end_indexes = return_partition_of_data_structure(length(sortedSequences), j)
    #partitions_sequences_loaded = find_partitions_for_sequences_given_start_end(sortedSequences, start_end_indexes)
    
    start_time = Sys.time()
    
    sequences_loaded_list_partitions <- load_custom_sequences_if_needed() 
    
    
    # parts_theta_frequent_sequences = mcmapply(getThetaFrequentSequences, sortedSequences = partitions_sequences_loaded, theta=theta, mc.cores = j)
    
    end_time = Sys.time()
    time_spent = end_time - start_time
    
    runtimes_list[i] = time_spent
    
    print(paste(" i = " , i,  " amount of workers = ", amount_workers ,"Time spent = ", as.numeric(time_spent, units="secs")))
    
  }
  print(runtimes_list)
  
  
}


run_experiment_train_baum_welch <- function()
{
  sink("experiment_train_baum_welch_two_states.txt")
  
  runtimes_list = c()
  for(i in 1:10)
  {
    start_time = Sys.time()
    unconstrainedHMM <- trainBaumWelch(HMMInit, as.vector(sequences[[1]]))
    end_time = Sys.time()
    seq_time =  as.numeric((end_time - start_time), units="secs")
    
    runtimes_list[i] = seq_time
    print(paste("for i = ", i , " time = ", seq_time))
  }
  
  return(runtimes_list)
}







getThetaProbableSequencesParallel<-function(HMMTrained, theta){
  #Theta-Probable sequences as global variables
  symbols<-as.vector(HMMTrained$Symbols)
  column_names = c(1:length(symbols))
  thetaProbableProbabilities <<- list()
  #thetaProbableSequences <<- list()
  #thetaProbableSequences <<- data.frame(matrix(ncol = length(symbols)))
  #result will need to be put back into a list though!
  
  #initialize the index of the two global list variables ThetaProbableProbabilities, ThetaProbableSequences
  #populate the global list variables ThetaProbableProbabilities, ThetaProbableSequences.
  thetaProbableSequences = generateProbableSequencesParallel(HMMTrained, theta)
  
  #put them together into a single list.
  #thetaProbableValues = list(thetaProbableSequences, thetaProbableProbabilities)
  return(thetaProbableProbabilities)
}   


create_HMMs <- function(M, HMMTrained)
{
  output_list = list()
  for(i in 1:M)
  {
    output_list[[i]] = HMMTrained
  }
  return(output_list)
}



#Daniele has fixed the data format (as.vector) for symbols 10.04.2017, verified:CORRECT
generateProbableSequencesParallel<-function(HMMTrained, theta){
  symbols<-as.vector(HMMTrained$Symbols)
  M <- length(symbols)
  print(M)
  
  n = 10
  partitions = identify_partitions_symbols(n, symbols)
  
  HMMsTrained = create_HMMs(length(partitions), HMMTrained)
  
  
  all_theta_probable_sequences = mcmapply(process_symbols_of_partition, HMMTrained = HMMsTrained, theta=theta, symbols_partition = partitions, mc.cores = 8) 
  
  #somehow, also need to combine the result
  
  return(all_theta_probable_sequences)
}

#TEST: process_symbols_of_partition(HMMTrained, theta, partition, 1)

process_symbols_of_partition <- function(HMMTrained,  theta, symbols_partition)
{
  symbols_partition = array(unlist(symbols_partition))
  
  #print(theta)
  #print(HMMTrained)
  #print(symbols_partition)
  
  theta_probable_sequences_partition = list()
  
  for (j in 1:length(symbols_partition)) 
  {
    #Every worker gets a chuck of the symbols.
    print(j)
    # this is the first step from the default "start" state. We create a sequence of length two just to compute the probability of the first time step in a sequence. Such probability does not change by changing the second element of the sequence (symbols[1] or sumbols[2] etc)  	
    init <- symbols_partition[j]
    
    #browser()
    sequence <- c(init, symbols_partition[1])
    
    
    forwardProb = forward(HMMTrained, sequence)
    # this is the inductive case
    theta_probable_sequences_one_symbol = append(theta_probable_sequences_partition, generateProbableSequences_sequential_no_global(HMMTrained, init))
  }
  
  return(theta_probable_sequences_partition)
}

#Daniele has fixed the data format (as.vector) for symbols 10.04.2017, verified:CORRECT
generateProbableSequences_sequential_no_global<-function(HMMTrained, theta){
  all_theta_probable_sequences = list()
  symbols<-as.vector(HMMTrained$Symbols)
  M <- length(symbols)
  print(M)
  for (j in 1:M) {
    print(j)
    # this is the first step from the default "start" state. We create a sequence of length two just to compute the probability of the first time step in a sequence. Such probability does not change by changing the second element of the sequence (symbols[1] or sumbols[2] etc)  	
    init <- symbols[j]
    print(init)
    sequence <- c(init, symbols[1])
    #print(sequence)
    Sys.sleep(2)
    forwardProb = forward(HMMTrained, sequence)
    # this is the inductive case
    all_theta_probable_sequences = append(all_theta_probable_sequences, generateHMMSequencesIteration(HMMTrained, init, forwardProb, theta, symbols))
  }
  
  return(all_theta_probable_sequences)
  
  #print(result)
  
}



#Input: M: size of the data structure to partition
#       n: amount of workers that will process the work in parallel
identify_partitions_symbols <- function(n, symbols)
{
  M = length(symbols)
  partitions =  list() 
  
  delta = round(M / n, digits=0)
  
  start = 1
  end = delta
  continue = TRUE
  i = 0
  while(i < n && continue == TRUE)
  {
    end = (delta * (i + 1)) + 1
    
    start = (delta * i)  + 2
    if(i == 0)
    {
      start = 1
    }
    if(i == n - 1)
    {
      end = M
      continue = FALSE
    }
    
    i = i + 1
    
    print(i)
    print(paste(start, end))
    partitions[[i]] = as.list(symbols[start:end])
    
  }
  
  return(partitions)
  
}






#Daniele has fixed the data format (as.vector) for symbols 10.04.2017, verified:CORRECT
generateProbableSequencesNewParallel<-function(HMMTrained, theta, amount_workers){
  symbols<-as.vector(HMMTrained$Symbols)
  M <- length(symbols)
  print(M)
  sequencesReturn = list()
  
  
  
  for (j in 1:M) 
  {
    print(j)
    # this is the first step from the default "start" state. We create a sequence of length two just to compute the probability of the first time step in a sequence. Such probability does not change by changing the second element of the sequence (symbols[1] or sumbols[2] etc)  	
    init <- symbols[j]
    sequence <- c(init, symbols[1])
    forwardProb = forward(HMMTrained, sequence)
    # this is the inductive case thetaProbableSequences[[j]] =
    thetaProbableSequence = generateHMMSequencesIterationNewParallel(HMMTrained, init, forwardProb, theta, symbols,  list())
    
    sequencesReturn[[length(sequencesReturn) + 1]] =  thetaProbableSequence
    
    print(paste("OUTPUT" , thetaProbableSequence))
  }
  
  #unlist possibly?
  return(sequencesReturn)
}


#used in generateProbableSquences. It creates twoglobal list variables (<<-) ThetaProbableProbabilities, ThetaProbableSequences. It generates all the sequences with prob>theta not only the longest.
generateHMMSequencesIterationNewParallel <- function(HMMTrained, sequence, forwardProb, theta, symbols,  thetaProbableSequences)
{	
  
  #print(sequence)
  
  if(length(sequence)==1)
  {
    forwardProbSum <- sum(exp(forwardProb[, 1]))
  }
  else
  {
    forwardProbSum <- sum(exp(forwardProb[, length(sequence)]))
  }
  if(forwardProbSum > theta)
  {
    if(length(sequence) > 0)
    {
      #Need a way to concatenate all the difference sequences and pass them over.
      #Somehow, need to save this
      sequences_list[length(sequences_list)] = sequence
      
    }
    #saved as global variables
    #the probability decreases with the addition of new symbols. Sooner or later the prob will be less than theta.
    #When this happens a new combination of 	symbols starts: see the output 	
    foreach (i=1:length(symbols)) %dopar%
    {
      sequenceIterative <- c(sequence, symbols[i])
      forwardProb = forward(HMMTrained, sequenceIterative)
      #this was the mistake. We need just sum up the last colum ("temp") each time
      #forwardProbSum <- sum(exp(logForwardProb[, temp]))
      generateHMMSequencesIterationNewParallel(HMMTrained, sequenceIterative, forwardProb, theta, symbols, thetaProbableSequences)
      #thetaProbableSequeEnces[[length(thetaProbableSequences) + 1]] =
    } 
    
    #thetaProbableSequences[[length(thetaProbableSequences) + 1]] = sequence
    #print(paste("Here, it is ", sequence))
  }
  
}  




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


##FROM DAMEVSKI_PREPROCESSING.R

load_marked_sequences_old <- function()
{
  #1: Filter extremely rare messages (messages occurring in less than 3% of the developers)
  #[[1]] = all unique developers
  #[[2]] = all unique messages corresponding to the developer
  # print("Starting sequential")
  # print(Sys.time())
  #Sequential 
  # list_devs_messages = load_unique_messages_for_developers(DATA_PATH)
  #Array of messages, sequential
  # all_unique_messages = load_all_existing_unique_messages(INFO_PATH, list_devs_messages)
  #No rare messages found for the full dataset. No need to invoke this function as well. 
  #messages_to_remove = find_extremely_rare_messages(list_devs_messages, all_unique_messages) #NOT USED!
  # messages_to_remove = c()
  #2: Remove cursor movement messages (too frequent) and rare messages identified above from the full dataset loaded
  #We may omit passing messages_to_remove, because no rare messages were actually identified.
  #Also load the timestamp and the developer id, alongside the message
  #datasetAndPositions[[1]] = overall dataset containing all the observations. 
  #datasetAndPositions[[2]] = contains start and end position of each dataset
  # dataset_and_positions = load_and_filter_dataset(list_devs_messages, messages_to_remove = messages_to_remove, DATA_PATH)
  # sampleObs = dataset_and_positions[[1]]
  # startEndVectors = dataset_and_positions[[2]]
  #As long as the amount of datasets used
  # start_indexes_datasets = startEndVectors[[1]]
  # end_indexes_datasets = startEndVectors[[2]]
  # partitions = create_partitions_for_workers(start_indexes_datasets, end_indexes_datasets, sampleObs)
  # indexes = find_indices_for_partitions(partitions)
  # sequences_marked_split = mcmapply(mark_debug_sessions_with_ID, sampleObs = partitions, index = indexes, mc.cores = AMOUNT_WORKERS) 
  
}


####NB: OBSOLETE METHOD. NO LONGER USED IN THE CURRENT VERSION!
load_and_filter_dataset <- function(list_devs_messages, messages_to_remove = c(), folder_datasets)
{
  #Useless, if passing symbols to remove already as input
  #messages_to_remove = c(messages_to_remove, "View.OnChangeCaretLine", "View.OnChangeScrollInfo")
  
  all_datasets = list.files(path=folder_datasets)
  
  start_dataset = c()
  end_dataset = c()
  
  print(paste("Loading all datasets in the folder ", folder_datasets))
  i = 1
  messages_loaded = c()
  timestamps_loaded = c()
  developers_loaded = c()
  
  start = 1
  
  for (dataset in all_datasets)
  {
    print(paste("Loading dataset from: ", dataset))
    dataLoaded <- read.csv(paste(folder_datasets,"/", dataset, sep=""), sep=SEPARATOR, header=T)
    timestamps_loaded_remove_symbols = as.array(dataLoaded$timestamp)
    developers_loaded_remove_symbols = as.array(dataLoaded$developer_id)
    messages_loaded_remove_symbols = as.array(dataLoaded$message)
    
    messages_filtered = messages_loaded_remove_symbols
    timestamps_filtered  = timestamps_loaded_remove_symbols
    developers_filtered = developers_loaded_remove_symbols
    
    
    #Here, actually filter out the symbols to remove
    for (message in messages_to_remove)
    {
      timestamps_filtered = timestamps_filtered[which(messages_filtered != message)]
      developers_filtered = developers_filtered[which(messages_filtered != message)]
      messages_filtered = messages_filtered[which(messages_filtered != message)]
    }
    
    messages_loaded[[i]] = messages_filtered
    timestamps_loaded[[i]] = timestamps_filtered
    developers_loaded[[i]] = developers_filtered
    
    amount_symbols_removed = nrow(messages_loaded_remove_symbols) - nrow(messages_filtered)
    
    
    #assign start and end of each dataset over here, subtracting the amount of symbols removed to properly set the end
    start_dataset[i] = start
    end_dataset[i] = start + nrow(as.array(dataLoaded$message)) - 1 - amount_symbols_removed
    
    print(paste("i " , i , " start = ", start_dataset[i], " end = ", end_dataset[i]))
    
    start = end_dataset[i] + 1
    
    i = i + 1
  }
  
  print("Merging datasets into a single data structure")
  #Now combine the three lists as columns of a single data frame, flattening them at the same time. Access them like:
  #sequences$sample, sequences$timestamp, sequences$developer
  sequences = do.call(rbind, Map(data.frame, sample=messages_loaded, timestamp=timestamps_loaded, developer=developers_loaded))
  
  #start_end_list[[1]] = array containing the start indexes of all datasets.
  #start_end_list[[2]] = array containing the end indexes of all datasets
  start_end_list = list(start_dataset, end_dataset)
  
  print(paste("Merged dataset has ", length(sequences$sample), " messages.", sep=""))
  
  
  return(list(sequences, start_end_list))
  
  #use functions display_symbols_occurrences and display_symbols_frequency to see how symbols' occurrences are distributed, if necessary
  #display_symbols_occurrences(sample)
  #display_symbols_frequency(sample)
  
}


load_all_existing_unique_messages <- function(INFO_PATH, list_devs_messages)
{
  dataset = "MessageList.csv"
  #Load all the unique messages now
  dataLoaded <- read.csv(paste(INFO_PATH,"/", dataset, sep=""), sep=SEPARATOR, header=T)
  return(as.array(dataLoaded$message))
  
}




create_partitions_for_workers <- function(start_indexes_datasets, end_indexes_datasets, sampleObs)
{
  partitions = list()
  
  for(i in 1:length(start_indexes_datasets))
  {
    partitions[[i]] = sampleObs[start_indexes_datasets[[i]]:end_indexes_datasets[[i]],]
  }
  return(partitions)
}





