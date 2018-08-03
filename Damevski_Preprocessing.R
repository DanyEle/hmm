#Daniele Gadler, Andrea Janes, Barbara Russo
#Free University of Bolzano-Bozen 2016-2018


#Initial "global" variables
DATA_PATH = "Daniele_Test" #NB: There should be no other file in this folder other than the datasets to load
SEPARATOR = ","
#Print an update every X messages processed in the function "mark_debug_sessions_with_ID"
FREQUENCY_PRINT = 5000
#Amount of cores to be used
AMOUNT_WORKERS <<- 1
#Used to keep track of the time spent in the sequential and (potentially) parallel parts of the program




##########################
#####MAIN FUNCTION##############
#########################
load_marked_sequences_damevski <- function()
{
  
  messages_to_remove = c("View.OnChangeCaretLine", "View.OnChangeScrollInfo", "View.File",  "Debug.Debug Break Mode",
                         "Debug.Debug Run Mode",  "Debug.DebugType", "Debug.Enter Design Mode" ,"Build.BuildDone", "Build.BuildBegin")
  
  #names_size_datasets[[1]] = Vector containing names of all datasets
  #names_size_datasets[[2]] = Vector containing size of all datasets (in bytes)
  names_size_datasets = load_names_size_datasets(DATA_PATH)
  #Sort by the size of each dataset
  names_datasets_sorted = sort_datasets_names_by_size(names_size_datasets)
  
  indexes = find_indices_for_partitions(names_datasets_sorted)
  print("Pre-processing started")
  print(paste("Processing", length(names_datasets_sorted), "datasets with ", AMOUNT_WORKERS, " workers"))
  print("Starting parallel")
  
  library("parallel")
  #PARALLELISM ONLY WORKS ON LINUX, i.e: mc.cores > 2! If mc.cores = 1, then it executes sequentially
  
  sequences_marked_split = mcmapply(load_filter_dataset_given_name_parallel, dataset_name = as.list(names_datasets_sorted), index = indexes,
                                      outlier_symbols = replicate(length(names_datasets_sorted), messages_to_remove, FALSE), mc.cores = AMOUNT_WORKERS) 
  print("Finished parallel")
  print(Sys.time())
  #Now merge the different partitions into one data table
  sequences_marked = combine_sequences_marked(sequences_marked_split)
  
  return(list(sequences_marked, sequences_marked_split))
}







#Input: sampleObs: the sample of actions which is is to be split
#       no_cores: the amount of cores, also the amount of partitions into that the sampleObs is going to be split
#This script splits the input sampleObs into no_cores partitions, approximately
find_partitions_previous <- function(sampleObs, amount_partitions)
{
  partitions = list()

  i = 1
  #initialization of boundaries
  start = 1
  end = ceiling(nrow(sampleObs) / amount_partitions)
  
  size_partition = end
  print(paste("Data structure size = ", nrow(sampleObs)))
  
  while(i <= amount_partitions)
  {
    subset =  sampleObs[start:end,]
    partitions[[i]] = subset
    #Can split the sample here
    
    #continue after the end until the current developer is over and a new sequence can safely start
    end = find_splitting_point_different_developer(end, sampleObs, i, no_cores)
    end = end - 1
    
    #if almost at the end, then just go for the end
    if(end == (nrow(sampleObs) -1))
    {
      end = nrow(sampleObs)
    }
    
    subset = sampleObs[start:end,]
    
    print(paste("Partition [", i , "] start = ", start, " end = ", end, " size = ", nrow(subset) ))
    
    partitions[[i]] = subset
    #increase all indexes    
    start = end + 1
    
    if(end == nrow(sampleObs) || end == nrow(sampleObs) - 1)
    {
     print(paste("Identified #partitions with contiguous developer =", length(partitions)))
     return(partitions) 
    }
    
    end = min(end + size_partition, nrow(sampleObs))
    i = i + 1
  }
  
  return(partitions)
}



find_splitting_point_different_developer <- function(start,  sampleObs, i, no_cores)
{
  if(i == no_cores)
  {
    return(nrow(sampleObs))
  }
  
  last_developer = sampleObs$developer[start]
  
  #Check where developer changes
  for(i in start:nrow(sampleObs))
  {
    cur_developer = sampleObs$developer[i]
    
    #we have a new developer, then just return the index
    if(cur_developer != last_developer)
    {
      return(i)
    }
  }
  
  #Worst case, last point matches the end of the sample
  return(nrow(sampleObs))
  
}



load_filter_dataset_given_name_parallel <- function(dataset_name, outlier_symbols, index)
{
  #Great, now load the dataset passed and remove outliers from it
  dataLoaded = read.csv(dataset_name, sep=SEPARATOR, header=T)
  #outliers removed
  sampleObs = load_filter_single_dataset(outlier_symbols, dataLoaded)
  
  print(paste("Loaded " , dataset_name, " with ", length(sampleObs$sample), " messages.", sep=""))
  
  #Fine, dataset successfully loaded, Now let's start the actual processing of sequences
  return(mark_debug_sessions_with_ID(sampleObs, index))
}


load_filter_single_dataset <- function(messages_to_remove, dataLoaded)
{
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
  
  sequences = data.frame(sample=messages_filtered, timestamp = timestamps_filtered, developer = developers_filtered)
  
  return(sequences)
}




mark_debug_sessions_with_ID <- function(sampleObs, index){
  
  print(paste("Processing partition of size", nrow(sampleObs)))
  
  index_initial = index
  
  #List of messages representing interactions of developers with Visual Studio IDE for debugging
  msgs_IDE_interactions = ('^Debug.ToggleBreakpoint|Debug.CallStack|View.Call Stack|Debug.Start|Debug.StepOver|Debug.StepInto|Debug.StepOut|
                           Debug.AttachtoProcess|Debug.StartDebugTarget|Debug.StopDebugging|Debug.QuickWatch|Debug.AddWatch|View.Watch 1|
                           Debug.DisableAllBreakpoints|Debug.DetachAll|Debug.Restart|Debug.RunToCursor|Debug.EnableAllBreakpoints|
                           Debug.ShowNextStatement|Debug.BreakatFunction|Debug.StartPerformanceAnalysis|Debug.AddParallelWatch|
                           Debug.Threads|Debug.Disassembly|Debug.GoToDisassembly|Debug.EvaluateStatement|Debug.SetNextStatement|
                           Debug.Exceptions|Debug.BreakAll|Debug.Breakpoints|Debug.AddParallelWatch|Debug.Watch1|Debug.Modules|
                           Debug.Output|Debug.Print|Debug.DeleteAllBreakpoints|TestExplorer.DebugSelectedTests|
                           TestExplorer.DebugAllTestsInContext|TestExplorer.DebugAllTests|View.Locals$')
  
  
  indexes_matches <-grep(pattern=msgs_IDE_interactions, sampleObs$sample, invert=F)
 
  last_timestamp = strptime(sampleObs$timestamp[1], format="%Y-%m-%d %H:%M:%S")
  last_developer = sampleObs$developer[1]
  
  #if a sequence ID is 0, then the sequence is not part of a debugging session
  sequenceIds = c(0*1:nrow(sampleObs))
  
  print(paste("Marking all the messages in data frame of size", nrow(sampleObs)," with a debug sequence ID"))

  time_before_loop = proc.time()
  
  for(i in 1: (nrow(sampleObs)))
  {
    cur_timestamp = strptime(sampleObs$timestamp[i], format="%Y-%m-%d %H:%M:%S")
    cur_developer = sampleObs$developer[i]
    
    #We have a match. Now check if the timestamp of the current sequence is within 30 seconds of the last one
    if(cur_developer != last_developer)
    {
      index = index + 1
      sequenceIds[i] = index
      last_timestamp = cur_timestamp
      last_developer = cur_developer 
    }
    else if(i %in% indexes_matches)
    {
      sequenceIds[i] = index
      
      #it is within 30 seconds, then continue session and keep the same index
      if(cur_timestamp <= last_timestamp + 30)
      {
        sequenceIds[i] = index
      }
      #it is not within 30 seconds, then start a new debugging session increasing the index
      else
      {
        index = index + 1
        sequenceIds[i] = index
      }
      last_timestamp = cur_timestamp
      last_developer = cur_developer
    }
    else
    {
      #no match. again, check if the timestamp is within 30 seconds from the last action in the debugging session
      
      #is within it, then assign the sequence ID to the current action
      if( (cur_timestamp <= last_timestamp + 30))
      {
        sequenceIds[i] = index
      }
      else
      {
        #not within 30 seconds, then just assign 0. stray action not within a debugging session
        sequenceIds[i] = 0
      }
    }
    
    if(i %% FREQUENCY_PRINT == 0)
    {
      print(paste(i, " messages have been processed."))
    }
  }
  
  time_after_loop = proc.time()
  elapsed_time = time_after_loop[3] - time_before_loop[3]
  print(paste("Algorithm has run in ", elapsed_time, "s for data frame of size", nrow(sampleObs)))
  
  sampleObs$SequenceID<-sequenceIds
  #now remove from the data frame all the sequences that are marked with a sequence ID = 0
  sampleObsOutput = sampleObs[which(sampleObs$SequenceID != 0), ]
  
  last_sequence_id = sampleObsOutput$SequenceID[nrow(sampleObsOutput)]
  amount_sequences = last_sequence_id - index_initial + 1
  
  print(paste("Amount of sequences identified: ", (amount_sequences), sep=""))
  
  return(sampleObsOutput)	
}





