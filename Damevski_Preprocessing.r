DATA_PATH = "Datasets_Damevski_Small"
INFO_PATH = "Info_Dataset"

SEPARATOR = ","
THRESHOLD_RARE_MSG = 0.03

#Print an update every 2000 messages processes
FREQUENCY_PRINT = 2000



#######################
#####MAIN##############
#######################
load_marked_sequences <- function()
{
  
  #1: Filter extremely rare messages (messages occurring in less than 3% of the developers)
  #[[1]] = all unique developers
  #[[2]] = all unique messages corresponding to the developer
  list_devs_messages = load_unique_messages_for_developers(DATA_PATH)
  
  all_unique_messages = load_all_existing_unique_messages(INFO_PATH, list_devs_messages)
  
  messages_to_remove = find_extremely_rare_messages(list_devs_messages, all_unique_messages)
  
  
  #2: Remove cursor movement messages (too frequent) and rare messages identified above from the full dataset loaded
  #We may omit passing messages_to_remove, because no rare messages were actually identified.
  #Also load the timestamp and the developer id, alongside the message
  sampleObs = load_and_filter_dataset(list_devs_messages, messages_to_remove = c(), DATA_PATH)
  
  no_cores = 4
  partitions = find_partitions_based_on_cores(sampleObs, no_cores)
  
  indexes = find_indices_for_partitions(partitions)
  
  library("parallel")
  #PARALLELISM ONLY WORKS ON LINUX, i.e: mc.cores > 2!
  sequences_marked_split = mcmapply(mark_debug_sessions_with_ID, sampleObs = partitions, index = indexes, mc.cores = 1)
  
  sequences_marked = combine_sequences_marked(sequences_marked_split)
  
  return(sequences_marked)
  
  
  
}

combine_sequences_marked <- function(sequences_marked_split)
{
  list_data_frames = list()
  
  #combine the resulting data frames into a list
  for(i in ncol(sequences_marked_split))
  {
    data_frame_cur = as.data.frame(sequences_marked_split[, i])
    list_data_frames[[i]] = data_frame_cur
  }
  
  #now "unwrap" the list into one single data frame
  merged_data_frames = rbindlist(list_data_frames)
  
  return(merged_data_frames)
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

#Input: sampleObs: the sample of actions which is is to be split
#       no_cores: the amount of cores, also the amount of partitions into that the sampleObs is going to be split
#This script splits the input sampleObs into no_cores partitions, approximately
find_partitions_based_on_cores <- function(sampleObs, no_cores)
{
  partitions = list()

  i = 1
  #initialization of boundaries
  start = 1
  end = ceiling(nrow(sampleObs) / no_cores)
  
  size_partition = end
  print(paste("Data structure size = ", nrow(sampleObs)))
  
  while(i <= no_cores)
  {
    subset =  sampleObs[start:end,]
    partitions[[i]] = subset
    #Can split the sample here
    
    subset = sampleObs[start:end,]
    
    print(paste("Partition [", i , "] start = ", start, " end = ", end, " size = ", nrow(subset) ))
    
    partitions[[i]] = subset
    #increase all indexes    
    start = start + size_partition
    end = min(end + size_partition, nrow(sampleObs))
    i = i + 1
  }
  
  return(partitions)
}


#Apply the same function to every single piece.
find_splitting_point_in_half_and_developer_equal <- function(start, end, sampleObs)
{
  
  last_developer = sampleObs$developer[amount_rows_half]
  
  #Check where developer changes
  for(i in amount_rows_half:amount_rows)
  {
    cur_developer = sampleObs$developer[i]
    
    #we have a new developer, then just return the index
    if(cur_developer != last_developer)
    {
      return(i)
    }
  }
  
  #Worst case, last point matches the end of the sample
  return(amount_rows)
  
}






mark_debug_sessions_with_ID <- function(sampleObs, index){
  
  index_initial = index
  
  #List of messages representing interactions of developers with Visual Studio IDE for debugging
  msgs_IDE_interactions = ('^View.Locals|Dbug.QuickWatch|Debug.AddWatch|Debug.StepOver|Debug.StepInto|Debug.StepOut|De-
bug.SetNextStatement|Debug.RunToCursor|View.ImmediateWindow|Debug.Immediate|View.CallStack|Debug.CallStack|View.Autos|View.Output|Debug.Output
                           |Debug.StopDebugging|Debug.Start|Debug.StartDebugTarget|TestExplorer.DebugSelectedTests|Debug.Restart|Debug.AttachtoProcess|
                           TestExplorer.DebugAllTestsInContext|TestExplorer.DebugAllTests|View.SolutionExplorer|Debug.ToggleBreakpoint|Debug.EnableBreakpoint|
                           View.FindandReplace|View.FindResults1|View.SandoSearch|Edit.FindinFiles|Edit.GoToDefinition|View.FindSymbolResults|
                           Edit.FindAllReferences|ReSharper.ReSharper.GotoDeclaration|Edit.NavigateTo|ReSharper.ReSharper.FindUsages|Edit.GoToDeclaration|View.CallHierarchy$')
  
  
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
    if(i %in% indexes_matches)
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
      if( (cur_timestamp <= last_timestamp + 30) && (cur_developer == last_developer))
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
  print(paste("Algorithm has run in ", elapsed_time))
  
  sampleObs$SequenceID<-sequenceIds
  #now remove from the data frame all the sequences that are marked with a sequence ID = 0
  sampleObsOutput = sampleObs[which(sampleObs$SequenceID != 0), ]
  
  last_sequence_id = sampleObsOutput$SequenceID[nrow(sampleObsOutput)]
  amount_sequences = last_sequence_id - index_initial + 1
  
  print(paste("Amount of sequences identified: ", (amount_sequences), sep=""))
  
  
  return(sampleObsOutput)	
}



mark_debug_sessions_with_ID_parallel <- function(sampleObs){
  
  #List of messages representing interactions of developers with Visual Studio IDE for debugging
  msgs_IDE_interactions = ('^View.Locals|Dbug.QuickWatch|Debug.AddWatch|Debug.StepOver|Debug.StepInto|Debug.StepOut|De-
                           bug.SetNextStatement|Debug.RunToCursor|View.ImmediateWindow|Debug.Immediate|View.CallStack|Debug.CallStack|View.Autos|View.Output|Debug.Output
                           |Debug.StopDebugging|Debug.Start|Debug.StartDebugTarget|TestExplorer.DebugSelectedTests|Debug.Restart|Debug.AttachtoProcess|
                           TestExplorer.DebugAllTestsInContext|TestExplorer.DebugAllTests|View.SolutionExplorer|Debug.ToggleBreakpoint|Debug.EnableBreakpoint|
                           View.FindandReplace|View.FindResults1|View.SandoSearch|Edit.FindinFiles|Edit.GoToDefinition|View.FindSymbolResults|
                           Edit.FindAllReferences|ReSharper.ReSharper.GotoDeclaration|Edit.NavigateTo|ReSharper.ReSharper.FindUsages|Edit.GoToDeclaration|View.CallHierarchy$')
  
  
  indexes_matches <- grep(pattern=msgs_IDE_interactions, sampleObs$sample, invert=F)
  
  index = 1
  initial_timestamp = strptime(sampleObs$timestamp[1], format="%Y-%m-%d %H:%M:%S")
  initial_developer = sampleObs$developer[1]
  
  #if a sequence ID is 0, then the sequence is not part of a debugging session
  #sequenceIds = c(0*1:nrow(sampleObs))
  
  print("Marking all the messages with a debug sequence ID")
  
  
  library("foreach")
  library("doParallel")
  
  
  #These two commands need to be changed if running on Linux https://www.r-bloggers.com/parallel-r-loops-for-windows-and-linux/
  #cl <- makeCluster(2, outfile="debug.txt")
  #registerDoParallel(cl)
  
  time_before_loop = proc.time()
  
  sequenceIds = foreach(i = 1:nrow(sampleObs), .combine=c, .inorder=TRUE) %dopar%
  {
    cur_timestamp = strptime(sampleObs$timestamp[i], format="%Y-%m-%d %H:%M:%S")
    cur_developer = sampleObs$developer[i]
    
    if(i == 1)
    {
      last_timestamp = initial_timestamp
      last_developer = initial_developer
    }  
    else    
    {
      last_timestamp = strptime(sampleObs$lasttimestamp[i - 1], format="%Y-%m-%d %H:%M:%S")
      last_developer = sampleObs$lastdeveloper[i - 1]
    }
    
    print(paste("i = ", i , "last timestamp = ", last_timestamp, " cur timestamp = ", cur_timestamp , " last dev = " , last_developer , " cur dev = ", cur_developer))
    
    #We have a match. Now check if the timestamp of the current sequence is within 30 seconds of the last one
    if(i %in% indexes_matches)
    {
      #sequenceIds[i] = index
      
      #if it is within 30 seconds, then continue session and keep the same index
      #if it is not within 30 seconds, then start a new debugging session increasing the index
      if(cur_timestamp > last_timestamp + 30)
      {
        index = index + 1
        #sequenceIds[i] = index
      }
      
      
      #last_timestamp = cur_timestamp
      #last_developer = cur_developer
      
      sampleObs$lastdeveloper[i] = cur_developer
      sampleObs$lasttimestamp[i] = cur_timestamp
      #print(cur_timestamp)
      #return the index in the list
      index
    } 
    else    
    {
      #no match. again, check if the timestamp is within 30 seconds from the last action in the debugging session
      
      if(i == 1)
      {
        sampleObs$lasttimestamp[i] = initial_timestamp
        sampleObs$lastdeveloper[i] = initial_developer
      }   
      else
      {
        #assign the last valid timestamp to it
        sampleObs$lasttimestamp[i] = sampleObs$lasttimestamp[i - 1]  
        sampleObs$lastdeveloper[i] = sampleObs$lastdeveloper[i - 1]
      }  
      
      #is within it, then assign the sequence ID to the current action
      if( (cur_timestamp <= last_timestamp + 30) && (cur_developer == last_developer))
      {
        index
      }      
      else     
      {
        #not within 30 seconds, then just assign 0. stray action not within a debugging session
        0
      }
      
    }
    
    #if(i %% FREQUENCY_PRINT == 0)
    #{
    #  print(paste(i, " messages have been processed."))
    #}
    
  }
  #stopCluster(cl)
  
  time_after_loop = proc.time()
  
  elapsed_time = time_after_loop[3] - time_before_loop[3]
  
  print(paste("The loop has ran in ", elapsed_time , "seconds"))
  
  sampleObs$SequenceID<-sequenceIds
  
  sampleObsOutput = sampleObs[which(sampleObs$SequenceID != 0), ]
  
  print(paste("Amount of sequences identified: ", sampleObsOutput$SequenceID[nrow(sampleObsOutput)], sep=""))
  
  
  #now remove from the data frame all the sequences that are marked with a sequence ID = 0
  
  return(sampleObsOutput)	
}






#1 dataset = 1 developer. Will need to use double indexing for accessing multiple elements in it
load_unique_messages_for_developers <- function(folder_datasets)
{
 all_datasets = list.files(path=DATA_PATH)
 
 #Array of all unique developers
 all_devs = list()
 #Array of all unique messages corresponding to one developer
 all_unique_messages = list()
 print(paste("Getting all unique messages for developers from the datasets in the folder ", folder_datasets))
 i = 1
 for (dataset in all_datasets)
 {
   print(paste("Loading unique messages from dataset ", dataset))
   dataLoaded <- read.csv(paste(folder_datasets,"/", dataset, sep=""), sep=SEPARATOR, header=T)
   uniqueMessages = unique(dataLoaded$message)
   developer = dataLoaded$developer_id[1]
   all_devs[i] = developer
   all_unique_messages[[i]] = as.array(uniqueMessages)
   i = i + 1
 }
 return(list(all_devs, all_unique_messages))
}


load_all_existing_unique_messages <- function(INFO_PATH, list_devs_messages)
{
  dataset = "MessageList.csv"
  #Load all the unique messages now
  dataLoaded <- read.csv(paste(INFO_PATH,"/", dataset, sep=""), sep=SEPARATOR, header=T)
  return(as.array(dataLoaded$message))
  
}


#all_unique_messages: Array containing all the unique messages that can possibly occur in the dataset
#lists_devs_messages: List of lists, containing:
#[[1]] = all developers
#[[2]] = unique messages corresponding to the developer
find_extremely_rare_messages <- function(list_devs_messages, all_unique_messages)
{
  library(stringr)
  
  list_messages_from_developers = list_devs_messages[[2]]
  data_frame_messages = data.frame(message=1:length(all_unique_messages))
  
  i = 1
  for(message in all_unique_messages)
  {
    #Need to count how many times one message occurred within a list of lists
    wt <- data.frame(lineNum = 1:length(list_messages_from_developers))
    
    #escape all square brackets, else they wouldnt be matched
    message_escaped_1 = gsub('\\[', '\\\\[', message)
    message_escaped_2 = gsub('\\]', '\\\\]', message_escaped_1)
    pattern_match = paste("^",message_escaped_2,"$", sep="")
    wt$count <- sapply(list_messages_from_developers, function(x) sum(str_count(x, pattern=pattern_match)))
    occurrences = sum(wt$count)
    #print(paste(message, " found in ", occurrences , " developers."))
    
    data_frame_messages$message[i] = message
    data_frame_messages$count[i] = occurrences / length(list_messages_from_developers)
    i = i + 1
  }
  
  messages_to_remove = as.array(data_frame_messages$message[which(data_frame_messages$count <= THRESHOLD_RARE_MSG)])
  
  if(length(messages_to_remove) == 0)
  {
    print(paste("No actions occurs with <= ", THRESHOLD_RARE_MSG, " frequency"))
  }
  

  return(messages_to_remove)
  
}

load_and_filter_dataset <- function(list_devs_messages, messages_to_remove = c(), folder_datasets)
{
  #Too frequent messages
  messages_to_remove = c(messages_to_remove, "View.OnChangeCaretLine", "View.OnChangeScrollInfo")

  
  all_datasets = list.files(path=DATA_PATH)
  
  print(paste("Loading all datasets in the folder ", folder_datasets))
  i = 1
  messages_loaded = c()
  timestamps_loaded = c()
  developers_loaded = c()
  
  for (dataset in all_datasets)
  {
    print(paste("Loading dataset from: ", dataset))
    dataLoaded <- read.csv(paste(folder_datasets,"/", dataset, sep=""), sep=SEPARATOR, header=T)
    messages_loaded[[i]] = as.array(dataLoaded$message)
    timestamps_loaded[[i]] = as.array(dataLoaded$timestamp)
    developers_loaded[[i]] = as.array(dataLoaded$developer_id)
    
    i = i + 1
  }
  

  
  print("Merging datasets into a single data structure")
  #Now combine the three lists as columns of a single data frame, flattening them at the same time. Access them like:
  #sequences$sample, sequences$timestamp, sequences$developer
  sequences = do.call(rbind, Map(data.frame, sample=messages_loaded, timestamp=timestamps_loaded, developer=developers_loaded))
  
  print(paste("Merged dataset has ", length(sequences$sample), " messages.", sep=""))
  

  sequences_filtered = sequences
  
  #Here, actually filter out the symbols to remove
  for (message in messages_to_remove)
  {
    sequences_filtered = sequences_filtered[which(sequences_filtered$sample!=message),]
  }
  
  print(paste("After filtering frequent messages, dataset has ", length(sequences_filtered$sample), " messages.", sep=""))
  
  
  return(sequences_filtered)
  
  #use functions display_symbols_occurrences and display_symbols_frequency to see how symbols' occurrences are distributed, if necessary
  #display_symbols_occurrences(sample)
  #display_symbols_frequency(sample)
}





#input: dataset. list of symbols.
display_symbols_occurrences <- function(sample)
{
  table(sample)
  
  #increased bottom margin so as to show long text.
  par(mar= c(15, 4, 4, 2))
  #display graph with all labels.
  plot(table(sample), las=2, ann = FALSE)
}

#NOT VERIFIED YET
#Daniele: not really necessary.
#normalized by the overall amount of symbols' occurrences. from 0 to 1.
display_symbols_frequency<- function(sample)
{
  totalSymbolsAmount = length(sample)
  
  sampleFreq = table(sample) / totalSymbolsAmount
  
  #increased bottom margin so as to show long text.
  par(mar= c(15, 4, 4, 2))
  #display graph with all labels.
  plot(sampleFreq, las=2, ann = FALSE)
  
  return(sampleFreq)
}



