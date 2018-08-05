#Initial "global" variables
#Locations of the input files
SOURCE_PATH = "LogSourceInformation.txt"
EVENT_PATH = "LogEventSet.txt"
RUNTIME_PATH = "LogRuntimeInformation.txt"
#Print an update every X messages processed in the function "mark_debug_sessions_with_ID"
#Amount of cores to be used
AMOUNT_WORKERS <<- 1
#Used to keep track of the time spent in the sequential and (potentially) parallel parts of the program
MESSAGES_PER_SEQUENCE = 15





load_marked_sequences_alma <- function()
{
  amount_rows = get_amount_rows_from_file(SOURCE_PATH) #OK
  #find start and end indexes for the input dataset based on the amount of workers passed
  start_end_indexes_dataset <- find_start_end_indexes_dataset(MESSAGES_PER_SEQUENCE, AMOUNT_WORKERS, amount_rows) #OK
  #now load up the dataset and break it into smaller partitions consisting of data frames
  dataframes_partitions_dataset <- load_partitions_dataset(start_end_indexes_dataset, SOURCE_PATH)  #OK
  indexes = find_indices_for_partitions(dataframes_partitions_dataset) #OK
  
  #Now we also need to partition the EVENT_PATH based on the start_end_indexes found
  eventPartitions <- load_partitions_event_runtime_path(start_end_indexes_dataset, EVENT_PATH, 5) #OK
  runtimePartitions <- load_partitions_event_runtime_path(start_end_indexes_dataset, RUNTIME_PATH, 2) #OK
  

  print(paste("Starting parallel dataset", SOURCE_PATH  , " with ", AMOUNT_WORKERS, " workers"))
  library("parallel")
  
  #merge_filter_alma_dataset_parallel <- function(partition_dataframe, index, linesReadEvent, linesReadRuntime)
    
  sequences_marked_split = mcmapply(merge_filter_alma_dataset_parallel, partition_dataframe = dataframes_partitions_dataset, index = indexes, 
                                    eventPartitions = eventPartitions, runtimePartitions = runtimePartitions, mc.cores = AMOUNT_WORKERS ) 

  print("Finished parallel")
  print(Sys.time())
  #Now merge the different partitions into one data table
  sequences_marked = combine_sequences_marked(sequences_marked_split)
  
  return(list(sequences_marked, sequences_marked_split))
}


#Merge the "File" and "Routine" columns, then remove all the ones not matching the condition for it
merge_filter_alma_dataset_parallel <- function(partition_dataframe, index, eventPartitions, runtimePartitions)
{
  #Now we need to put together the "File" and "Routine" columns for one partition
  #OK
  partitionDataFrameMerged = data.frame(sample=(paste(partition_dataframe$File, "::", partition_dataframe$Routine, sep="")))
  #Firstly, need to assign the sequence ID. Then, need to filter. 
  sequenceIDs = find_sequence_ids_given_messages(partitionDataFrameMerged$sample, index)
  partitionDataFrameMerged$SequenceID = sequenceIDs
  
  sampleMessagesPartition = filter_messages_for_data_frame(partitionDataFrameMerged, eventPartitions, runtimePartitions)
  #  sequenceIDs = find_sequence_ids_given_messages(sampleMessagesPartition, index)

  
  #now assign one sequence ID to every message of the data frame, after filtering them
  dataFrameReturn = data.frame(data.frame(matrix(ncol = 2, nrow = length(sampleMessagesPartition$SequenceID))))
  colnames(dataFrameReturn) = c("sample", "SequenceID")
  
  
  if(length(dataFrameReturn$SequenceID) > 0)
  {
    dataFrameReturn$SequenceID = sampleMessagesPartition$SequenceID
    dataFrameReturn$sample = sampleMessagesPartition$sample
  }
  
  return(dataFrameReturn)
}




#dataset_begin = At which position (index) data actually starts from in the dataset
load_partitions_event_runtime_path <- function(start_end_indexes_dataset, event_runtime_path, dataset_begin)
{
  linesReadEvent = readLines(event_runtime_path)  
  linesReadEvent = linesReadEvent[dataset_begin:length(linesReadEvent)]
  
  #now actually need to partition it based on the indexes 
  partitionsEvents = find_partitions_for_sequences_given_start_end(linesReadEvent, start_end_indexes_dataset)
  #remove eventual NAs existing in the partitions
  for(i in 1:length(partitionsEvents))
  {
    partitionsEvents[[i]] = partitionsEvents[[i]][which(!is.na(partitionsEvents[[i]]))]
  }
  
  return(partitionsEvents)
}



get_amount_rows_from_file <- function(source_path)
{
  #Now, the dataset only consists of one file
  file_source_path <- file(source_path)
  amount_rows = length(readLines(file_source_path)) - 1 #-1 because of the header in the file
  close(file_source_path)
  return(amount_rows)
}




load_partitions_dataset <- function(start_end_indexes_dataset, source_path)
{
  #Fine, we got our dataset_name that is a relative or absolute location of the dataset being considered
  linesRead = readLines(source_path, skipNul=FALSE)  
  #Create a data frame
  dataLoaded = read.table(textConnection(linesRead), col.names = c("File", "Line", "Routine"), header=T)
  #fine, we got our data frame loaded. We now need to split it into as many partitions as indexes
  
  partitions_data_loaded = find_partitions_for_data_frame_given_start_end(dataLoaded, start_end_indexes_dataset)
  
  return(partitions_data_loaded)
}




#Input:
#start_end_indexes[[1]] = start
#start_end_indexes[[2]] = end
#Output: partitions of the dataFrame passed

find_partitions_for_data_frame_given_start_end <- function(dataLoaded, start_end_indexes)
{
  #Beautiful one liner to create partitions out of all sortedSequences passed over
  return(lapply(1:length(start_end_indexes[[1]]), function(i) dataLoaded[start_end_indexes[[1]][i]:start_end_indexes[[2]][i], ]))
}




find_sequence_ids_given_messages <- function(list_messages, index)
{
  vector_indexes = c()
  #initialize it with the index passed
  index_cur = index
  for(i in 1:length(list_messages))
  {
    
    if(i %% MESSAGES_PER_SEQUENCE == 1 && (i != 1))
    {
      index_cur = index_cur + 1
    }
    
    vector_indexes[i] = index_cur
    
  }
  return(vector_indexes)
}


filter_messages_for_data_frame <- function(partitionDataFrameMerged, eventPartitions, runtimePartitions)
{
  ####FILTER THE EVENT MESSAGES####
  #Create a data frame based on the events passed
  dataLoadedEvent = read.table(textConnection(eventPartitions[1:length(eventPartitions)]),  header=FALSE,  col.names = c("Level", "Timestamp", "LogID", "Priority", "Audience", "Data", "Null"))
  #However, we are only interested in the "Level" Column actually
  #Indexes of all levels being "Info"
  levelsListInfo = which(dataLoadedEvent$Level == "Info")
  
  
  #####FILTER THE RUNTIME MESSAGES
  #now load up all event logs that are javaContainer processes
  dataLoadedRuntime = read.table(textConnection(runtimePartitions[1:length(runtimePartitions)]),  header=FALSE,  col.names = c("Host", "Process", "Thread", "SourceObject"))
  library("stringr")
  processListRuntime =  which(!is.na(str_match(dataLoadedRuntime$Process, "javaContainer")))
  ##NB DANIELE: USE AN "AND" HERE
  indexesIntersectionRunTimeEvent = intersect(levelsListInfo, processListRuntime)
  
  #now get all messages (rows) in the data frame at the indexes provided
  messages_filtered = partitionDataFrameMerged[indexesIntersectionRunTimeEvent, ]

  return(messages_filtered)
}


find_start_end_indexes_dataset <- function(messages_seq, amount_workers, dataset_size)
{
  #first of all, divide the dataset size by the amount of workers
  dataset_partition_size = floor(dataset_size / amount_workers)
  
  partition_sizes = c()
  
  #make every single partition become a multiple of messages_seq
  for(i in 1:amount_workers)
  {
    remaining_rows_per_partition = dataset_partition_size - (floor(dataset_partition_size / messages_seq) * messages_seq)

    partition_sizes[i] = dataset_partition_size - remaining_rows_per_partition
  }
  
  #now get the remaining rows
  remaining_rows = dataset_size - sum(partition_sizes)
  #and assign the remaining rows to the last worker
  partition_sizes[length(partition_sizes)] = partition_sizes[length(partition_sizes)] + remaining_rows
  
  #nb: finally, sum(partition_sizes) == dataset_size

  #Good, all sizes found Now find the start and end indexes for each partition
  start_indexes = c()
  end_indexes = c()
  
  start = 1
  for(i in 1:amount_workers)
  {
    start_indexes[i] = start
    
    if(i == 1)
    {
      end_indexes[i] =  start + partition_sizes[i] - 1
    }
    else
    {
      end_indexes[i] = start_indexes[i] + partition_sizes[i] - 1
      
    }
    
    start = end_indexes[i] + 1
    print(paste("i = ", i , "start = ", start_indexes[i] , "end = ", end_indexes[i]))
    
  }
  
  return(list(start_indexes, end_indexes))
}



