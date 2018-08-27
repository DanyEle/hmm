#Initial "global" variables
#Locations of the input files
#Now passing these variables as arguments to the function call itself
#SOURCE_PATH = "Datasets_ALMA/LogSourceInformation.txt"
#EVENT_PATH = "Datasets_ALMA/LogEventSet.txt"
#RUNTIME_PATH = "Datasets_ALMA/LogRuntimeInformation.txt"


########################### 
#####MAIN FUNCTION#########
###########################
#input_dataset = [SOURCE_PATH, EVENT_PATH, RUNTIME_PATH]
load_marked_sequences_alma <- function(amount_workers, input_dataset)
{
  MESSAGES_PER_SEQUENCE = 15
  
  source_path = input_dataset[1]
  event_path = input_dataset[2]
  runtime_path = input_dataset[3]
  
  
  amount_rows = get_amount_rows_from_file(source_path) 
  #find start and end indexes for the input dataset based on the amount of workers passed
  start_end_indexes_dataset <- find_start_end_indexes_dataset(MESSAGES_PER_SEQUENCE, amount_workers, amount_rows) 
  #now load up the dataset and break it into smaller partitions consisting of data frames
  dataframes_partitions_dataset <- load_partitions_dataset(start_end_indexes_dataset, source_path)  
  indexes = find_indices_for_partitions(dataframes_partitions_dataset) 
  
  #Now we also need to partition the EVENT_PATH based on the start_end_indexes found
  eventPartitions <- load_partitions_event_runtime_path(start_end_indexes_dataset, event_path, 5) 
  runtimePartitions <- load_partitions_event_runtime_path(start_end_indexes_dataset, event_path, 2) 
  
  
  print(paste("Starting parallel dataset", source_path  , " with ", amount_workers, " workers"))
  library("parallel")
  
  #debug: 
  #partition_dataframe = dataframes_partitions_dataset[[1]]
  #index = indexes[[1]]
  #eventPartitions = eventPartitions[[1]]
  #runtimePartitions = runtimePartitions[[1]]
  
  #merge_filter_alma_dataset_parallel(partition_dataframe, index, linesReadEvent, linesReadRuntime)
  
  print(paste("Reading datasets as table in parallel with", amount_workers, " workers"))
  
  sequences_marked_split = mcmapply(merge_filter_alma_dataset_parallel, partition_dataframe = dataframes_partitions_dataset, index = indexes, 
                                          eventPartitions = eventPartitions, runtimePartitions = runtimePartitions, mc.cores = amount_workers ) 
        
  print("Finished parallel")
  print(Sys.time())
  #Now merge the different partitions into one data table
  sequences_marked = combine_sequences_marked(sequences_marked_split)
    
  return(list(sequences_marked, sequences_marked_split))
}


#Merge the "File" and "Routine" columns, then remove all the ones not matching the condition for them
merge_filter_alma_dataset_parallel <- function(partition_dataframe, index, eventPartitions, runtimePartitions)
{
  
  #getting the linesRead as input, now actually turning them into a proper data frame
  partition_dataframe = read.table(textConnection(partition_dataframe), col.names = c("File", "Line", "Routine"), header=T)
  
  #Now we need to put together the "File" and "Routine" columns for one partition
  #OK
  print("Merging data frame columns")
  partitionDataFrameMerged = data.frame(sample=(paste(partition_dataframe$File, "::", partition_dataframe$Routine, sep="")))
  #Firstly, need to assign the sequence ID. Then, need to filter. 
  print("Finding sequence ID")
  sequenceIDs = find_sequence_ids_given_messages(partitionDataFrameMerged$sample, index)
  partitionDataFrameMerged$SequenceID = sequenceIDs
  
  print("Filtering messages")
  sampleMessagesPartition = filter_messages_for_data_frame(partitionDataFrameMerged, eventPartitions, runtimePartitions)
  #sequenceIDs = find_sequence_ids_given_messages(sampleMessagesPartition, index)
  
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
  
  if(length(file_source_path) == 0)
  {
    stop(paste("No SOURCE DATASET found in the SOURCE_PATH ", source_path ," provided. Did you specify the source_dataset correctly?"))
  }
  amount_rows = length(readLines(file_source_path)) - 1 #-1 because of the header in the file
  close(file_source_path)
  return(amount_rows)
}





load_partitions_dataset <- function(start_end_indexes_dataset, source_path)
{
  print("reading lines...")
  #Fine, we got our dataset_name that is a relative or absolute location of the dataset being considered
  linesRead = readLines(source_path, skipNul=FALSE)  
  
  #fine, we got our data frame loaded. We now need to split it into as many partitions as indexes
  print("generating partitions from the dataset")
  partitions_data_loaded = find_partitions_for_sequences_given_start_end(linesRead, start_end_indexes_dataset)
  #keep it like this for now
  
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
  #Remove trailining whitespace
  eventPartitions = trimws(eventPartitions, which=c("right"))
  
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



