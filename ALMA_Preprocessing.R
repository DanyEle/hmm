#Initial "global" variables
DATA_PATH = "Datasets_ALMA" #NB: There should be no other file in this folder other than the datasets to load
EVENT_PATH = "LogEventSet.txt"
RUNTIME_PATH = "LogRuntimeInformation.txt"
SEPARATOR = ","
#Print an update every X messages processed in the function "mark_debug_sessions_with_ID"
FREQUENCY_PRINT = 5000
#Amount of cores to be used
AMOUNT_WORKERS <<- 1
#Used to keep track of the time spent in the sequential and (potentially) parallel parts of the program
MESSAGES_PER_SEQUENCE = 15


load_marked_sequences_alma <- function()
{
  
  #names_size_datasets[[1]] = Vector containing names of all datasets
  #names_size_datasets[[2]] = Vector containing size of all datasets (in bytes)
  names_size_datasets = load_names_size_datasets(DATA_PATH)
  #Sort by the size of each dataset. Largest datasets will be processed first
  names_datasets_sorted = sort_datasets_names_by_size(names_size_datasets)  
  
  indexes = find_indices_for_partitions(names_datasets_sorted)
  print(paste("Starting parallel pre-processing", length(names_datasets_sorted), "datasets with ", AMOUNT_WORKERS, " workers"))
  library("parallel")
  sequences_marked_split = mcmapply(load_filter_alma_dataset_parallel, dataset_name = as.list(names_datasets_sorted), index = indexes, mc.cores = AMOUNT_WORKERS, 
                                    replicate(AMOUNT_WORKERS, EVENT_PATH, FALSE), replicate(AMOUNT_WORKERS, RUNTIME_PATH, FALSE)) 

  print("Finished parallel")
  print(Sys.time())
  #Now merge the different partitions into one data table
  sequences_marked = combine_sequences_marked(sequences_marked_split)
  
  return(list(sequences_marked, sequences_marked_split))
}


load_filter_alma_dataset_parallel <- function(dataset_name, index, event_log, runtime_log)
{
  #Fine, we got our dataset_name that is a relative or absolute location of the dataset being considered
  linesRead = readLines(dataset_name, skipNul=FALSE)  
  #Create a data frame
  dataLoaded = read.table(textConnection(linesRead), col.names = c("File", "Line", "Routine"), header=T)
  
  #Now we need to put together the "File" and "Routine" columns
  dataFrameMerged = data.frame(sample=(paste(dataLoaded$File, "::", dataLoaded$Routine, sep="")))
  
  sampleMessages = filter_messages_for_data_frame(dataFrameMerged, event_log, runtime_log)
  
  #now assign one sequence ID to every message of the data frame, after filtering them
  sequenceIDs = find_sequence_ids_given_messages(dataFrameMerged, index)
  dataFrameMerged$SequenceID = sequenceIDs
  dataFrameMerged$sampleMessages
  
  return(dataFrameMerged)
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


filter_messages_for_data_frame <- function(dataFrameMerged, event_log, runtime_log)
{
  ##### INFO EVENTS#####
  #firstly, load up all the event logs and check where we have a "INFO"
  linesReadEvent = readLines(event_log)  
  #Create a data frame
  dataLoadedEvent = read.table(textConnection(linesReadEvent[5:length(linesReadEvent)]),  header=FALSE,  col.names = c("Level", "Timestamp", "LogID", "Priority", "Audience", "Data", "Null"))
  #However, we are only interested in the "Level" Column actually
  #Indexes of all levels being "Info"
  levelsListInfo = which(dataLoadedEvent$Level == "Info")
  
  ###JAVA CONTAINER IN RUNTIME####
  
  #now load up all event logs that are javaContainer processes
  linesReadRuntime = readLines(runtime_log)  
  dataLoadedRuntime = read.table(textConnection(linesReadRuntime[1:length(linesReadRuntime)]),  header=TRUE,  col.names = c("Host", "Process", "Thread", "SourceObject"))
  
  library("stringr")
  
  ##NB DANIELE: USE AN "AND" HERE (?)
  #Get the indexes of which levels are actually "Info"
  processListRuntime =  which(!is.na(str_match(dataLoadedRuntime$Process, "javaContainer")))
  
  indexesIntersectionRunTimeEvent = intersect(levelsListInfo, processListRuntime)
  
  #now get all messages (rows) in the data frame at the indexes provided
  messages_filtered = dataFrameMerged[indexesIntersectionRunTimeEvent, ]

  return(messages_filtered)
}



