#Initial "global" variables
DATA_PATH = "Datasets_ALMA" #NB: There should be no other file in this folder other than the datasets to load
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
  sequences_marked_split = mcmapply(load_filter_alma_dataset_parallel, dataset_name = as.list(names_datasets_sorted), index = indexes, mc.cores = AMOUNT_WORKERS) 

  print("Finished parallel")
  print(Sys.time())
  #Now merge the different partitions into one data table
  sequences_marked = combine_sequences_marked(sequences_marked_split)
  
  return(list(sequences_marked, sequences_marked_split))
}


load_filter_alma_dataset_parallel <- function(dataset_name, index)
{
  #Fine, we got our dataset_name that is a relative or absolute location of the dataset being considered
  linesRead = readLines(dataset_name, skipNul=FALSE)  
  #Create a data frame
  dataLoaded = read.table(textConnection(linesRead), col.names = c("File", "Line", "Routine"), header=T)
  #Now we need to put together the "File" and "Routine" columns
  dataFrameMerged = data.frame(sample=(paste(dataLoaded$File, "::", dataLoaded$Routine, sep="")))
  #now assign one sequence ID to every message of the data frame
  sequenceIDs = find_sequence_ids_given_messages(dataFrameMerged$sample, index)
  dataFrameMerged$SequenceID = sequenceIDs
  
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



