

########################### 
#####MAIN FUNCTION#########
###########################
#input_dataset = [SOURCE_PATH, EVENT_PATH, RUNTIME_PATH]
load_marked_sequences_alma <- function(amount_workers, input_dataset)
{
  source("/home/daniele/HMM_ALMA/init_michele.R")
  
  
  #daniele: actually use the functions initialized
  info_search = search("forType", "INFO")
  log_info_parse = parse(input_dataset)
  #log_info_parse = parse("/home/daniele/HMM_ALMA/APE1/SYSTEM/2017-03-13/log2017-03-12T22_48_45.319_2017-03-13T00_00_07.882.xml.gz")
  java_containers = where("container", "java")
  java_containers_filtered = 
  java_classes = select("file")
  java_routines = select("routine")
  
  amount_rows = get_amount_rows_from_data_structure(java_classes) 
  #find start and end indexes for the input dataset based on the amount of workers passed
  
  MESSAGES_PER_SEQUENCE = 15
  start_end_indexes_dataset <- find_start_end_indexes_dataset(MESSAGES_PER_SEQUENCE, amount_workers, amount_rows) 
  #now load up the dataset and break it into smaller partitions consisting of data frames
  java_classes_partitions <- load_partitions_data_structure(start_end_indexes_dataset, java_classes)  
  java_routines_partitions  <- load_partitions_data_structure(start_end_indexes_dataset, java_routines)
  
  indexes = find_indices_for_partitions(java_classes_partitions) 
  
  print(paste("Processing datasets in ", input_dataset  , " with ", amount_workers, " workers"))
  library("parallel")
  #debug: 
  #partition_java_classes = java_classes_partitions[[1]]
  #partition_java_routines = java_routines_partitions[[1]]
  #index = indexes[[1]]
  #merge_filter_alma_dataset_parallel(partition_java_class, partition_java_routine, index)
  
  print(paste("Reading datasets as table in parallel with", amount_workers, " workers"))
  
  sequences_marked_split = mcmapply(merge_filter_alma_dataset_parallel, partition_java_classes = java_classes_partitions, partition_java_routines = java_routines_partitions,
                                  index = indexes, mc.cores = amount_workers ) 
        
  print("Finished parallel")
  print(Sys.time())
  #Now merge the different partitions into one data table
  sequences_marked = combine_sequences_marked(sequences_marked_split)
    
  return(list(sequences_marked, sequences_marked_split))
}


#Merge the "File" and "Routine" columns, then remove all the ones not matching the condition for them
merge_filter_alma_dataset_parallel <- function(partition_java_classes, partition_java_routines, index)
{
  
  print("Merging java classes and routines")
  java_classes_routines_df = data.frame(sample=(paste(partition_java_classes, "::", partition_java_routines, sep="")))
  #Firstly, need to assign the sequence ID. Then, need to filter. 
  print("Finding sequence ID")
  sequenceIDs = find_sequence_ids_given_messages(java_classes_routines_df$sample, index)
  java_classes_routines_df$SequenceID = sequenceIDs
  
  
  #now assign one sequence ID to every message of the data frame,
  dataFrameReturn = data.frame(data.frame(matrix(ncol = 2, nrow = length(java_classes_routines_df$SequenceID))))
  colnames(dataFrameReturn) = c("sample", "SequenceID")
  
  if(length(dataFrameReturn$SequenceID) > 0)
  {
    print(paste("Processed data frame with size", length(dataFrameReturn)))
  }
  else
  {
    print(paste("The merged java classes and routines' data frame has size 0 for partition with index", index))
  }
  return(dataFrameReturn)
}


get_amount_rows_from_data_structure <- function(java_classes)
{
  #Now, the dataset only consists of one file

  if(length(java_classes) == 0)
  {
    stop(paste("Loaded java classes have size 0. Did you specify the input_dataset variable correctly?"))
  }
  
  print(paste("Loaded input classes and methods have size", length(java_classes)))
  amount_rows = length(java_classes)
  
  return(amount_rows)
}



load_partitions_data_structure <- function(start_end_indexes_dataset, data_structure)
{
  #fine, we got our data frame loaded. We now need to split it into as many partitions as indexes
  print(paste("generating ", length(start_end_indexes_dataset), "partitions from the dataset"))
  
  partitions_data_loaded = find_partitions_for_sequences_given_start_end(data_structure, start_end_indexes_dataset)
  #keep it like this for now
  
  for(i in 1:length(partitions_data_loaded))
  {
    print(paste("Partition", i, " has size ", length(partitions_data_loaded[[i]])))
  }
  
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



