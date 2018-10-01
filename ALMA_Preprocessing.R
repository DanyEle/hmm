MESSAGES_PER_SEQUENCE = 95

########################### 
#####MAIN FUNCTION#########
###########################
#input_dataset = [SOURCE_PATH, EVENT_PATH, RUNTIME_PATH]
load_marked_sequences_alma <- function(amount_workers, input_dataset)
{
  input_dataset = "/media/daniele/66D6DE91D6DE60BB/Users/Daniele/Ubuntu_Transfer/ALMA_Logs/All_Logs/"
  source("/home/daniele/HMM_ALMA/init_michele.R")
  
  #now loop through all the datasets in the input folder specified
  all_datasets = list.files(path=input_dataset)
  
  #Array of all unique developers
  print(paste("Getting all datasets' names and sizes in the folder ", input_dataset))
  i = 1
  
  java_classes = c()
  java_routines = c()
  java_timestamps = c()
  for (dataset in all_datasets)
  {
    dataset_name = paste(input_dataset,"", dataset, sep="")
    print(dataset_name)
    classes_routines_timestamps = get_java_class_routines_timestamp_from_input_dataset(dataset_name, "ERROR")
    java_classes[i] = classes_routines_timestamps[1]
    java_routines[i] = classes_routines_timestamps[2]
    java_timestamps[i] = classes_routines_timestamps[3]
    clear()
    i = i + 1
    
    print(paste("We are at ", i ," / ", length(all_datasets)))
  }
  
  #unwrap and get the unique java classes
  java_classes_unique = unique(unlist(java_classes))
  
  #same thing for the routines
  java_routines_unique = unique(unlist(java_routines))

  #concatenate the results found
  java_classes = c(java_classes_error, java_classes_warning) #java_classes_info
  java_routines = c(java_routines_error, java_routines_warning) #java_routines_info
  java_timestamps = c(java_timestamps_error, java_timestamps_warning) #java_timestamps_info

  #now sort the java_classes and java_routines according to the order of the java_timestamps
  numbers_order = order(java_classes)
  java_classes = java_classes[numbers_order]
  java_routines = java_routines[numbers_order]
  
  amount_rows = get_amount_rows_from_data_structure(java_classes) 
  #find start and end indexes for the input dataset based on the amount of workers passed
  
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
  #merge_filter_alma_dataset_parallel(partition_java_classes, partition_java_routines, index)
  
  print(paste("Reading datasets as table in parallel with", amount_workers, " workers"))
  
  sequences_marked_split = mcmapply(merge_filter_alma_dataset_parallel, partition_java_classes = java_classes_partitions, partition_java_routines = java_routines_partitions,
                                  index = indexes, mc.cores = amount_workers ) 
        
  print("Finished parallel")
  print(Sys.time())
  #Now merge the different partitions into one data table

  sequences_marked = combine_sequences_marked(sequences_marked_split)
    
  return(list(sequences_marked, sequences_marked_split))
}


#filter can be one of the following three types:
- "INFO"
- "WARNING"
- "ERROR"
get_java_class_routines_timestamp_from_input_dataset <- function(input_dataset, filter)
{
   search("forType", filter)
   parse(input_dataset)
   where("container", "CONTROL/ACC/javaContainer")
   select("component")
   array = where("component", "Array")
   java_classes = select("file")   #_info
   java_routines = select("routine")  #_info
   java_timestamps = select("timestamp") #_info
   
   return(list(java_classes, java_routines, java_timestamps))
}


#Merge the "File" and "Routine" columns, then remove all the ones not matching the condition for them
merge_filter_alma_dataset_parallel <- function(partition_java_classes, partition_java_routines, index)
{
  
  #print("Merging java classes and routines")
  java_classes_routines_df = data.frame(sample=(paste(partition_java_classes, "::", partition_java_routines, sep="")))
  #Firstly, need to assign the sequence ID. Then, need to filter. 
  #print("Finding sequence ID")
  sequenceIDs = find_sequence_ids_given_messages(java_classes_routines_df$sample, index)
  java_classes_routines_df$SequenceID = sequenceIDs
  
  
  #now assign one sequence ID to every message of the data frame,
  #dataFrameReturn = data.frame(data.frame(matrix(ncol = 2, nrow = length(java_classes_routines_df$SequenceID))))
  #colnames(dataFrameReturn) = c("sample", "SequenceID")
  
  if(length(java_classes_routines_df$SequenceID) > 0)
  {
    print(paste("Processed data frame with size", length(java_classes_routines_df$SequenceID)))
  }
  else
  {
    print(paste("The merged java classes and routines' data frame has size 0 for partition with index", index))
  }
  return(java_classes_routines_df)
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



