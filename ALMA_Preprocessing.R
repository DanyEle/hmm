#Initial "global" variables
DATA_PATH = "Datasets_ALMA" #NB: There should be no other file in this folder other than the datasets to load
SEPARATOR = ","
#Print an update every X messages processed in the function "mark_debug_sessions_with_ID"
FREQUENCY_PRINT = 5000
#Amount of cores to be used
AMOUNT_WORKERS <<- 1
#Used to keep track of the time spent in the sequential and (potentially) parallel parts of the program


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

}


load_filter_alma_dataset_parallel <- function(dataset_name, index)
{
  #Fine, we got our dataset_name that is a relative or absolute location of the dataset being considered
  
  #Cast the black cells ("") to NA
  linesRead = readLines(dataset_name, skipNul=FALSE)  
  
  linesReadNA = sapply(linesRead, insertNAInLine)
  
  read.table(textConnection(linesReadNA))


}

#Input: a line from the input text file
#Output: a line with an "NA" in it if it contains only two elements (i.e: third column is empty)
insertNAInLine <- function(line)
{
  
  amount_matches = str_count(line, "\\S+")
  
  #Only two matches, then we need to insert an NA into the string
  if(amount_matches == 2)
  {
    line = paste(line, "     ", "NA" , sep="")
  }
  
  return(line)
}







