

load_marked_sequences_mm <- function(input_dataset)
{
  #Output needs to be a dataframe with a $sample and a $SequenceID columnn
  #Sample has two columns: sample and ID. It contains all symbols including "Start" and "End"
  print(paste("Loading Mairegger's dataset:", input_dataset))
  loaded_dataset<- read.csv(input_dataset, sep=";", header=T)
  
  print("Removing irrelevant symbols ")
  #pre-processing symbols: remove symbols that are not relevant. Output: sequences (with ID and cleaned from irrelevant symbols),symbols, theta, HMMTrained
  SymbolsToRemove = c("Navigate Back to HubPage", "Navigate to HubPage", "Start", "End")
  
  filtered_marked_dataset<-loaded_dataset
  for(x in SymbolsToRemove)
  {
    temp <- filtered_marked_dataset[-which(filtered_marked_dataset$sample==x),]
    filtered_marked_dataset <- temp
  }
  #filtered_marked_dataset just has $sample and $SequenceID as columns
  #just just need to create a list with 1 entry, which contains the dataset 
  
  #Now convert the data frame we had into a list
  #list_sequences_marked = matrix(list(sample=filtered_marked_dataset$sample, SequenceID=filtered_marked_dataset$SequenceID), nrow=2, ncol=1)
  matrix_sequences_marked = matrix(filtered_marked_dataset  , nrow=2, ncol=1)
  rownames(matrix_sequences_marked) = c("sample", "SequenceID")
  
  return(list(filtered_marked_dataset, matrix_sequences_marked))
  
}