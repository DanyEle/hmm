

build_matrix <- function()
{
  
  #all the ones <= 0.10% --> 0
  trans_mat = matrix(c (0.382050449 , 0.26350547 , 0.17473206 , 0.01071777 , 0.01266645 , 0.1313196925 , 0.0192703259 , 0.002273465 , 0.0034643282,
                                0.248689575 , 0.45884294 , 0.14783537 , 0.01446321 , 0.03610949 , 0.0528052804 , 0.0343622598 , 0.005047564 , 0.0018443021,
                                0.128079163 , 0.10204225 , 0.23587620 , 0.12828970 , 0.24815777 , 0.1256228505 , 0.0025264931 , 0.027510703 , 0.0018948698,
                                0.013898081 , 0.02934039 , 0.45797485 , 0.09684536 , 0.39642621 , 0.0008824178 , 0.0000000000 , 0.004632694 , 0.0000000000,
                                0.022444946 , 0.06549972 , 0.46767363 , 0.28049125 , 0.09754376 , 0.0296442687 , 0.0001411632 , 0.036420102 , 0.0001411632,
                                0.228990552 , 0.15688712 , 0.48557931 , 0.00198906 , 0.06190950 , 0.0611636000 , 0.0012431626 , 0.001243163 , 0.0009945301,
                                0.222720478 , 0.59790732 , 0.03587444 , 0.00000000 , 0.00000000 , 0.0029895366 , 0.1405082212 , 0.000000000 , 0.0000000000,
                                0.003778338 , 0.05793451 , 0.45465995 , 0.02770781 , 0.39546600 , 0.0037783375 , 0.0000000000 , 0.056675063 , 0.0000000000,
                                0.297619047 , 0.16666667 , 0.28571429 , 0.01190476 , 0.09523810 , 0.1190476191 , 0.0119047619 , 0.000000000 , 0.0119047619
  ), nrow = 9, ncol = 9, byrow = TRUE)
  
  #[X, Y] = From state X to state Y
  #X = 5
  #Y = 3
  #From state X to state Y
  #This means, X = Row, Y = Column
  
  trans_mat_edited = matrix(c (0.382050449 , 0.26350547 , 0.17473206 , 0 , 0 , 0.1313196925 , 0 , 0 , 0,
                        0.248689575 , 0.45884294 , 0.14783537 , 0 , 0 , 0.0528052804 , 0 , 0 , 0,
                        0.128079163 , 0.10204225 , 0.23587620 , 0.12828970 , 0.24815777 , 0.1256228505 , 0 , 0 , 0,
                        0 , 0 , 0.45797485 , 0 , 0.39642621 , 0 , 0 , 0 , 0,
                        0 , 0 , 0.46767363 , 0.28049125 , 0 , 0 , 0 , 0 , 0,
                        0.228990552 , 0.15688712 , 0.48557931 , 0 , 0 , 0 , 0 , 0 , 0,
                        0.222720478 , 0.59790732 , 0 , 0 , 0 , 0 , 0.1405082212 , 0 , 0,
                        0 , 0 , 0.45465995 , 0 , 0.39546600 , 0 , 0 , 0 , 0,
                        0.297619047 , 0.16666667 , 0.28571429 , 0 , 0 , 0.1190476191 , 0 , 0 ,0
  ), nrow = 9, ncol = 9, byrow = TRUE)
  
  transposed_mat = t(trans_mat_edited)
  
  symbols_states = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9")
  
  colnames(trans_mat_edited) = symbols_states
  #rownames(trans_mat_edited) = symbols_states
  
  
  library(diagram)
  
  plotmat(transposed_mat, cex=0.8, box.cex=0.8, box.size=0.057, 
          main="Transition Matrix for the AIHMM generated with k = 2 from a sample of Damevski's Dataset 
          (Only showing transition with probability >= 10%)", cex.main = 1.7)
  
  
  
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




#all_unique_messages: Array containing all the unique messages that can possibly occur in the dataset
#lists_devs_messages: List of lists, containing:
#[[1]] = all developers
#[[2]] = unique messages corresponding to the developer
find_extremely_rare_messages <- function(list_devs_messages, all_unique_messages)
{
  THRESHOLD_RARE_MSG = 0.03
  
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



#Create a dataframe with sequences and their IDs
mark_sequences_of_actions_with_ID<-function(sample){
  sampleObs<-data.frame(sample)
  prova<-grep("Start", sampleObs$sample, invert=F)
  t<-0
  temp<-c(0*1:nrow(sampleObs))
  for(i in 1:(nrow(sampleObs))){
    if(i %in% prova){t<-t+1
    temp[i]<-t
    }else{t<-t
    temp[i]<-t}	
  }
  sampleObs$SequenceID<-temp
  return(sampleObs)	
}




