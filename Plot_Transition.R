

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



getThetaProbableSequencesParallel<-function(HMMTrained, theta){
  #Theta-Probable sequences as global variables
  symbols<-as.vector(HMMTrained$Symbols)
  column_names = c(1:length(symbols))
  thetaProbableProbabilities <<- list()
  #thetaProbableSequences <<- list()
  #thetaProbableSequences <<- data.frame(matrix(ncol = length(symbols)))
  #result will need to be put back into a list though!
  
  #initialize the index of the two global list variables ThetaProbableProbabilities, ThetaProbableSequences
  #populate the global list variables ThetaProbableProbabilities, ThetaProbableSequences.
  thetaProbableSequences = generateProbableSequencesParallel(HMMTrained, theta)
  
  #put them together into a single list.
  #thetaProbableValues = list(thetaProbableSequences, thetaProbableProbabilities)
  return(thetaProbableProbabilities)
}   


create_HMMs <- function(M, HMMTrained)
{
  output_list = list()
  for(i in 1:M)
  {
    output_list[[i]] = HMMTrained
  }
  return(output_list)
}



#Daniele has fixed the data format (as.vector) for symbols 10.04.2017, verified:CORRECT
generateProbableSequencesParallel<-function(HMMTrained, theta){
  symbols<-as.vector(HMMTrained$Symbols)
  M <- length(symbols)
  print(M)
  
  n = 10
  partitions = identify_partitions_symbols(n, symbols)
  
  HMMsTrained = create_HMMs(length(partitions), HMMTrained)
  
  
  all_theta_probable_sequences = mcmapply(process_symbols_of_partition, HMMTrained = HMMsTrained, theta=theta, symbols_partition = partitions, mc.cores = 8) 
  
  #somehow, also need to combine the result
  
  return(all_theta_probable_sequences)
}

#TEST: process_symbols_of_partition(HMMTrained, theta, partition, 1)

process_symbols_of_partition <- function(HMMTrained,  theta, symbols_partition)
{
  symbols_partition = array(unlist(symbols_partition))
  
  #print(theta)
  #print(HMMTrained)
  #print(symbols_partition)
  
  theta_probable_sequences_partition = list()
  
  for (j in 1:length(symbols_partition)) 
  {
    #Every worker gets a chuck of the symbols.
    print(j)
    # this is the first step from the default "start" state. We create a sequence of length two just to compute the probability of the first time step in a sequence. Such probability does not change by changing the second element of the sequence (symbols[1] or sumbols[2] etc)  	
    init <- symbols_partition[j]
    
    #browser()
    sequence <- c(init, symbols_partition[1])
    
    
    forwardProb = forward(HMMTrained, sequence)
    # this is the inductive case
    theta_probable_sequences_one_symbol = append(theta_probable_sequences_partition, generateProbableSequences_sequential_no_global(HMMTrained, init))
  }
  
  return(theta_probable_sequences_partition)
}

#Daniele has fixed the data format (as.vector) for symbols 10.04.2017, verified:CORRECT
generateProbableSequences_sequential_no_global<-function(HMMTrained, theta){
  all_theta_probable_sequences = list()
  symbols<-as.vector(HMMTrained$Symbols)
  M <- length(symbols)
  print(M)
  for (j in 1:M) {
    print(j)
    # this is the first step from the default "start" state. We create a sequence of length two just to compute the probability of the first time step in a sequence. Such probability does not change by changing the second element of the sequence (symbols[1] or sumbols[2] etc)  	
    init <- symbols[j]
    print(init)
    sequence <- c(init, symbols[1])
    #print(sequence)
    Sys.sleep(2)
    forwardProb = forward(HMMTrained, sequence)
    # this is the inductive case
    all_theta_probable_sequences = append(all_theta_probable_sequences, generateHMMSequencesIteration(HMMTrained, init, forwardProb, theta, symbols))
  }
  
  return(all_theta_probable_sequences)
  
  #print(result)
  
}



#Input: M: size of the data structure to partition
#       n: amount of workers that will process the work in parallel
identify_partitions_symbols <- function(n, symbols)
{
  M = length(symbols)
  partitions =  list() 
  
  delta = round(M / n, digits=0)
  
  start = 1
  end = delta
  continue = TRUE
  i = 0
  while(i < n && continue == TRUE)
  {
    end = (delta * (i + 1)) + 1
    
    start = (delta * i)  + 2
    if(i == 0)
    {
      start = 1
    }
    if(i == n - 1)
    {
      end = M
      continue = FALSE
    }
    
    i = i + 1
    
    print(i)
    print(paste(start, end))
    partitions[[i]] = as.list(symbols[start:end])
    
  }
  
  return(partitions)
  
}


