# Automatic and Interactive Hidden Markov Model creation in R
## Authors: Daniele Gadler, Barbara Russo, Andrea Janes
## Free University of Bolzano-Bozen 2016-2018


The present repository contains R scripts to generate HMMs interactively (IIHMM) and automatically (AIHMM).  

## Required Packages:
- "hmm.discnp"
- "hmm"
- "stringr"
- "parallel"
- "purrr"
- "data.table

Make sure you install these packages in your R environment before running the scripts

Also set the following global variables:

SEPARATOR: The separator used to separate different columns in the dataset
MARK_SEQUENCE_IDS:   TRUE if there is no sequence ID associated to every action in the dataset. It will be added automatically by the mark_sequence_ids function. You may hence want to configure this function as well, to identify how a sequence begins
                    FALSE is there is already a sequence ID associated to every single action in the file
LOAD_CUSTOM_SEQUENCES: TRUE if you want to specify your own dataset(s) with custom pre-processing operations. 
                       FALSE if you want to use one standard dataset on that the script runs by default. 


## AIHMM_Generation.R
Given a certain range of k (the amount of interesting sequences to consider), this script automatically generates Hidden Markov Models by constraining all symbols not previously constrained on previous states onto the newly added state. It does so by considering all symbols in the generated top k interesting sequences. (for example, setting k = 5, the script will generate AIHMMs with 1,2,3,4,5 interesting sequences)..

## Steps for Damevski dataset processing (only initialization phase)
1. Navigate to a folder where you'll be hosting the R project. 
EX: cd HMM or mkdir HMM
2. Place the datasets desired in the "Datasets_Damevski_Small" folder or set the variable "DATA_PATH" in Damevski_Preprocessing.R
to point to a different location. All datasets containing the DATA_PATH path will be automatically loaded by the initialization phase. 
3. Put the "MessageList.csv" file containing all existing unique messages in the "Info_Dataset" folder in the project root folder or
change the location of the unique messages file by altering the variable "INFO_PATH" in Damevski_Preprocessing.R to point to
a different location
4. Run R in the project location. 
R
5. Load the content of the three files needed :
source("Damevski_Preprocessing.R")
source("AIHMM_Generation.R")
source("Common_Functions.R")
6. (Optional) Set an output file 
sink("file_name.txt")
7.  Run the following command to let the initialization phase begin, passing the amount of workers to be used as a parameter of the function call.  \footnote{If wanting to run the script on a local machine, further details can be found in the Readme file at \url{https://github.com/DanyEle/hmm/blob/master/README.md}}
run_experiment_workers(N)
	   
					   
## IIHMM_generation.R
This script allows for the creation of Hidden Markov Models iteratively, namely adding a state per iteration and constraining a set of hand-picked symbols onto the newly added state.

## Steps
1. Load the libraries of the "hmm.discnp" and "hmm" packages after installing them through following commands
library("hmm.discnp")
library("hmm")
2. Load all the scripts underneath "k <- 5" into your R environment.
3. Set the path where the dataset is stored into the variable "pathToData".
4. If sequences are not marked with IDs, use the method "mark_sequences_of_actions_with_ID" to mark them with IDs in the procedure "initializeHMM".
6. Set k, the amount of interesting sequences that should be considered in the present iteration.
7. Run line-by-line (CTRL + R / Command + R) all commands in the procedure "loopOverSequenceSet" until line 129 with comment	#GO BACK UP! is reached. Symbols in the array "toMoveSymbols" at line 89 ought to be picked manually out of the set of proposed interesting sequences.
8.1 If "continue" has value FALSE, then the process terminated, as log-likelihood has either not increased from the previous iteration or the log-likelihood of constrained HMM is less than the log-likelihood of the unconstrained HMM with same amount of states.
8.2. If "continue" has value TRUE, then go back to line 55 with comment ITERATION_BEGIN and run all commands underneath until "GO BACK UP!.Repeat this until State 8.1 is reached.




