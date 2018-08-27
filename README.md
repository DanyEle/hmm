# Automatic and Interactive Hidden Markov Model creation in R
## Authors: Daniele Gadler, Barbara Russo, Andrea Janes
## Free University of Bolzano-Bozen 2016-2018


The present repository contains R scripts to generate HMMs interactively (IIHMM) and automatically (AIHMM).  

## Required Packages:
- "hmm.discnp"
- "HMM"
- "stringr"
- "parallel"
- "purrr"
- "data.table
- "naturalsort"

Make sure you install these packages in your R environment before running the scripts

## AIHMM_Generation.R
Given a certain range of k (the amount of top interesting sequences to consider), this script automatically generates Hidden Markov Models by constraining all symbols not previously constrained on previous states onto the newly added state. It does so by considering all symbols in the generated top k interesting sequences. (for example, setting k = 5, the script will generate AIHMMs with 1,2,3,4,5 interesting sequences).

1. Navigate to a folder where you'll be hosting the R project. 
EX: mkdir HMM, then cd HMM

Run: generate_hmm_workers(amount_workers, output_to_file, dataset_index, input_dataset)
Where the input parameters have the following meaning:
amount_workers = INT. parallelism degree (i.e: amount of cores to be used)
output_to_file = BOOLEAN. TRUE - Output to a file
                          FALSE - Output to the console
dataset_index = INT. 1 = Michael Mairegger's dataset of mobile interactions
                     2 = Damevski et al.'s dataset for visual studio IDE interactions
                     https://github.com/abb-iss/DeveloperInteractionLogs/tree/master
                     3 = ALMA dataset for radio-telescope Java classes' interactions
input_dataset = List/String Contains the dataset (s) to use in the analysis, or the folder where datasets are contained, as in the case of Damevski et al's dataset. 

###1 - Michael Mairegger's dataset.
Example Invocation: 
source("AIHMM_Generation.R")
generate_hmm_workers(N, FALSE, 1, "WindowsPhone_One_Month.csv")

###2 - Damevski et al.'s ABB Dev Interaction dataset:
Example invocation:
source("AIHMM_Generation.R")
generate_hmm_workers(N, TRUE, 2, "Datasets_Damevski")

Where "Datasets_Damevski" is the folder where all the datasets are taken from and will be automatically during the initialization phase. 

###3 - ALMA dataset: 
Example invocation: 
generate_hmm_workers(N, FALSE, 3, c("LogSourceInformation.txt", "LogEventSet.txt", "LogRuntimeInformation.txt"))



	   
			   
# IIHMM_generation.R
This script allows for the creation of Hidden Markov Models iteratively, namely adding a state per iteration and constraining a set of hand-picked symbols onto the newly added state.

## Steps
1. Run all code up until: show_top_k_interesting_sequences(intersection, k)
2. Assign a set of symbols to the "toMoveSymbols" vector from the set of interesting sequencees, ensuring these hand-picked symbols haven't been constrained on an existing state already. 
3. Run the remaining code until the end of the iterative_phase method
--> Go back to (1)



