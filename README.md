# Automatic and Interactive Hidden Markov Model creation in R
## Authors: Daniele Gadler, Barbara Russo, Andrea Janes
## Free University of Bolzano-Bozen 2016-2017


The present repository contains R scripts to generate HMMs interactively (IIHMM) and automatically (AIHMM).  

## Required Packages:
- "hmm.discnp"
- "hmm"
Make sure you install these packages in your R environment before running the scripts


#IIHMM_generation.R
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
8.1 If "stop" has value FALSE, then the process terminated, as log-likelihood has either not increased from the previous iteration or the log-likelihood of constrained HMM is less than the log-likelihood of the unconstrained HMM with same amount of states.
8.2. If "stop" has value TRUE, then go back to line 55 with comment ITERATION_BEGIN and run all commands underneath until "GO BACK UP!.Repeat this until State 8.1 is reached.

## AIHMM_Generation.R
Given a certain range of k (the amount of interesting sequences to consider), this script automatically generates Hidden Markov Models by constraining all symbols not previously constrained on previous states onto the newly added state. It does so by considering all symbols in the generated top k interesting sequences.

## Steps:
1. Load the libraries of the "hmm.discnp" and "hmm" packages after installing them through following commands
library("hmm.discnp")
library("hmm")
2. Load all scripts underneath the while loop ending at line 18 into the environment
3. Set the path where the dataset is stored into the variable "pathToData"
4. Set the output path and files name for the generated HMMs at line 12
5. Set the k range at line 9 and 10
5. Run the while loop line-by-line starting from line 10









