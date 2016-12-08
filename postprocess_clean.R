#' ---
#' title: "Allostatic Load Post-Processing, Refactored"
#' author: "Vivek Prakash, Shane M. Gerry, Alex F. Bokov, and Darpan Patel"
#' date: "09/14/2016"
#' ---
#+ echo=TRUE,cache=TRUE

#' # Load libraries
#+ message=FALSE
require("readr");
require('rjson');
require("dplyr");
require('data.table');
require("ggplot2");
source("helpers.R");

#' # Set project variables
#' 
#' # Load lookup tables
#' 
#' # Load the raw data, and make the first-stage `data.frame` `df0` from it
#' 
#' # Find useful column subsets
#' 
#' # Do transformations upon certain sets of columns
#' 
#' # Create second-stage `data.frame` `df1` from `df0`
#' 
#' # Optionally do machine-assisted review of VFs and units
#' 
#' At this step, labs occurring between office visits are collapsed into the next
#' chronological office visit for that patient (or, as a special case, into the 
#' visit when the first PC diagnosis was entered)
#' 
#' # Create `data.frame` `df2` with variables for time-to-event analysis
#' 
#' # Clean `data.frame` `df3` with no pre-existing patients
#' 
#' # Create the training data, if not already existant.
#' 
#' # Calculate case weights for control group
#' 
#' # Create `data.frame` `df4`, our training set.
#' 
#' # Approach 1: Complete case analysis
#' 
#' ## Create `data.frame` `df4cc`, our complete-case training set.
#' 
#' ## Fit basic additive Cox PH model.
#' 
#' ## Use stepAIC for term selection
#' 
#' ## Model result tables and plot of survival curves split by linear predictor
#' 
#' # Approach 2: Score
#' 
#' # Approach 3: Imputation
