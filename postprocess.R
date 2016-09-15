#' ---
#' title: "Allostatic Load Post-Processing, Refactored"
#' author: "Vivek Prakash, Shane M. Gerry, Alex F. Bokov, and Darpan Patel"
#' date: "09/14/2016"
#' ---
#+ echo=TRUE,cache=TRUE

#' # Load libraries
#+ message=FALSE
require("readr")
require("dplyr")
require("ggplot2")
source("helpers.R")

#' # Set session variables
#' 
#' The name of the raw data file
datafile <- 'allo_160913_update.csv';
#' Minimum number of non-missing values to remain in dataset
minnm <- 50;
#' ...except for the following columns which are always kept.
vars_keep <- c('v034_Dcsd_pr_SS');
#' These variables should always be factors, even if they look like numbers.
vars_factor <- c('patient_num');
#' grep targets for column names that should not be part of analysis
patterns_nonanalytic <- paste0(c(
  # can add more values and/or new lines as needed
  # just make sure there are no leading or trailing
  # commas
  '_date$','_Dschrg_Dspstn$','_unit$'
  ,collapse='|'));

#' # Read in Data and Review It
#' df0 will be where the raw data is read in
df0 <- read_csv(datafile, na = c("", "(null)")
                ,locale = locale(date_format="%m/%d/%y"));

#' Remove info columns, retaining which ones they are in the `vars_noninfo` variable.
df1 <- df0[ , vars_noninfo <- grep("_info$", names(df0),inv=T,val=T)];
#' We kept: 
#+ echo=FALSE
vars_noninfo;

#' Now, how many non-missing values does each row have? Our convention will be to
#' save lists of column names in vectors prefixed by `vars_` and lists of column
#' information in vectors prefixed by `meta_`.
meta_nonmissing <- sapply(df1,function(xx) sum(!is.na(xx)));
cbind(`Number Non Missing`=meta_nonmissing);

#' We don't have a one-size fits all cutoff-- this is EMR data, and missing 
#' values are the norm. We need to eyeball the data and see how often 'typical' 
#' variables are non-missing for _this_ dataset. We might want to also keep some
#' variables even though they are frequently missing because they might be of
#' special interest. For example the variable containing `Dcsd_pr_SS` in its
#' name means "deceased according to Social Security records". Even though this
#' does not happen to many patients here (fortunately) we do want to know about
#' it if it does.
#' 
#' So, currently the minimum number of non-missing values cutoff (`minnm`) is 
#' `r minnm` and the "always keep" columns (`vars_keep`) are `r vars_keep`. At any time,
#' we can go back up to the `Set session variables` section, change these values
#' and re-run this script.
#' 
#' We keep just the variables that have more non-missing values than the threshold
#' set by `minnm`.
vars_enoughvals <- sort(unique(c(names(meta_nonmissing)[meta_nonmissing>minnm],keep)));
df1 <- df1[,vars_enoughvals];
#' Guess which columns are numeric. `vs()` is a function defined in the 
#' `helpers.R` file.
vars_numeric <- setdiff(na.exclude(vs(df1,'z')),vars_factor);
#' Sanity check... from what we know about the variables, does this look right?
# Numeric Variables
vars_numeric;
#' See which columns were guessed to be non-numeric
setdiff(names(df1),vars_numeric);

#' If all looks good, convert numeric columns to numeric 
#' (in this case the `for` loop is faster)
for(.ii in vars_numeric) df1[[.ii]] <- as.numeric(df1[[.ii]]);
#' ...and characters to factors, again saving the selected column names first
vars_2factor <- unique(c(vs(df1,'c'),vars_factor));
df1[,vars_2factor] <- sapply(df1[,vars_2factor],factor,simplify = F);

#' Hardcoding some new columns
#' 
#' DF_TODO: Put this into DataFinisher, many projects will need this
df1$age_at_visit_years <- df1$age_at_visit_days / 365  # Convert to years.
#' TODO: Oops, forgot to pull vitals! Will need to re-run. :-()
#df1$v039_Wght_lbs_num  <- df1$v039_Wght_oz_num * 0.0625 # Convert to pounds.

#' Sanity-checking units 
#' 
#' Removing non-analytic columns
#' 
#' Setting diagnoses to T/F
#' 
#' 
#' # Questions to think about:
#' 
#' * In what cases to parse variables out of JSON vs. pre-separate them in query?
#' * How to map `Prvdr_Spclt`, `Encntr_Tp`, and `Fncl_Cls`?
#' * Perhaps DataFinisher should also include `DEATH_DATE`?
#' * How to plot this data?
#' 
