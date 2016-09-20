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
datafile <- 'allo_04_allo_megaquery_v4.csv';
#' Minimum number of non-missing values to remain in dataset
minnm <- 50;
#' ...except for the following columns which are always kept.
vars_keep <- c('v028_Dcsd_pr_SS');
#' Patient ID. For i2b2/DB/DF always going to be `patient_num`
vars_patid <- 'patient_num';
#' These variables should always be factors, even if they look like numbers.
vars_factor <- c(vars_patid);
#' Grep targets for columns that are actually blobs of JSON
patterns_removefromvals <- paste0(c(
  'GENERIC_UTHSCSA_FINCLASS_','GENERIC_UTHSCSA_ENC_TYPE_','"'
  ),collapse='|');
patterns_JSON <- paste0(c(
  '_Yrs_Tbc_Usg','_Pcks_Pr_D'
),collapse='|');
#' Grep targets for column names that should not be part of analysis.
#' (after already eliminating all the _info columns)
patterns_nonanalytic <- paste0(c(
  # can add more values and/or new lines as needed
  # just make sure there are no leading or trailing
  # commas. Note that for the prostate cancer 
  # sex_cd is non-analytic, but for most datasets
  # this will not be the case
  patterns_JSON,'_info$',
  '_date$','_Dschrg_Dspstn$','_unit$','^sex_cd$'
  ),collapse='|');
#' As above, but these are grep targets for column names some or all
#' of which we expect to be collected specifically during in-person
#' visits.
patterns_realvisit <- paste0(c(
  'stlc_Prsr_num$',
  '_Pls_num$','_Wght_oz_num$',
  '_Bd_Ms_Indx_num$','_Hght_cm_num$',
  '_Tmprtr_F_num$'
  ),collapse='|');
#' Variables to later convert to 2-level factors
patterns_twolvl <- paste0(c(
  '_Mlgnt_prst','_Esphgl_rflx','_Dcsd_pr_SS'
  ),collapse='|');

#' # Read in Data and Review It
#' df0 will be where the raw data is read in
df0 <- read_csv(datafile, na = c("", "(null)")
                ,locale = locale(date_format="%m/%d/%y"));

#' Convert JSON values to lists squished into a `data.frame` column.
vars_JSON <- grep(patterns_JSON,names(df0),val=T);
df0[,vars_JSON] <- sapply(df0[,vars_JSON],jsonParse,simplify = F);
#' At the moment it happens that both of the JSON variables also have numeric 
#' values that need to be extracted. This will not always be the case, and so a 
#' separate var vector might be needed.
vars_JSON2num <- vars_JSON;
for(ii in vars_JSON2num) df0[[paste0(ii,'_num')]] <- dfListExtract(df0[[ii]],'nv');

#' Remove info columns, retaining which ones they are in the `vars_noninfo` variable.
df1 <- df0[ , vars_noninfo <- grep("_info$", names(df0),inv=T,val=T)];
#' We kept: 
#+ echo=FALSE
vars_noninfo;

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
#' Collapse certain variables to two levels
vars_twolvl <- grep(patterns_twolvl,names(df1),val=T);
df1[,vars_twolvl] <- data.frame(lapply(df1[,vars_twolvl],function(xx) factor(is.na(xx))));

#' Hardcoding some new columns
#' 
#' DF_TODO: Put this into DataFinisher, many projects will need this
df1$age_at_visit_years <- df1$age_at_visit_days / 365  # Convert to years.
#' TODO: Oops, forgot to pull vitals! Will need to re-run. :-()
#df1$v039_Wght_lbs_num  <- df1$v039_Wght_oz_num * 0.0625 # Convert to pounds.

#' Let's mark the in-person visits. New function in `helpers.R` for doing this 
#' concisely. Additional `ids` and `indicators` columns added to `df1`.
#' For larger datasets might want to capture those as a separate two-column
#' `data.frame` by setting the optional `returnDF` argument to `FALSE` and
#' then inserting the columns of that `data.frame` into `df1` in a separate
#' command. But this is a reasonably sized dataset.
df1 <- findEvents(df1,patterns_realvisit);
#' Create a unique patient_num/visit-set combo `pn_vis`
df1$pn_vis <- paste(df1[[vars_patid]],df1$ids,sep=':');
#' Identify the analytic variables
vars_analytic <- grep(patterns_nonanalytic,names(df1),inv=T,val=T);
#' Create an empty `data.frame` with an identical column layout to `df1`
#' Removing non-analytic columns
df2 <- subset(df1,FALSE)[,vars_analytic,drop=F];
#' Iterate over `pn_vis` to create the collapsed `data.frame` populated with
#' the last non-missing value of every column in the dataset. 
#' This part takes a looong time:
for(ii in 1:length(meta_unqpnvis <- unique(df1$pn_vis))){
  df2[ii,] <- data.frame(lapply(df1[df1$pn_vis==meta_unqpnvis[ii],vars_analytic],lastNonMissing));
}

#' Sanity-checking units 
#' Checking to see if any of the units of measurement will need converting later on.
# Not yet... let's get the rest of this working and then...
#summary(df1[ , grepl("unit", names(df1))])
#' 

#' Now, how many non-missing values does each row have? Our convention will be to
#' save lists of column names in vectors prefixed by `vars_` and lists of column
#' information in vectors prefixed by `meta_`.
meta_nonmissing <- sapply(df2,function(xx) sum(!is.na(xx)));
cbind(`Number Non Missing`=meta_nonmissing);
#' We keep just the variables that have more non-missing values than the threshold
#' set by `minnm`.
vars_enoughvals <- sort(unique(c(names(meta_nonmissing)[meta_nonmissing>minnm],keep)));
#df1 <- df1[,vars_enoughvals];

#' Any crazy number of levels?
meta_flevels <- sapply(df3[, vs(df3, "f")], function(xx) length(levels(xx)));
cbind(sort(meta_flevels))
#' 
#' 
#' # Questions to think about:
#' 
#' * In what cases to parse variables out of JSON vs. pre-separate them in query?
#' * How to map `Prvdr_Spclt`, `Encntr_Tp`, and `Fncl_Cls`?
#' * GENERIC_KUMC_PACK_PER_DAY: NVAL_NUM is the only informative field, TODO: write new rule
#' * Ditto for GENERIC_KUMC_TOBACCO_USED_YEARS, but why is units still 'Packs'?
#' * Perhaps DataFinisher should also include `DEATH_DATE`?
#' * How to plot this data?
#' 
