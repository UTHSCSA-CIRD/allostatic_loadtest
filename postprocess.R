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
require("ggplot2");
source("helpers.R");

#' Lookup tables
mapfc <- read_csv('finclass_codes.csv', na = c("", "(null)"));
mapen <- read_csv('enctypes.csv', na = c("", "(null)"));
mapsp <- read_csv('specialty_codes.csv', na = c("", "(null)"));

#' # Set session variables
#' 
#' The name of the raw data file
datafile <- 'allo_04_allo_megaquery_v4.csv';
#' Minimum number of non-missing values to remain in dataset
minnm <- 4;
#' Screw it, here is an object to store all the patterns
#' At runtime these will get collapsed by '|' to strings to be used as regexp 
#' targets
rxp <- list(
  # can add more values and/or new lines as needed
  # just make sure there are no leading or trailing
  # commas. Note that for the prostate cancer 
  # sex_cd is non-analytic, but for most datasets
  # this will not be the case
  shortenlevels =  c('_Prvdr_Spclt$','_Encntr_Tp$','_Fncl_Cls$'),
  # Grep targets for columns that are actually blobs of JSON
  JSON2num =       c('_Yrs_Tbc_Usg$','_Pcks_Pr_D$'),
  # At the moment it happens that both of the JSON variables also have numeric 
  # values that need to be extracted. This will not always be the case, and so a 
  # separate var vector might be needed.
  JSON =           c('_Yrs_Tbc_Usg$','_Pcks_Pr_D$'),
  # Grep targets for column names that should not be part of analysis.
  # (after already eliminating all the _info columns)
  nonanalytic =    c('_Yrs_Tbc_Usg$','_Pcks_Pr_D$','_info$',
                     '_date$','_Dschrg_Dspstn$','_unit$','^sex_cd$'),
  # grep targets for column names some or all of which we expect to be 
  # collected specifically during in-person visits.
  realvisit =      c('stlc_Prsr_num$',
                     '_Pls_num$','_Wght_oz_num$',
                     '_Bd_Ms_Indx_num$','_Hght_cm_num$',
                     '_Tmprtr_F_num$'),
  vitals =         c('stlc_Prsr_num$','_Rsprtn_Rt_num$',
                     '_Pls_num$','_Wght_oz_num$',
                     '_Bd_Ms_Indx_num$','_Hght_cm_num$',
                     '_Tmprtr_F_num$'),
  diags =          c('_Mlgnt_prst','_Esphgl_rflx'),
  lab =            c('\\d{3,6}_\\d_num$','COMPONENT_ID_\\d{3,6}_num$'),
  smoking =        c('_Nvr$','_Nt_Askd$','_Pcks_Pr_D','_Psv$','_Qt$',
                     '_Smk(ls|ng)_(Tbc|Qt)','_Ys$','_Yrs_Tbc_Usg'),
  twolvl =         c('_Mlgnt_prst','_Esphgl_rflx','_Dcsd_pr_SS$',
                     # the below two can actually be a splitCodes use case
                     # but will deal with that later
                     '_Smkng_Tbc_Us$','_Smkls_Tbc$'),
  keep =             '_Dcsd_pr_SS$', # always keep column even if many missing values
  patid =            '^patient_num$' # patient ID column (deidentified)
);

#' This one is separate because it's done to column values, not column names
pattern_removefromvals <- paste0(c('"GENERIC_UTHSCSA_FINCLASS_',
                                   '"GENERIC_UTHSCSA_ENC_TYPE_',
                                   '"GENERIC_UTHSCSA_VISITDETAIL_DEPT_\\d+',
                                   '"GENERIC_UTHSCSA_VISITDETAIL_SPEC_',
                                   '"'),collapse='|');


#' These variables should always be factors, even if they look like numbers.
#vars_factor <- c(vars_patid);
#' 
#' TODO: _info are not valid JSON

#' Variables to later convert to 2-level factors

#' Read in Data and Review It
#' df0 will be where the raw data is read in
df0 <- read_csv(datafile, na = c("", "(null)")
                ,locale = locale(date_format="%m/%d/%y"));
#' Create column name lists for df0
df0cls <- sapply(rxp,function(xx) 
  grep(paste0(xx,collapse='|'),names(df0),val=T),simplify=F);

df0cls$global <- names(df0)[1:7];
#' Find the variables with too few nonmissing values
df0cls$toofew <- setdiff(names(df0)[sapply(df0,function(xx) sum(!is.na(xx))<minnm)],
                         df0cls$keep);
df0cls$unchanging <- setdiff(names(df0)[sapply(df0,function(xx) length(unique(xx)))==1],
                             df0cls$keep);
df0cls$nonanalytic <- with(df0cls,unique(c(nonanalytic,toofew,unchanging)));
df0cls$twolvl <- with(df0cls,union(setdiff(names(df0)[sapply(df0,function(xx){
  xxuq <- unique(xx); NA %in% xxuq && length(xxuq)==2})],nonanalytic),twolvl));

#' Convert JSON values to lists squished into a `data.frame` column.
df0[,df0cls$JSON] <- sapply(df0[,df0cls$JSON],jsonParse,simplify = F);
for(ii in df0cls$JSON2num) 
  df0[[paste0(ii,'_num')]] <- as.numeric(trimws(dfListExtract(df0[[ii]],'nv')));

#' Find the numeric columns
df0cls$numeric <- setdiff(vs(df0,'z'),
                          with(df0cls,c(diags,patid,twolvl,nonanalytic)));

#' Update the smoking column names
df0cls$smoking <- setdiff(grep(paste0(df0cls$smoking,collapse='|'),names(df0),val=TRUE),
                          df0cls$nonanalytic);
#' Replace the extra stuff in certain code-columns
for(ii in df0cls$shortenlevels){
  .iilevs <- levels(df0[[ii]] <- factor(df0[[ii]]));
  .iilevs <- levels(df0[[ii]]) <- gsub(pattern_removefromvals,'',.iilevs);
  .iilevs <- levels(df0[[ii]]) <- gsub(',+',',',gsub('^,+|,+$','',.iilevs));
}

#' Add prefixes of the columns that are going to be 
v_listprf <- substr(df0cls$shortenlevels,1,5);
v_listsuf <- substr(df0cls$shortenlevels,6,100);
#' Expand the code-list columns. It so happens that these are the 
#' same columns as in vars_shortenlevels, but in future datasets
#' this may change. Really need to fix this in DF
#' Holy shit is this messy.
for(ii in seq_along(df0cls$shortenlevels)){
  .iiname <- df0cls$shortenlevels[ii]; .iiprf <- v_listprf[ii]; .iisuf <- v_listsuf[ii];
  .iidf <- splitCodes(df0[[.iiname]],.iiprf);
  .mapii <- switch(.iisuf,
                   'Encntr_Tp'= mapen,
                   'Fncl_Cls'= mapfc,
                   'Prvdr_Spclt'=mapsp);
  .newnamesii <- paste0(.iiprf,
                        .mapii[match(substr(names(.iidf),6,100),.mapii$numcode),'shortname'][[1]]);
  df0cls[[.iisuf]] <- names(.iidf) <- .newnamesii;
  df0<-cbind(df0,.iidf);
}


#' Remove info columns, retaining which ones they are in the `vars_noninfo` variable.
#df1 <- df0[ , vars_noninfo <- grep("_info$", names(df0),inv=T,val=T)];

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

#' Sanity check... from what we know about the variables, does this look right?
# Numeric Variables
#' See which columns were guessed to be non-numeric
#setdiff(names(df1),vars_numeric);

#' If all looks good, convert numeric columns to numeric 
#' (in this case the `for` loop is faster)
for(.ii in df0cls$numeric) df0[[.ii]] <- as.numeric(df0[[.ii]]);
df0$age_years <- df0$age_at_visit_days / 365  # Convert to years.
df0cls$numeric <- c(df0cls$numeric,'age_years');
#' ...and characters to factors, again saving the selected column names first
#' Note that since we rely on column types, this has to run after the numeric
#' coercions above are finished. Otherwise it will catch some of the numeric 
#' columns.
df0cls$factor <- setdiff(vs(df0,'c'),df0cls$nonanalytic);
df0[,df0cls$factor] <- sapply(df0[,df0cls$factor],factor,simplify = F);
#' Collapse certain variables to two levels
df0[,df0cls$twolvl] <- data.frame(lapply(df0[,df0cls$twolvl],
                                         function(xx) factor(!is.na(xx))));

#' Total up nonmissing vitals, labs, diags, and smoking
df0$nm_vitals <- apply(df0[,df0cls$vitals],1,function(xx) sum(!is.na(xx)));
df0$nm_labs <- apply(df0[,df0cls$lab],1,function(xx) sum(!is.na(xx)));
df0$nm_diags <- apply(df0[,df0cls$diags],1,function(xx) sum(xx=='TRUE'));
df0$nm_smoking <- with(df0cls,
                       apply(df0[,intersect(smoking,numeric)],1,function(xx) sum(!is.na(xx)))
                       +
                         apply(df0[,intersect(smoking,twolvl)],1,function(xx) sum(xx=='TRUE')));
#' Find the variables with hopelessly few nonmissing values
#' Hardcoding some new columns
#' 
#' DF_TODO: Put this into DataFinisher, many projects will need this
#' TODO: Oops, forgot to pull vitals! Will need to re-run. :-()
#df1$v039_Wght_lbs_num  <- df1$v039_Wght_oz_num * 0.0625 # Convert to pounds.

#' Let's mark the in-person visits. New function in `helpers.R` for doing this 
#' concisely. Additional `ids` and `indicators` columns added to `df1`.
#' For larger datasets might want to capture those as a separate two-column
#' `data.frame` by setting the optional `returnDF` argument to `FALSE` and
#' then inserting the columns of that `data.frame` into `df1` in a separate
#' command. But this is a reasonably sized dataset.
df0 <- findEvents(df0,df0cls$realvisit);
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
meta_flevels <- sapply(df2[, vs(df2, "f")], function(xx) length(levels(xx)));
cbind(sort(meta_flevels))
#' 
#' # TODO:
#' * Use the `splitCodes()` function
#' * Write function for remapping certain codes to readable names
#' * SMOKING_TOB_USE is another `splitCodes()` case
#' * Remove department and just keep spec for Prvdr_Spclt, which makes it another splitCodes()
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
#' 
