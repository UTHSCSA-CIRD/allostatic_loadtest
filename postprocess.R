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
#' Whether to do interactive diagnostic plots for units and valueflags, 
#' respectively
plotunits <- F; plotvfs <- F;
#' The name of the raw data file
datafile <- 'allo_05_combined.csv';
#' Minimum number of non-missing values to remain in dataset
minnm <- 4;
#' Minimum number of visits for below which the specialty, enctype, or financial 
#' class are binned together in the 'Other' category
minvs <- 18;

#' The training set and control training set were defined once, near the end of this script.
#' From now on they must remain static, at the following values:
trset <- c('4668579','4639333','4025815','4610333','4451646','5395328','4135687','5601609','3934070','5615572','4281679','4303649','4091860','5529362','4123946','5411073','4596447','5069805','3883587','3942296','5499986','5495319','4314613','4822143','5476090','4083814','4157378','5447492','3971116','5375977','4288851','4316696','3919462','4216406','5394713','5270628','5158127','4888638','5608673','3918313','3901964','5237956','5609476','5044537','5562742','4712658','5631163','4207786','4652271','5632107','4225291','4222985','5302101','5533523','5294228','4031582','5623393','5636855','4028511','5433112','4893191','4437214','4186597','4192578','3854215','4927155','4680640','4929015','4827229','4282324','4801094','5095790','3866081','5068675');
trsetctr <- c('4666105','4121041','5150369','4738333','4137444','4705027','5314101','4729094','3780606','4255169','4904234','4094606','5570643','5394798','5412335','5425457','4307097','4474620','4702035','4972124','5255189','3778123','4163621','4550401','3999892','4790240','5067431','4343005','4164096','5260836','5205367','4235239','3783330','4370225','4872870','5533837','5099659','4813273','4145966','5375190','5000277','3785966','4216569','4641892','4126881','4077354','5003694','4235229','5156176','4731278','4198662','3793194','5161804','3810226','5005072','4344854','5369225','5496383','5394673','5343282','4198829','5062097','4168668','3998687','5585093','3931787','5100827','4854070','3810868','3813464','4500029','5515735','3926477','5219911','4465484','4211912','5641204','5284502','5334742','4769108','4909771','5598522','5237898','4697217','4283927','5612635','4932733','4608623','4833100','4777769','4514549','4041503','5432241','4691755','5297623','4046932','5567273','3978506','4041889','4779867','5022956','4559444','5302710','5403136','4735590','5530538','3779219','4258578','4073874','4437572','4830693','4080508','4324070','4840386','5602521','3812468','4364020','4682210','4076374','4701147','4458290','5520452','4337124','5094756','4868417','4522663','5376061','5214022','4503866','3920833','4664601','4826674','3843651','5232510','4831201','4551705','5445933','4565747','5094339','4206683','4117872','5150902','4977389','5094345','5601726','4249743','4621289','4591382','3845801','4107724');
#' Date range/var/dbuilder1/allopr/allo_05_combined.csv
daterange <- as.POSIXct(as.Date(c(
  '2006-01-01','2016-06-01'
)));
#' Screw it, here is an object to store all the patterns
#' At runtime these will get collapsed by '|' to strings to be used as regexp 
#' targets
rxp <- list(
  # can add more values and/or new lines as needed
  # just make sure there are no leading or trailing
  # commas. Note that for the prostate cancer 
  # sex_cd is non-analytic, but for most datasets
  # this will not be the case
  shortenlevels =  c('_Prvdr_Spclt$','_Encntr_Tp$','_Fncl_Cls$','_Ethnct$'),
  splitlevels =    c('_Prvdr_Spclt$','_Encntr_Tp$','_Fncl_Cls$'),
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
  realvisit =      '\\d_Office_Vis',
  #realvisit =      c('stlc_Prsr_num$',
  #                   '_Pls_num$','_Wght_oz_num$',
  #                   '_Bd_Ms_Indx_num$','_Hght_cm_num$',
  #                   '_Tmprtr_F_num$'),
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
  patid =            '^patient_num$', # patient ID column (deidentified)
  force2factor =     '^patient_num$',
  onco =             'Onco',
  pcp =              '(Family|Geriatric|Internal)_Medicine$',
  event =            '_Mlgnt_prst$',
  event_preexisting ='_Mlgnt_prst_inactive$',
  # this one is rather ad-hoc, but basically the labs that permit at least a 
  # tiny sample size rather than the 0 for more ambitious combinations of labs
  safelabs =     c('_3094_0_num$','_2028_9_num$','_17861_6_num$','_2075_0_num$',
                     '_2160_0_num$','_2345_7_num$','_2823_3_num$','_2951_2_num$'),
  # ditto for labs
  safevitals =   c('_.stlc_Prsr_num$','_Pls_num','_Wght_oz_num')
);

#' This one is separate because it's done to column values, not column names
pattern_removefromvals <- paste0(c('"GENERIC_UTHSCSA_FINCLASS_',
                                   '"GENERIC_UTHSCSA_ENC_TYPE_',
                                   '"GENERIC_UTHSCSA_VISITDETAIL_DEPT_\\d+',
                                   '"GENERIC_UTHSCSA_VISITDETAIL_SPEC_',
                                   'GENERIC_DEM_ETHNICITY_',
                                   '"'),collapse='|');


event_definition <- "!is.na(%s)";

#' These variables should always be factors, even if they look like numbers.
#vars_factor <- c(vars_patid);
#' 
#' TODO: _info are not valid JSON

#' Variables to later convert to 2-level factors

#' Read in Data and Review It
#' dfraw will be where the raw data is read in
dfraw <- read_csv(datafile, na = c("", "(null)")
                ,locale = locale(date_format="%m/%d/%y"));

rxp$event %>% lazygrep(names(dfraw)) %>% sprintf(event_definition,.) %>% 
  paste0(collapse='|') %>% parse(text=.) %>% `[[`(1) -> event_expression;

rxp$event_preexisting %>% lazygrep(names(dfraw)) %>% sprintf(event_definition,.) %>% 
  paste0(collapse='|') %>% parse(text=.) %>% `[[`(1) -> preexisting_expression;

#' Create column name lists for df0
df0cls <- sapply(rxp,lazygrep,names(dfraw),simplify=F);

#' `deflateDF()` removes rows and columns that are empty (perhaps as a result of a 
#' subsetting operation), and pipe through `beforeAfter()` in order to add 
#' several transformations of the time variable and encode events for Anderson 
#' Gill format.
df0 <- subset(dfraw,start_date>daterange[1]&start_date<daterange[2]);

#' # Define a bunch more column-lists
df0cls$global <- names(df0)[1:7];
#' Original variables
df0cls$origvars <- setdiff(names(df0),df0cls$global);
#' Find the variables with too few nonmissing values
df0cls$toofew <- setdiff(names(df0)[sapply(df0,function(xx) sum(!is.na(xx))<minnm)],
                         df0cls$keep);
df0cls$unchanging <- setdiff(names(df0)[sapply(df0,function(xx) length(unique(xx)))==1],
                             df0cls$keep);
df0cls$nonanalytic <- with(df0cls,unique(c(nonanalytic,toofew,unchanging)));
df0cls$twolvl <- with(df0cls,union(setdiff(names(df0)[sapply(df0,function(xx){
  xxuq <- unique(xx); NA %in% xxuq && length(xxuq)==2})],nonanalytic),twolvl));

#' # Starting column transforms
#' Convert JSON values to lists squished into a `data.frame` column.
df0[,df0cls$JSON] <- sapply(df0[,df0cls$JSON],jsonParse,simplify = F);
for(ii in df0cls$JSON2num) 
  df0[[paste0(ii,'_num')]] <- as.numeric(trimws(dfListExtract(df0[[ii]],'nv')));


#' Update the smoking column names
df0cls$smoking <- setdiff(lazygrep(df0cls$smoking,names(df0)),df0cls$nonanalytic);

#' Replace the extra stuff in certain code-columns
for(ii in df0cls$shortenlevels){
  .iilevs <- levels(df0[[ii]] <- factor(df0[[ii]]));
  .iilevs <- levels(df0[[ii]]) <- gsub(pattern_removefromvals,'',.iilevs);
  .iilevs <- levels(df0[[ii]]) <- gsub(',+',',',gsub('^,+|,+$','',.iilevs));
}

#' Add prefixes of the columns that are going to be 
v_listprf <- substr(df0cls$splitlevels,1,5);
v_listsuf <- substr(df0cls$splitlevels,6,100);
#' Expand the code-list columns. It so happens that these are the 
#' same columns as in vars_shortenlevels, but in future datasets
#' this may change. Really need to fix this in DF
#' Holy shit is this messy.
for(ii in seq_along(df0cls$splitlevels)){
  .iiname <- df0cls$splitlevels[ii]; .iiprf <- v_listprf[ii]; .iisuf <- v_listsuf[ii];
  .iidf <- splitCodes(df0[[.iiname]],.iiprf);
  .mapii <- switch(.iisuf,
                   'Encntr_Tp'= mapen,
                   'Fncl_Cls'= mapfc,
                   'Prvdr_Spclt'=mapsp);
  .newnamesii <- paste0(.iiprf,
                        .mapii[match(substr(names(.iidf),6,100),.mapii$numcode),'shortname'][[1]]);
  names(.iidf) <- .newnamesii;
  df0cls[[paste0('toofew_',.iisuf)]] <- .fewvisii <- names(.iidf)[colSums(.iidf=='TRUE')<minvs];
  .iidf[[paste0(.iiprf,'Other')]] <- apply(.iidf[,.fewvisii,drop=F],1,
                                                  function(xx) any(xx=='TRUE'));
  .iidf[,.fewvisii] <- NULL;
  df0cls[[.iisuf]] <- names(.iidf);
  df0<-cbind(df0,.iidf);
}

df0cls$onco <- grep(rxp$onco,df0cls$Prvdr_Spclt,val=T);
df0cls$pcp <- grep(rxp$pcp,df0cls$Prvdr_Spclt,val=T);
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
#' Find the numeric columns
df0cls$numeric <- setdiff(vs(df0,'z'),
                          with(df0cls,c(diags,patid,twolvl,nonanalytic)));


for(.ii in df0cls$numeric) df0[[.ii]] <- as.numeric(df0[[.ii]]);
df0$age_years <- df0$age_at_visit_days / 365  # Convert to years.
df0cls$numeric <- c(df0cls$numeric,'age_years');
#' ...and characters to factors, again saving the selected column names first
#' Note that since we rely on column types, this has to run after the numeric
#' coercions above are finished. Otherwise it will catch some of the numeric 
#' columns.
df0cls$factor <- setdiff(c(df0cls$force2factor,vs(df0,'c')),df0cls$nonanalytic);
df0[,df0cls$factor] <- sapply(df0[,df0cls$factor],factor,simplify = F);
#' Collapse certain variables to two levels
df0[,df0cls$twolvl] <- data.frame(lapply(df0[,df0cls$twolvl],
                                         function(xx) !is.na(xx)));

#' Total up nonmissing vitals, labs, diags, and smoking
df0$nm_vitals <- apply(df0[,df0cls$vitals],1,function(xx) sum(!is.na(xx)));
df0$nm_labs <- apply(df0[,df0cls$lab],1,function(xx) sum(!is.na(xx)));
df0$nm_diags <- apply(df0[,df0cls$diags],1,function(xx) sum(xx=='TRUE'));
df0$nm_smoking <- with(df0cls,
                       apply(df0[,intersect(smoking,numeric)],1,function(xx) sum(!is.na(xx)))
                       +
                         apply(df0[,intersect(smoking,twolvl)],1,function(xx) sum(xx=='TRUE')));
df0$nm_vitalslabs <- with(df0,nm_vitals+nm_labs);
df0$nm_total <- with(df0,nm_vitalslabs+nm_diags+nm_smoking);
df0cls$nnonmissing <- grep('^nm_',names(df0),val=T);

#' Grep pattern for getting rid of instance numbers:
# foo <- data.frame(sapply(df0[,df0cls$info],function(xx) gsub("\\'ix\\':\\[(\\'\\d+\\',{0,1})+\\],{0,1}","",xx),simplify=F))
#' Info columns
df0cls$info <- intersect(names(df0),gsub('_num$','_info',df0cls$lab));
#' The info columns that can provide a vf (value-flag)
df0cls$vfinfo <- df0cls$info[sapply(df0[,df0cls$info],function(xx) 
  any(grepl("'vf':",levels(factor(xx)))))];
df0cls$vf <- gsub('_info$','_vf',df0cls$vfinfo);
#' Add the valueflag columns!
df0[,df0cls$vf] <- data.frame(sapply(df0[,df0cls$vfinfo],mapLevels,
                                     exclude=NA,simplify=F));
#' Later our analysis will need to distinguish between affirmatively normal
#' lab values and missing ones. The following step prepares for this.
#' The NA value-flags for non-missing lab-values will be recoded as 'N'
for(ii in df0cls$vf) {
  nii <- gsub('_vf$','_num',ii);
  df0[!is.na(df0[[nii]])&is.na(df0[[ii]]),ii] <- 'N';
}

#' Review values
if(plotvfs)
  for(ii in intersect(gsub('_num$','_vf',df0cls$safelabs),df0cls$vf)){
    .iin<-gsub('_vf$','_num',ii); 
    if(is.numeric(df0[[.iin]])&&length(levels(df0[[ii]]))>1) 
      feedbackOMatic(ii,vffb,
                     stripchart(formula(paste(.iin,'~I(factor(',ii,',exclude = NULL))')),
                                df0,method='jitter',pch='.',
                                cex=2.5,col='#FF444430',ver=T,las=2,
                                ylim=if(is.na(.vs$lim)) c() else c(0,.vs$lim)));
  }

#' Define unit columns
df0cls$unit<-setdiff(intersect(gsub('_num$','_unit',grep('_num$',names(df0),val=T)),
                               names(df0)),df0cls$toofew);
#' Make them into factors
df0[,df0cls$unit]<-data.frame(sapply(df0[,df0cls$unit],factor,exclude=NULL,
                                     simplify=F));
#' Try out the unit plots.
if(plotunits)
  for(ii in df0cls$unit){
    .iin<-gsub('_unit$','_num',ii); 
    if(is.numeric(df0[[.iin]])&&length(levels(df0[[ii]]))>1) 
      feedbackOMatic(ii,unitfb,
                     stripchart(formula(paste(.iin,'~',ii)),
                                df0,method='jitter',pch='.',
                                cex=2.5,col='#FF444430',ver=T,las=2,
                                ylim=if(is.na(.vs$lim)) c() else c(0,.vs$lim)));
  }
#' DF_TODO: Put this into DataFinisher, many projects will need this
#' TODO: Oops, forgot to pull vitals! Will need to re-run. :-()
#df1$v039_Wght_lbs_num  <- df1$v039_Wght_oz_num * 0.0625 # Convert to pounds.

#' Let's mark the in-person visits. New function in `helpers.R` for doing this 
#' concisely. Additional `ids` and `indicators` columns added to `df1`.
#' For larger datasets might want to capture those as a separate two-column
#' `data.frame` by setting the optional `returnDF` argument to `FALSE` and
#' then inserting the columns of that `data.frame` into `df1` in a separate
#' command. But this is a reasonably sized dataset.
#' Update the realvisit column list, since columns may have been added
df0cls$realvisit <- lazygrep(rxp$realvisit,names(df0));
df0 <- findEvents(df0,cnames=c(df0cls$realvisit,df0cls$event),
                  selectfun=any,groupby=df0cls$patid);
#' Create a unique patient_num/visit-set combo `pn_vis`
#df0$pn_vis <- paste(df0[,df0cls$patid],df0$ids,sep=':');
#' list-valued columns in df0, so we can avoid them breaking lastNonMissing()
df0cls$nonlists <- names(df0)[sapply(df0,is.atomic)];
#' Update the nonanlytic list of column names
df0cls$nonanalytic <- union(df0cls$nonanalytic,c('ids','indicators','pn_vis'));

#' Change of plan from below, for how to invoke the collapsing of lab-visits into
#' office visits.
split(df0[,df0cls$nonlists],df0$groupids) %>% 
  lapply(function(xx) if(nrow(xx)==1) return(xx) else sapply(xx,lastNonMissing,simplify=F) %>% data.frame) %>% 
  bind_rows -> df1;

split(df1,df1[,df0cls$patid]) %>% 
  lapply(beforeAfter,parse(text = paste0(df0cls$event,collapse='|'))[[1]]) %>% 
  bind_rows %>% subset(event<2&(is.na(ev)|ev>1)) -> df2;

#' There was a bug in calculating tevent in versions previous to 16/10/02
#' If reusing data.frames calculated before then, do the following patch:
# df2[is.na(df2$ev),'tevent']<- -df2[is.na(df2$ev),'tevent']
#' ...and similarly for df3 if reusing that

#' At the time of this writing, preexisting_patients ends up being c(), but that
#' seems to not be an error-- rather, we don't have any preexisiting PC after
#' updating the event criteria to include the ICD10 version
subset(df2,event==0 & eval(df0cls$event_preexisting %>% paste0(collapse='|') %>% 
                             parse(text=.) %>% `[[`(1)))[[df0cls$patid]] %>%
  unique %>% as.character -> preexisting_patients;

if(length(preexisting_patients)>0) df3 <- subset(df2,!patient_num%in%preexisting_patients) else {
  df3 <- df2;
}

#' # We take a random training sample of about half the  PC patients.
if(!exists('trset')) trset <- sample(unique(subset(df3,!is.na(ev))[[df0cls$patid]]),round(length(unique(df3[[df0cls$patid]]))/2));


#' In preparation for the control sample, here are the tstart quantiles for
#' the PC sample:
split(subset(df3,!is.na(ev))$age_at_visit_days,
      subset(df3,!is.na(ev))[[df0cls$patid]],drop = T) %>% sapply(min) %>% 
  quantile(seq(0,1,by=.1)) -> df3quants;

if(!exists('trsetctr')){
  with(subset(df3,is.na(ev)),split(patient_num,cut(tstart,df3quants,include.lowest = T))) %>% 
    lapply(function(xx) length(unique(xx))) %>% unlist %>% min -> ctrminbin;
  
  with(subset(df3,is.na(ev)),split(patient_num,cut(tstart,df3quants,include.lowest = T))) %>% 
    lapply(function(xx) sample(unique(xx),min(ctrminbin))) %>% unlist -> smpctr;
}

#' Now let's see how big the smallest decile is without holding out the PC patients
with(df3,split(patient_num,cut(tstart,df3quants,include.lowest = T))) %>% lapply(function(xx) length(unique(xx))) %>% unlist %>% min -> allminbin;
#' So, 48
replicate(1000,with(df3,split(patient_num,cut(tstart,df3quants,include.lowest = T))) %>% 
            lapply(function(xx) sample(unique(xx),allminbin)) %>% unlist %>% as.character) -> resampledFromCombined;
apply(resampledFromCombined,2,function(xx) nrow(subset(df3,patient_num %in% xx & event ==1)))->pcs;
#' One average, there are `r mean(pcs)` PC patients and therefore `r 480-mean(pcs)` 
#' non-PC patients in such a sample, or `r (480-mean(pcs))/480`. In our actual training set we will use 150 
#' control patients and the 74 PC patients in `trset`. Therefore, `r 150/(150+74)`
#' of them will be non-PC. This means they will be `r (150/(150+74))/((480-mean(pcs))/480)`
#' of the actual population. Furthermore, that population itself is 1/10 th of 
#' the overall patient population in the Family/Geriatric/Internal med clinics.
#' So our weighing factor should be:
ctrwt <- 10/((150/(150+74))/((480-mean(pcs))/480)); ctrwt;
#' Although, later we find out that we lose a lot more samples to missing values,
#' so the more accurate weight for complete cases is...
ctrtrwt <- 10/((70/(57+70))/((480-mean(pcs))/480))
#' Add weighing factor to the data.frame.
df3$smplwt <- ifelse(is.na(df3$ev),ctrwt,1);
#' Here is going to be our actual control sample for the training set
if(!exists('trsetctr')) trsetctr <- sample(smpctr,150);

#' Finally. Our training set.
df4 <- df3[df3[[df0cls$patid]]%in%c(trset,trsetctr),];
df4$smplwt <- ifelse(is.na(df4$ev),ctrtrwt,1);

#rpcheat <- deflateReport(df2[,with(df0cls,c(patid,lab))]);
#rp <- deflateReport(df3[,with(df0cls,c(patid,lab))]);
#rpall <- deflateReport(df3);
#' The following report can help us decide what to put in which dataset:
#' * Naive approach: at least 40 patients w/ >=3 complete cases
#' * Interpolation/Imputation: at least 40 patients w/ >=3 ocurrences of variable
#' * Score: base that on manual review of vf plots
#df0cls$casecomp <- setdiff(rownames(subset(df4rpt,HaveCompleteCases>=20)),c(df0cls$nonanalytic));
#df0cls$imputable <- setdiff(rownames(subset(df4rpt,HaveEnoughData>40)),c(df0cls$nonanalytic));

#' How many case-complete observations actually remain?
#nrow(na.omit(df3[,df0cls$casecomp]));
#' How many unique patients are still covered? (remember, we'll have to fit our
#' model on a training set containing only half of them).
length(unique(na.omit(df3[,df0cls$casecomp])[[df0cls$patid]]));
#' How many of them have more than one complete visit before initial diagnosis?
#sum(summary(factor(na.omit(df3[,df0cls$casecomp])[[df0cls$patid]]),maxsum = 1000)>1);
#' # Interrim conclusion:
#' 
#' If we are willing to impute/interpolate up to 80% of the missing data, we can
#' use this:
# df4_imputable <- df3[,with(df0cls,c(global,vitals,diags,smoking,
#                                     'time','tevent','event','tstart',
#                                     'v039_Ethnct',
#                                     intersect(lab,imputable)))];
#' If we do only complete cases, we need to use fewer missing variables and should
#' use this:
# df4_casecomp <- df3[,with(df0cls,c(global,vitals,diags,smoking,
#                                    'time','tevent','event','tstart',
#                                    'v039_Ethnct',
#                                    intersect(lab,casecomp)))];
#' # Now watch, our first visualization of actual data. 
#' 
#' Risk of PC diagnosis for patients in the older half of the set
#' versus patients in the younger half.
# plot(survfit(Surv(time)~1,df4_casecomp,subset=scale(tstart)>=0&event==1),
#      las=2,xlab='Days Since First Visit',main='PC Diagnosis Rate',bty='n',lwd=2)
# lines(survfit(Surv(time)~1,df4_casecomp,subset=scale(tstart)<0&event==1),col='red',lwd=2)
# legend('topright',legend=c('Younger','Older'),col=c('red','black'),pch=1,bty='n',cex=1);
# 
#' Take this with a huge grain of salt, though. We still need to add in the healthy
#' patients of comparable age at first visit and comparable distribution of health 
#' history lengths who so far never developed PC.


#' To select an age-balanced healthy sample from dataset bar we would do...
#split(bar,cut(bar$tstart,df2quants,include.lowest = T)) %>% 
#  lapply(function(xx) sample(unique(xx$patient_num),3)) %>% unlist %>% 
#  as.character -> matchedpns;
#subset(bar,patient_num %in% matchedpns) -> df2ctrl;
#' To construct a score, e.g. total high/low value-flags as a fraction of 
#' non-missing labs during a followup period, we will need to clean up the vfs,
#' throw out the uninformative ones, and then construct a (relatively simple) 
#' score on the rest.

#rept <- deflateDF(subset(df1,event<2&!patient_num%in%preexising)[,setdiff(names(df1),df0cls$nonanalytic)],
#                  df0cls$lab,sumThresh = 2,output='re'); 
#' Create Anderson-Gill format table for time-to-event analysis. Basically
#' for each patient, stop on their first diagnosis and create column of 0,1
#' censoring indicators. The catch is, if after doing that a patient still has 
#' inactive malignant prostate, we have to weed them out too, because that means
#' their first PC diagnosis in the EMR record is not their first PC diagnosis
# split(df0,df0$patient_num) %>% lapply(function(xx){
#     .xxmin<-min(which(xx$v000_Mlgnt_prst=='TRUE'));
#     if(is.infinite(.xxmin)||any(xx$v000_Mlgnt_prst_inactive[1:.xxmin]=='TRUE')) return(NULL);
#     out<-xx[1:.xxmin,];out$c<-c(rep(0,.xxmin-1),1);out;
#     }) %>% do.call(rbind,.) -> agdf0;
#' TODO: a lot of patients and visits have been dropped. Might need to re-run from
#' the start on agdf0 to find what the data problems, etc. are in this dataset.
#' Identify the analytic variables
#vars_analytic <- grep(patterns_nonanalytic,names(df1),inv=T,val=T);
#' Create an empty `data.frame` with an identical column layout to `df1`
#' Removing non-analytic columns
#df1 <- subset(df0,FALSE)[,df0cls$nonlists,drop=F];
#df1[seq_along(meta_unqpnvis <- unique(df0$pn_vis)),]<-NA;
#' Iterate over `pn_vis` to create the collapsed `data.frame` populated with
#' the last non-missing value of every column in the dataset. 
#' This part takes a looong time:
#pb <- txtProgressBar(min=0,max=length(meta_unqpnvis),style=3);
#for(ii in seq_along(meta_unqpnvis)){
#  setTxtProgressBar(pb,ii);
#  df1[ii,] <- data.frame(lapply(df0[df0$pn_vis==meta_unqpnvis[ii],df0cls$nonlists,drop=F],
#                                lastNonMissing));
#}
#close(pb);

#' Sanity-checking units 
#' Checking to see if any of the units of measurement will need converting later on.
# Not yet... let's get the rest of this working and then...
#summary(df1[ , grepl("unit", names(df1))])
#' 

#' Now, how many non-missing values does each row have? Our convention will be to
#' save lists of column names in vectors prefixed by `vars_` and lists of column
#' information in vectors prefixed by `meta_`.
# meta_nonmissing <- sapply(df1,function(xx) sum(!is.na(xx)));
#cbind(`Number Non Missing`=meta_nonmissing);
#' We keep just the variables that have more non-missing values than the threshold
#' set by `minnm`.
# vars_enoughvals <- sort(unique(c(names(meta_nonmissing)[meta_nonmissing>minnm]
#                                  ,df0cls$keep)));
#df1 <- df1[,vars_enoughvals];

#' Any crazy number of levels?
# meta_flevels <- sapply(df1[, vs(df1, "f")], function(xx) length(levels(xx)));
#cbind(sort(meta_flevels))
#' 
#' # TODOs
#' * TODO:Write up questions for collaborators:
#' ** Do we use the L/H valueflags as-is or use them as guidelines to recode some of the overlapping values?
#' ** What is our definition of a real visit? Office Visit code? Existence of vitals (how many?). Something else?
#' ** How to deal with unit differences?
#' ** How to construct score?
#' * DONE Use the `splitCodes()` function
#' * DONE Write function for remapping certain codes to readable names
#' * DONE: SMOKING_TOB_USE is another `splitCodes()` case, as is smokeless
#' * DONE Remove department and just keep spec for Prvdr_Spclt, which makes it another splitCodes()
#' * TODO: v023_Clr_Ur_5778_6_info is a code-valued lab, figure out how to catch and deal with these
#' * DONE: Decide on an indicator for 'real' visits (office visit, code 50)
#' * DONE: finish collapsing the lab-only visits into subsequent office visits
#' * DONE boxplot or stripchart each lab against units and valueflags
#' * DONE Anderson-Gill format
#' * TODO: ~catch~ and fix outliers (just for safe labs)
#' * DONE: control patients!
#' 
#' # Questions to think about:
#' 
#' * In what cases to parse variables out of JSON vs. pre-separate them in query?
#' * DONE How to map `Prvdr_Spclt`, `Encntr_Tp`, and `Fncl_Cls`?
#' * DONE GENERIC_KUMC_PACK_PER_DAY: NVAL_NUM is the only informative field, TODO: write new rule
#' * DONE Ditto for GENERIC_KUMC_TOBACCO_USED_YEARS, but why is units still 'Packs'?
#' * Perhaps DataFinisher should also include `DEATH_DATE`?
#' * How to plot this data?
#' 
#' 
