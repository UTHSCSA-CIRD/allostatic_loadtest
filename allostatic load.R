#' Load libraries
require("readr")
require("dplyr")
require("ggplot2")
source("helpers.R")

df1 <- read_csv("allostatic load.csv", na = c("", "(null)"),locale = locale(date_format="%m/%d/%y"))

#' Remove unnecessary info columns, convert characters to factors, and numeric columns to numeric.
df1 <- df1[ , !grepl("info", names(df1))]
#' Guess which columns are numeric
nums<-na.exclude(vs(df1,'z'))
#' See which columns were guessed to be non-numeric
setdiff(names(df1),nums)
#' In this case the `for` loop is faster
#df1[,nums] <- sapply(df1[,nums],as.numeric)
for(ii in nums) df1[[ii]] <- as.numeric(df1[[ii]])
#' Turn remaining character columns into factors
df1[ , vs(df1, 'c')] <- sapply(df1[ , vs(df1, 'c')], function(x) as.factor(x), simplify = FALSE)
#' No need for that. Integers are usually more efficient to manipulate
#df1$patient_num <- as.factor(df1$patient_num)

#' Consider creating a new column instead, named age_at_visit_years, since that's what this is
df1$age_at_visit_years <- df1$age_at_visit_days / 365  # Convert to years.
#' Ditto
df1$v017_Wght_lbs_num <- df1$v017_Wght_oz_num * 0.0625 # Convert to pounds.

# Produce data frame of number of visits per unique patient.
df1.counts <- count(df1, patient_num)
ggplot(df1.counts, aes(n)) + geom_histogram(binwidth = 5)

# Checking to see if any of the units of measurement will need converting later on.
lapply(df1[ , grepl("unit", names(df1))], FUN = unique)
#' Or you can try...
summary(df1[ , grepl("unit", names(df1))])
# Watch out for:
# $v001_Albmn_LP_1751_7_unit
# $v001_Albmn_LP_1755_8_unit
# $v004_rctv_prtn_1988_5_unit # yes, possible mismatch
# $v005_Chlstrl_LP_2089_1_unit
# $v006_Crtsl_LP_2143_6_unit
# $v007_D_DMR_KUH_COMPONENT_ID_2396_unit # yes, possible mismatch
# $v009_Dhdrpndrstrn_2191_5_unit
# $v001_Epnphrn_LP_2232_7_unit
# $v008_Nrpnphrn_LP_2668_2_unit


# Easy way to eyeball if any of the factor columns are "goofy." Tobacco is certainly goofy.
sapply(df1[, vs(df1, "f")], function(x) length(levels(x)))

#' Put all factors with only one level in a list for removal
#' 
#' Not a good idea! Look at this: `levels(factor(c(NA,'xx')))` 
#' 
#' Might want to at least eyeball it first
summary(df1[ , vs(df1, "f")])
kill_list <- names(which(sapply(df1[, vs(df1, "f")], function(x) length(levels(x)) == 1) == TRUE))
# browser()
df1 <- df1[ , !(names(df1) %in% kill_list)]

#' Sure, this is fine for now. Might want to kill them based on low count rather than uniqueness, though
#' Could in principle have a well-populated column that only ever has one of two possible values  
summary(df1[ , vs(df1, "n")])
sapply(df1[ , vs(df1, "n")], function(x) length(unique(x)), simplify = FALSE)
# Find numerical columns with only NA's and 1's.
kill_list <- names(which(sapply(df1[, vs(df1, "n")], function(x) length(unique(x)) <= 2))) 
df1 <- df1[ , !(names(df1) %in% kill_list)]

#' Collapse the smoking variables
levels(df1$v015_Tbc_Usg) <- gsub(",\"ix\":\"[0-9]{1,}\"",'',levels(df1$v015_Tbc_Usg));
#' Shorten the remaining variable name levels
levels(df1$v015_Tbc_Usg) <- gsub(",\"vf\":\"null\",\"un\":\"Packs\"",'',levels(df1$v015_Tbc_Usg))

#' You can collapse the `v000_Mlgnt_prst, v000_Mlgnt_prst_inactive, v004_gstrsphgl, v004_gstrsphgl_inactive`
#' into T/F for now... can always change this script later if the type of diagnosis turns out to be important,
#' and as T/F values, they will be much easier to analyze

#' What percent of variables are missing, and what percent, roughly of the total are missing.
#' (mice recommends not imputing more than 5% of a column)
#' Let's say you have more than 5% missing in a column, so imputation not recommended... but still a decent number
#' of values. Why not set a cutoff and bin them as "beyond cutoff" vs "within cutoff or missing"?
sapply(df1, function(x) mean(is.na(x)))
mean(sapply(df1, function(x) mean(is.na(x))))

# write_csv(df1, "allostatic load 2.csv")
