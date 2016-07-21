#' Load libraries
require("readr")
require("dplyr")
require("ggplot2")
source("helpers.R")

df1 <- read_csv("allo_prakash.csv", na = c("", "(null)"),locale = locale(date_format="%m/%d/%y"))

#' Remove unnecessary info columns, convert characters to factors, and numeric columns to numeric.
df1 <- df1[ , !grepl("info", names(df1))]

#' Guess which columns are numeric, See which columns were guessed to be non-numeric
nums<-na.exclude(vs(df1,'z'))
setdiff(names(df1),nums)

#' In this case the `for` loop is faster
for(ii in nums) df1[[ii]] <- as.numeric(df1[[ii]]); rm(ii, nums)

#' Turn remaining character columns into factors
df1[ , vs(df1, 'c')] <- sapply(df1[ , vs(df1, 'c')], function(x) as.factor(x), simplify = FALSE)
df1[ , vs(df1, 'c')] <- sapply(df1[ , vs(df1, 'c')], function(x) as.factor(x), simplify = FALSE)

#' Consider creating a new column instead, named age_at_visit_years, since that's what this is
df1$age_at_visit_years <- df1$age_at_visit_days / 365  # Convert to years.
df1$v039_Wght_lbs_num  <- df1$v039_Wght_oz_num * 0.0625 # Convert to pounds.
df1$age_at_visit_days               <- NULL
df1$v017_Wght_oz_num                <- NULL
df1$v004_Albmn_LP_1755_8_num        <- NULL
df1$v004_Albmn_LP_1755_8_unit       <- NULL
df1$v004_Albmn_LP_1755_8_info       <- NULL
df1[, grepl("2089_1", names(df1))]  <- NULL
df1[, grepl("18262_6", names(df1))] <- NULL
df1[, grepl("2145_1", names(df1))]  <- NULL
df1[, grepl("2147_7", names(df1))]  <- NULL
df1[, grepl("9812_9", names(df1))]  <- NULL
df1[, grepl("9813_7", names(df1))]  <- NULL
df1[, grepl("14338_8", names(df1))] <- NULL
df1[, grepl("1746_7", names(df1))]  <- NULL
df1[, grepl("1759_0", names(df1))]  <- NULL
df1[, grepl("2862_1", names(df1))]  <- NULL
df1[, grepl("2232_7", names(df1))]  <- NULL
df1[, grepl("2347_3", names(df1))]  <- NULL
df1[, grepl("2348_1", names(df1))]  <- NULL
df1[, grepl("27353_2", names(df1))] <- NULL
df1[, grepl("13782_8", names(df1))] <- NULL
df1[, grepl("2668_2", names(df1))]  <- NULL
df1[, grepl("30522_7", names(df1))] <- NULL
df1[, grepl("35741_8", names(df1))] <- NULL


#' ###Stop the presses. The majority of visits are not physical encounters between patient and doc!
#' They seem to be lab visits. How do we detect the 'real' ones? Perhaps by the presence of vitals?
real_visit_vars <- c('v039_Wght_lbs_num', 'v033_Pls_num', 'v011_Dstlc_Prsr_num', 'v005_Bd_Ms_Indx_num', 'v023_Hght_cm_num', 'v018_Tmprtr_F_num', 'v034_Rsprtn_Rt_num')
df1$visit_indicator <- apply(df1[, real_visit_vars], 1, function(x) !all(is.na(x))); rm(real_visit_vars)
df1$cumm_sum <- cumsum(df1$visit_indicator)
df1$cumm_sum <- c(0, df1$cumm_sum)[1:nrow(df1)]
df1$unique_string <- paste(df1$patient_num, df1$cumm_sum)

firstNonNA <- apply(df1,2,function(xx) min(which(!is.na(xx))))
df1[22351, ]$unique_string
df1_temp <- df1[df1$unique_string == "75602 4187", ]
df1_temp_out <- data.frame(lapply(df1_temp,lastNonMissing))

###########################################################

#enableJIT(3)
df3 <- subset(df1, FALSE)
df3[1:length(unique(df1$unique_string)), 1:66] <- NA
df1.unique <- unique(df1$unique_string)

# lastNonMissing <- function(xx) if(all(is.na(xx))) return(NA) else return(last(na.omit(xx)));
system.time(
  for(ii in 1:length(unique(df1$unique_string))) {
    #cat('.') 
    df3[ii, ] <- data.frame(lapply(df1[df1$unique_string == df1.unique[ii], ], lastNonMissing))
  }
)

df3$unique_string <- paste(df3$patient_num, df3$cumm_sum)

###############################################################################################


# Produce data frame of number of visits per unique patient.
df1.counts <- count(df1, patient_num)
ggplot(df1.counts, aes(n)) + geom_histogram(binwidth = 5)


# Checking to see if any of the units of measurement will need converting later on.
# Watch out for rctv, crtsl, dhdrpndrstrn, and DMR.
summary(df1[ , grepl("unit", names(df1))])


# Easy way to eyeball if any of the factor columns are "goofy." Tobacco is certainly goofy.
sapply(df1[, vs(df1, "f")], function(x) length(levels(x)))

#' Let's look at the Tobacco levels in more detail, after getting rid of the instance numbers
#' that cause them to be spuriously unique
View(table(gsub('\"cc\":\"GENERIC_KUMC_TOBACCO_USED_YEARS\",\"ix\":\"[0-9]{1,}\",','',levels(df1$v040_Yrs_Tbc_Usg))));

#' Down to 168 distinct levels without the instance numbers. Now let's try in addition getting rid of the repetitive null entries...
View(table(gsub('\\{\"vf\":\"null\",\"un\":\"Packs\"\\},','',gsub('\"cc\":\"GENERIC_KUMC_TOBACCO_USED_YEARS\",\"ix\":\"[0-9]{1,}\",','',levels(df1$v040_Yrs_Tbc_Usg)))))

#' ...and of some more, partially null entries
View(table(gsub(',\"vf\":\"null\",\"un\":\"Packs\"\\}','',gsub('\\{\"vf\":\"null\",\"un\":\"Packs\"\\},','',gsub('\"cc\":\"GENERIC_KUMC_TOBACCO_USED_YEARS\",\"ix\":\"[0-9]{1,}\",','',levels(df1$v040_Yrs_Tbc_Usg))))))

#' Looks like the only value-flag (`"vf"`) values were `"null"` and the only unit (`"un"`) values were `"Packs"`. This means we can
#' just convert these to character, extract the numeric values of the `"nv"` field, and call it a day!


#' Put all factors with only one level in a list for removal
#' 
#' Not a good idea! Look at this: `levels(factor(c(NA,'xx')))` 
#' 
#' Might want to at least eyeball it first
summary(df1[ , vs(df1, "f")])
kill_list <- names(which(sapply(df1[, vs(df1, "f")], function(x) length(levels(x)) == 1) == TRUE))
df1 <- df1[ , !(names(df1) %in% kill_list)]

#' Sure, this is fine for now. Might want to kill them based on low count rather than uniqueness, though
#' Could in principle have a well-populated column that only ever has one of two possible values  
summary(df1[ , vs(df1, "n")])
sapply(df1[ , vs(df1, "n")], function(x) length(unique(x)), simplify = FALSE)

# Find numerical columns with only NA's and 1's.
kill_list <- names(which(sapply(df1[, vs(df1, "n")], function(x) length(unique(x)) <= 2))) 
#df1 <- df1[ , !(names(df1) %in% kill_list)]


#' Collapse the smoking variables
#levels(df1$v015_Tbc_Usg) <- gsub("KUMC_",'',levels(df1$v015_Tbc_Usg));
#levels(df1$v015_Tbc_Usg) <- gsub(",\"ix\":\"[0-9]{1,}\"",'',levels(df1$v015_Tbc_Usg));
#' Shorten the remaining variable name levels
#levels(df1$v015_Tbc_Usg) <- gsub(",\"vf\":\"null\",\"un\":\"Packs\"",'',levels(df1$v015_Tbc_Usg))

#' You can collapse the `v000_Mlgnt_prst, v000_Mlgnt_prst_inactive, v004_gstrsphgl, v004_gstrsphgl_inactive`
#' into T/F for now... can always change this script later if the type of diagnosis turns out to be important,
#' and as T/F values, they will be much easier to analyze

#' What percent of variables are missing, and what percent, roughly of the total are missing.
#' (mice recommends not imputing more than 5% of a column)
#' Let's say you have more than 5% missing in a column, so imputation not recommended... but still a decent number
#' of values. Why not set a cutoff and bin them as "beyond cutoff" vs "within cutoff or missing"?

#' How to tell if they're "beyond" the cutoff? Well, see if you can come up with a function that takes a vector of
#' numbers, and then returns the percentile each number occupies relative to all the other numbers in the vector
#' As you found, `quantile()` tells what the cutoff values should be (as should the clinical dudes, and better, 
#' but this is on the assumption that you're feeling impatient). The `between()` function will return a T/F
#' for whether a value is between the second and third arguments.


View(cbind(sort(sapply(df1, function(x) mean(is.na(x))))))
mean(sapply(df1, function(x) mean(is.na(x))))


#' = Next time: decide which columns to impute, which ones to bin, and which ones to throw away
#' ...and impute the ones that you can


df1$v003_Mlgnt_prst_inactive <- ifelse(is.na(df1$v003_Mlgnt_prst_inactive), 0, 1)
df1$v003_Mlgnt_prst          <- ifelse(is.na(df1$v003_Mlgnt_prst), 0, 1)

df1$v024_gstrsphgl           <- ifelse(is.na(df1$v024_gstrsphgl), 0, 1)
df1$v024_gstrsphgl_inactive  <- ifelse(is.na(df1$v024_gstrsphgl_inactive), 0, 1)


df1 %>% group_by(patient_num) %>% summarise(patient_visits = n())
temp1 <- df1 %>% group_by(patient_num) %>% summarise(patient_visits = n())
temp2 <- df1 %>% group_by(patient_num) %>% summarise(albumin_data_percent = 1 - mean(is.na(v004_Albmn_LP_1751_7_num)))
ggplot(temp2, aes(albumin_data_percent)) + geom_histogram(binwidth = .05)

#########################################################################


samp <- samp[samp$age_at_visit_years >= 20, ]
samp$v007_rctv_prtn_1988_5_num <- with(samp, v007_rctv_prtn_1988_5_num * ifelse(v007_rctv_prtn_1988_5_unit=='mg/l', 10,	1))
samp$cumm_sum         <- NULL
samp$unique_string    <- NULL
samp$v039_Wght_oz_num <- NULL

plot(samp[, vs(samp)[2:16]],pch='.',col='#00000010')
samp[, grepl("unit", names(samp))] <- sapply(samp[ ,grepl("unit", names(samp))], function(x) as.factor(x), simplify = FALSE)
summary(samp[, grepl("unit", names(samp))])

# v004_Albmn_LP_1751_7_num > 10
# v005_Bd_Ms_Indx_num > 100
# v018_Tmprtr_F_num < 90
# v023_Hght_cm_num < 120
# v032_Prst_spcfc_2857_1_num > 100
# v034_Rsprtn_Rt_num > 40
samp[!is.na(samp$v004_Albmn_LP_1751_7_num) & samp$v004_Albmn_LP_1751_7_num > 10, 'v004_Albmn_LP_1751_7_num']        <- NA
samp[!is.na(samp$v005_Bd_Ms_Indx_num) & samp$v005_Bd_Ms_Indx_num > 100, 'v005_Bd_Ms_Indx_num']                      <- NA
samp[!is.na(samp$v018_Tmprtr_F_num) & samp$v018_Tmprtr_F_num < 90, 'v018_Tmprtr_F_num']                             <- NA
samp[!is.na(samp$v023_Hght_cm_num) & samp$v023_Hght_cm_num < 120, 'v023_Hght_cm_num']                               <- NA
samp[!is.na(samp$v032_Prst_spcfc_2857_1_num) & samp$v032_Prst_spcfc_2857_1_num > 100, 'v032_Prst_spcfc_2857_1_num'] <- NA
samp[!is.na(samp$v034_Rsprtn_Rt_num) & samp$v034_Rsprtn_Rt_num > 40, 'v034_Rsprtn_Rt_num']                          <- NA
plot(samp[, vs(samp)[2:16]], pch='.', col='#00000010')

plot(
	transform(
		samp[, vs(samp)[-1]],
			v031_Trglcrd_LP_2571_8_num = log(v031_Trglcrd_LP_2571_8_num),
			v032_Prst_spcfc_2857_1_num = log(v032_Prst_spcfc_2857_1_num)
	),
pch = '.', col = '#0000FF10')




plot(transform(samp[, vs(samp)[-1]], v031_Trglcrd_LP_2571_8_num = log(v031_Trglcrd_LP_2571_8_num), v032_Prst_spcfc_2857_1_num = log(v032_Prst_spcfc_2857_1_num), v007_rctv_prtn_1988_5_num = log(v007_rctv_prtn_1988_5_num)), pch = '.', col = '#0000FF10')

# invert, val returns values instead of indexes
samp[ , grep('_unit$',vs(samp,'c'), invert = T, val = T)] <- sapply(samp[, grep('_unit$', vs(samp, 'c'), invert = T, val = T)], function(x) as.factor(x), simplify = FALSE)
samp$v003_Mlgnt_prst               <- !is.na(samp$v003_Mlgnt_prst)
samp$v003_Mlgnt_prst_inactive      <- !is.na(samp$v003_Mlgnt_prst_inactive)
samp$v024_gstrsphgl                <- !is.na(samp$v024_gstrsphgl)
samp$v024_gstrsphgl_inactive       <- !is.na(samp$v024_gstrsphgl_inactive)
samp$v031_ln_Trglcrd_LP_2571_8_num <- log(samp$v031_Trglcrd_LP_2571_8_num)
samp$v032_ln_Prst_spcfc_2857_1_num <- log(samp$v032_Prst_spcfc_2857_1_num)
samp$v007_ln_rctv_prtn_1988_5_num  <- log(samp$v007_rctv_prtn_1988_5_num)

write(names(samp), "names.csv")
samp_names <- read_csv("names.csv")

samp$Malignancy              <- as.factor(samp$Malignancy)
samp$`Malignancy Inactive`   <- as.factor(samp$`Malignancy Inactive`)
samp$v024_gstrsphgl          <- as.factor(samp$v024_gstrsphgl)
samp$v024_gstrsphgl_inactive <- as.factor(samp$v024_gstrsphgl_inactive)

non_log_columns <- c('PSA', 'C-reactive Protein', 'Triglycerides')
pairs(samp[, vs(samp[, !(names(samp) %in% non_log_columns)])[-1]], lower.panel = NULL, pch = ".", col = '#0000FF10')

save(samp, samp2, randompats, file = "survSave.rdata")
shiny::runApp()

# 1. Convert the diagnoses columns to TRUE/FALSE with !is.na(...)
# 2. Log transform v031_Trglcrd_LP_2571_8_num, v032_Prst_spcfc_2857_1_num, v007_rctv_prtn_1988_5_num
# 3. If you haven't already, `for(ii in grep('_unit$',vs(samp,'c'),invert = T,val=T)) samp[[ii]] <- factor(samp[[ii]])`
# 4. Save this new samp (again and always, along with samp2 and randompats) to the survSave.rdata file, and run TABSIE
# 5. Save a nice screencap of the constellation plot, might look good in a poster. I'd also like to see it.
# 6. Ditto with the "Focused PCA" option enabled, once for each of the response variables, and I'd like to see that.
# 7. Save a final version of the scatter plot.

















































#