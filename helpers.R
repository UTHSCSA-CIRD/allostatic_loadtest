
vs <- function(xx
               ,type=c('numeric','factor','logical','character','binary','multinomial','time','date','dt','znumeric')
               ,ignorevs='ignorevs',...){
  # This function takes a data.frame and returns the names of columns of a 
  # particular type
  # xx        : data.frame
  # type      : string indicating what type of column names to return (the 
  #             non-standard ones are explained below)
  # ignorevs  : the name of the option setting where you store an optional 
  #             vector of column names that should never be returned (because 
  #             e.g. you know ahead of time that they are not useful for
  #             plotting or analysis)
  
  # first we define a TRUE/FALSE test function for selecting the columns
  # instead of a lot of if/else statements, we just have one switch() 
  # statement that is more readable. For more information, type ?switch
  # The first argument is match.arg, which matches the possibly partial
  # string the user typed to full argument. Think of tye `type` argument
  # to the vs() function as a multiple-choice question.
  test <- switch(match.arg(type)
                 ,'numeric'=is.numeric
                 ,'factor'=is.factor
                 ,'logical'=is.logical # i.e. all non-missing values in this column are TRUE/FALSE
                 ,'character'=is.character
                 ,'binary'=function(zz) length(unique(zz))==2
                 ,'multinomial'=function(zz) length(unique(zz))<length(zz)
                 ,'time'=function(zz) inherits(zz,'POSIXt')
                 ,'date'=function(zz) inherits(zz,'Date')
                 ,'dt'=function(zz) inherits(zz,c('Date','POSIXt')) # i.e. either date OR time
                 ,'znumeric'=function(zz) guessnum(zz,...)
  );
  # Then we apply the test function appropriate to the data type of to each 
  # column of xx using the `sapply()` function. What it returns, `matches` is a
  # vector of TRUE/FALSE (logical) values, with each having the same name as 
  # the column in xx that it refers to. If that column is the type being sought
  # it will have a value of TRUE, otherwise a value of FALSE.
  matches <- sapply(xx,test);
  # we return our final answer...
  return(
    setdiff( # the difference between
      # ...the names from `matches` where the corresponding value is `TRUE`
      names(matches)[matches]
      # ...and an optional environment variable that can be a vector of names
      # to ignore if they exist. To set this, you can do, e.g.
      # `option(ignorevs=c('patient_num','birth_date'))`
      ,getOption(ignorevs)));
}

guessnum <- function(xx,exclude='',returnval=F,tolerance=.11){
  xx <- xx[!is.na(xx)&!xx%in%exclude];
  out <- sum(is.na(as.numeric(as.character(xx))))/length(xx);
  if(returnval) out else out <= tolerance;
}

snu <- function(teststring = "Crtsl", df =  df1) {
  # Take a variable name summarize the number and unit columns, one on top of the other
  return(
    list(summary(df[, grepl(paste0(teststring, ".*num"), names(df))])
        ,summary(df[, grepl(paste0(teststring, ".*unit"), names(df))])
    )
  )
}


correct <- function(xx, range, conv){
# xx:              numeric vector of input data
# range:        biologically plausible range for that vector
# conv:          vector of unit conversion factors

   outliers <- findInterval(xx,range);
   # those below the range will be 0, those within, 1, and those above will be 2
   # for each value in xx whose corresponding value in outliers is not 1
   # try multiplying by conv and stop with the first one that gives a value within the range
   # replace the original element of xx with that value and move on to the next one
   # if none of them cause that element of xx fall wthin the normal range, 
   # replace that element in xx with NA
   # return(xx);
   holder <- xx %*%  t(conv)
   holder2 <- matrix(ifelse(findInterval(holder, range) != 1, NA, 1), ncol = length(conv))
   xx2 <- xx
   xx2[outliers != 1] <- NA
   choices <-matrix(holder*holder2, ncol = length(conv))
   for (ii in 1:length(xx2)) {
     for(iii in 1:length(conv)) {
       if (is.na(xx2[ii]) == TRUE) {
         if(is.na(choices[ii, iii]) == FALSE) xx2[ii] <- choices[ii, iii]
       }
      }
     }
   return(xx2)   
}


lastNonMissing <- function(xx,tfmethod=c('any','last'),listmethod=c('combine','last')){
  tfmethod<-match.arg(tfmethod);
  listmethod<-match.arg(listmethod);
  if(all(is.na(xx))) return(NA);
  if(is.logical(xx)) return(switch(tfmethod,
                                   any=any(xx),
                                   last=tail(na.omit(xx),1)));
  if('FALSE' %in% toupper(levels(factor(xx)))) return(switch(tfmethod,
                                                             any=any(toupper(na.omit(xx))!='FALSE'),
                                                             last=tail(na.omit(xx),1)));
  return(last(na.omit(xx)));
}

#' For data.frame `data` and column `name` (character) replace everything not 
#' between lthresh and uthresh (numerics) with NA
thresholdNA <- function(data, name, lthresh = -Inf, uthresh = Inf, envir){
  data <- as.character(substitute(data));
  if(missing(envir)) envir <- parent.frame();
  envir[[data]][[name]] <- ifelse(between(rawvals <- envir[[data]][[name]], lthresh, uthresh), rawvals, NA);
}

#' Return an indicator variable for events meeting the 
#' \code{selectfun} criterion or a series of ascending numbers ending at 
#' each event and restarting afterward, or both.
#' 
#' @param data A \code{data.frame} (required).
#' @param pattern A regular expression for identifying columns on which to run the test.
#' @param cnames If \code{pattern} is omitted, must be a vector of column names from the \code{data.frame}.
#' @param selectfun A function that operates on a vector and returns a single boolean value.
#' @param result What to return: ascending event-IDs, boolean indicators, or both (default).
#' @param returnDF Should the selected result be returned as part of the original \code{data.frame} (TRUE, default) or by itself?
findEvents <- function(data,pattern,
                       cnames=grep(pattern,names(data),val=T),
                       selectfun=function(xx) !all(is.na(xx)),
                       result=c('ids','indicators'),
                       returnDF=TRUE){
  indicators <- apply(data[,cnames],1,selectfun);
  ids <- c(0,cumsum(indicators))[1:length(indicators)];
  out <- data.frame(indicators,ids);
  if(returnDF) return(cbind(data,out[,result,drop=F]));
  return(out[,result]);
}


#' Parse \code{data} a vector of character variables in JSON format and return 
#' a vector of list objects.
#' @param data A character vector in JSON format.
jsonParse <- function(xx,encl=c('[',']'),...){
  # TODO: error checking to make sure it's actually JSON
  # Lazy way to create a vectorized JSON parser
  jpfn <- Vectorize(fromJSON,USE.NAMES=F);
  # Now use it, after catching missing values
  jpfn(paste0(encl[1],ifelse(is.na(xx),'null',as.character(xx)),encl[2]));
}

#' Iterate over a list of lists and from each list extract the element named by
#'\code{item} if it exists. Each result can be a vector. Return a vector or list.
#' @param xx List of lists, each itself containing at least one list-like object
#' @param item Character vector of length 1 saying what attribute to try to extract from each sub object of each list of \code{xx}
#' @param type If more than one sub-lists contain the attribute named by \code{item}, what to do? \code{last} (default) and \code{first} guarantee single-value per cell output. \code{unique} returns unique values and \code{all} returns all values.
dfListExtract <- function(xx,item,type=c('last','first','unique','all')){
  fn <- switch(match.arg(type),
               last = function(xx) tail(xx,1),
               first = function(xx) head(xx,1),
               unique = unique,
               all = identity
               );
  sapply(xx,function(yy) {
    out <-fn(unlist(lapply(yy,`[[`,item)));
    ifelse(is.null(out),NA,out);
    });
}

#' Take a vector of codes, sometimes more than one that are comma separated
#' Find all unique codes, and return a \code{data.frame} with a column of T/F
#' values for each code.
splitCodes <- function(xx,prefix='code_',...){
  # converts a string to factor and if already a factor trims off unused levels  
  xx <- factor(xx); 
  # find unique codes
  codes <- sort(unique(unlist(strsplit(levels(xx),','))));
  out <- data.frame(sapply(trimws(codes),
                           function(yy) factor(grepl(paste0('\\b',yy,'\\b'),as.character(xx))),simplify=F));
  names(out) <- gsub('^X',prefix,names(out));
  out;
}

#' Take a factor and a named vector of regexps, evaluate the
#' vector in the order given, and replace the matching levels
#' of the factor with the names of the corresponding regexps
#' Collapse all non-matching levels to 'N'
#' The defaults are currently for extracting valueflag info
mapLevels <- function(xx,
                      map=c(LH="'vf':\\['H'\\].*'vf':\\['L'\\]|'vf':\\['L'\\].*'vf':\\['H'\\]",L="'vf':\\['L'\\]",H="'vf':\\['H'\\]"),
                      other='N',exclude=NULL,...){
  xx <- factor(xx,exclude=exclude);
  stopifnot(is.character(map));
  nm <- names(map);
  if(length(unique(nm))<length(map)) {
    nm<-names(map)<-make_names(seq_along(map));
  }
  lv<-levels(xx);
  for(ii in nm) lv[grepl(map[ii],lv)] <- ii;
  lv[!grepl(paste0('^',nm,'$',collapse='|'),lv)]<-other;
  levels(xx) <- lv;
  xx;
}

#' Interactive reporting tool. The idea is to quickly review a large number
#' of automatically generated plots. Definitely too much hardcoding to the 
#' units and valueflags current case, but it's a start.
feedbackOMatic <- function(index,logobj,xpr,env=parent.frame(),...){
  lgo <-as.character(substitute(logobj));
  xpr <- substitute(xpr);
  if(is.null(env[[lgo]])) env[[lgo]]<-list();
  stopifnot(is.recursive(env[[lgo]]),is.atomic(index));
  env[[lgo]][[index]]<-list();
  env[['.vs']]<-origvars<-list(log=F,lim=NA);
  q1 <- q2 <- 0;
  while(trimws(q1)!=""){
    cat('If there are outliers preventing you from seeing the 
distribution, please type the number at which you would
like the corresponding scale to be cut off. Type "log"
to toggle log scale and reset to remove cutoffs. Hit 
carriage return to accept scaling and move on.
');
    eval(xpr);
    q1 <- readline('log, reset, or a numeric value. Blank line to move on: ');
    if(trimws(tolower(q1))=='log') {
      env[['.vs']][['log']] <- !env[['.vs']][['log']];
      env[['.vs']][['lim']] <- NA;
    } else {
      print('not log case');
      if(trimws(tolower(q1))=='reset'){
        env[['.vs']] <- origvars
      } else {
        if(!is.na(q1a<-as.numeric(q1))) {
          env[['.vs']][['lim']]<-q1a;
        }
      }
    }
    #print(env[['.vs']]);
  }
  env[[lgo]][[index]]$q1<-env[['.vs']];
  while(trimws(q2)!=""){
    cat('
Do some levels have negligible numers of values? Type 
their names each on a separate line and input an empty 
carriage return when you are done. 
');
    q2 <- readline('Name of group, blank line to move on: ');
    env[[lgo]][[index]]$q2 <- c(env[[lgo]][[index]]$q2,q2);
  }
  cat('
If the distributions are not roughly similar, please type "y" 
with an optional explanation. Otherwise send an empty carriage
return.
');
  env[[lgo]][[index]]$q3 <- readline('y or comment or blank line: ');
  #print(env[[lgo]][[index]]);
}
