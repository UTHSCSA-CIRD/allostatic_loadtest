
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