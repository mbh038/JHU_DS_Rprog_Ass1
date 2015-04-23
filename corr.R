corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  filenames <- list.files(directory, pattern="*.csv", full.names=TRUE)
  nfiles<-length(filenames)
  cor_vect<-numeric(length=nfiles)
  j<-1
  for (i in 1:nfiles){
    #add leading zeros if necessary to file names
    if(i<10){
      prepend<-"00"
    }else if (i >=10 && i<100){
      prepend<-"0"
    }else{
      prepend<-""
    }
    #read in file
    pdata<-read.csv(paste(directory,"/",prepend,i,".csv",sep=""))
    good<-complete.cases(pdata)
    if(sum(good)>threshold) {
    cor_vect[j]<-cor(pdata$sulfate[good],pdata$nitrate[good])
    j<-j+1
    }
  }
  length(cor_vect)<-j-1
  #cor_vect<-round(cor_vect,3)
  cor_vect
}