pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)

  #how many monitors?
  nv<-length(id)
  
  #set up vectors to store mean and number of good measurements in each monitor file
  means<-numeric(length=nv)
  ndata<-integer(length=nv)
  
  #loop through all monitor files specified in id
  for (i in 1:nv){
    #add leading zeros if necessary to file names
    if(id[i]<10){
      prepend<-"00"
    }else if (id[i] >=10 && id[i]<100){
      prepend<-"0"
    }else{
      prepend<-""
    }
    #read in file
    pdata<-read.csv(paste(directory,"/",prepend,id[i],".csv",sep=""))
    #idenfity missing values
    baddata<-is.na(pdata[,pollutant])
    #find and store mean and number of good values
    means[i]<-mean(pdata[,pollutant][!baddata])
    ndata[i]<-length(pdata[,pollutant][!baddata])
  }
  #identify monitor files with no good values
  badfiles<-is.na(means)
  #find and return weighted sum of means of a monitor files specified in id
  final=sum(means[!badfiles]*ndata[!badfiles]/sum(ndata[!badfiles]))
  final<-round(final,digits=3)
  final
}