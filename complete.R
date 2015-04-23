complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  #how many monitors?
  nv<-length(id)
  
  #set up vectors to store mean and number of good measurements in each monitor file
  ids<-c(length=nv)
  nobs<-c(length=nv)
  
  
  ngood<-data.frame(ids,nobs)
  names(ngood)[names(ngood)=="ids"] <- "id"
  
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
   
   ngood[i,"id"]=id[i]
   ngood[i,"nobs"]<-sum(complete.cases(pdata))
         
  }
 
  print(ngood)
  
  
}