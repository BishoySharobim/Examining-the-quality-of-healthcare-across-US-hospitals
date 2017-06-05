#Downloading and unzipping data
    {url <- "http://spark-public.s3.amazonaws.com/compdata/data/ProgAssignment3-data.zip"
    
    if(!file.exists("ProgAssignment3-data.zip")){
        download.file(url, "ProgAssignment3-data.zip")}
    
    if(!file.exists("Data")){
        unzip("ProgAssignment3-data.zip", "Data")}}


    
#Reading and naming data and converting the mortality rates to numerical
    data <- read.csv("./Data/outcome-of-care-measures.csv", colClasses ="character", na.strings = "Not Available", stringsAsFactors = TRUE)
    
    
    names(data)[c(2,7,11, 17,23)] <- c("Hospital", "state", "heart attack", "heart failure", "pneumonia")
    
    
    for (i in c(11,17,23)){
        data[,i] <- as.numeric(data[,i])}
    
    
    
#Producing function
rankhospital <- function(state, outcome, num = "best"){

    
    #Taking complete cases based off outcome
    data1 <- data[which(complete.cases(data[outcome]) == TRUE),]
    
    
    #Splitting data according to state
    data2 <- split(data1, data1$state)
    


    #Ordering observations in each state in ascending 
    #order of 30-day mortality rates (i.e. the topmost
    #observations have better mortality). If there is ties
    #order is decided alphabetically based off the
    #hospital name
    
    data3 <- lapply(data2, function(x){x[order(x[[outcome]], x[["Hospital"]]),"Hospital"]})
    
    

    #Creating invalid/outcome state error and NA for
    #invalid number entered if it exceeds the number of
    #hospitals present in the state. Also allowing for
    #function to read "best" and "worst" as valid values
    #for hospital rankings.
    
    if(all(names(data3) != state)){
        stop('invalid state')
        
    } else if (all(names(data) != outcome)){
        stop('invalid outcome')
        
    } else if(num == "worst"){
        data3[[state]][length(data3[[state]])]
        
    } else if (num == "best"){
        data3[data3[[state]][1]]}
    
      else if(num > length(data3[[state]])){
        NA
          
    } else{data3[[state]][num]}
    
}    