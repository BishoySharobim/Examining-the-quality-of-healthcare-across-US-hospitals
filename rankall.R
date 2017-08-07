#Downloading and unzipping data
    {url <- "http://spark-public.s3.amazonaws.com/compdata/data/ProgAssignment3-data.zip"
    
    if(!file.exists("ProgAssignment3-data.zip")){
        download.file(url, "ProgAssignment3-data.zip")}
    
    if(!file.exists("Data")){
        unzip("ProgAssignment3-data.zip", exdir = "./Data")}}
    

    
#Reading and naming data and converting the mortality rates to numerical
    
    data <- read.csv("./Data/outcome-of-care-measures.csv", colClasses ="character", na.strings = "Not Available", stringsAsFactors = TRUE)
    
    
    names(data)[c(2,7,11, 17,23)] <- c("Hospital", "state", "heart attack", "heart failure", "pneumonia")
    
    
    for (i in c(11,17,23)){
        data[,i] <- as.numeric(data[,i])}

    

#Producing function
rankall <- function(outcome, num = "best"){

    
    #Taking complete cases based off outcome
    data1 <- data[which(complete.cases(data[outcome]) == TRUE),]
    
    
    #Splitting data according to state
    data2 <- split(data1, data1$"state")
    
    
    #Ordering observations in each state in ascending 
    #order of 30-day mortality rates (i.e. the topmost
    #observations have better mortality). If there is ties
    #order is decided alphabetically based off the
    #hospital name
    
    data3 <- lapply(data2, function(x){x[order(x[[outcome]], x[["Hospital"]]), "Hospital"]})
    
    

    #Using if and else statements to: (1) create invalid 
    #outcome error, (2) Dealing with the special case if num 
    #takes a  hospital ranking greater than the number of 
    #hospitals in a given state, (3) Allow for function to
    #read "best" and "worst" as valid values for 
    #hospital rankings.
    
    
    if (all(names(data) != outcome)){
        stop('invalid outcome')
        
    } else if(num == "worst"){
       
        a0 <- unlist(lapply(data3,function(x){x[length(x)]}))
        data.frame("hospital" = unname(a0), "state" = names(a0))
        
    } else if(num == "best"){
        
        a1 <- unlist(lapply(data3,function(x){x[1]}))
        data.frame("hospital" = unname(a1), "state" = names(a1))
        
    } else if(num > any(sapply(data3,function(x){length(x)}))){
        
        a3 <- unlist(lapply(data3,function(x){x[num]}))
        data.frame("hospital" = unname(a3), "state" = names(a3))
        
    } else{
        
        a2 <- unlist(lapply(data3,function(x){x[num]}))
        data.frame("hospital" = unname(a2), "state" = names(a2))
    }
    
}
    
