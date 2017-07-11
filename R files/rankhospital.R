rankhospital<-function(state,outcome,num){
  #Read outcome data
  out<-read.csv("/home/robla/Desktop/Coursera/DataScience/ProgrammingAssignment3/outcome-of-care-measures.csv",stringsAsFactors = FALSE)
  
  #Check if state is valid
  if(!(state %in% out[,7]))
    stop("invalid state")
  
  #subset 'out' based on outcome
  if(outcome=="heart attack")
    sub<-out[out$State==state,c(2,7,11)]
  else if(outcome=="heart failure")
    sub<-out[out$State==state,c(2,7,17)]
  else if(outcome=="pneumonia")
    sub<-out[out$State==state,c(2,7,23)]
  else
    stop("invalid outcome")
  
  #sub[,3]<-as.double(sub[,3])
  colnames(sub)[3]="Mortality"
  
  sub$Mortality<-suppressWarnings(as.numeric(sub$Mortality))
  sub$Hospital.Name<-as.character(sub$Hospital.Name)
  sub<-na.omit(sub)
  
  ord<-sub[order(sub[,3],sub[,1]),]


  View(ord)
  str(ord)
  
  if (num=="best")
    hosp<-ord[1,1]
  else if(num=="worst")
    hosp<-ord[nrow(ord),1]
  else if(num>nrow(ord))
    print("NA")
  else
    hosp<-ord[num,1]
  
    
  print(hosp)
  
}

rankhospital("NY", "heart attack", 7)

