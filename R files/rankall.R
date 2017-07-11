rankall<-function(outcome,num="best")
{
  #Read outcome data
  out<-read.csv("/home/robla/Desktop/Coursera/DataScience/ProgrammingAssignment3/outcome-of-care-measures.csv",stringsAsFactors = FALSE)
  
  #subset 'out' based on outcome
  if(outcome=="heart attack")
    sub<-out[,c(2,7,11)]
  else if(outcome=="heart failure")
    sub<-out[,c(2,7,17)]
  else if(outcome=="pneumonia")
    sub<-out[,c(2,7,23)]
  else
    stop("invalid outcome")
  
  #Rank hospital for each state
  colnames(sub)[3]="Mortality"
  sub$Mortality<-suppressWarnings(as.numeric(sub$Mortality))
  sub$Hospital.Name<-as.character(sub$Hospital.Name)
  sub<-na.omit(sub)
  
  library(stats)
  library(dplyr)
  rank<-sub %>%
    arrange(State, Mortality,Hospital.Name) %>%
    group_by(State) %>%
    mutate(my_ranks = order(Mortality, Hospital.Name))
  #View(rank)
  
  #Return a df with hospital name and state, given a rank
  if(num=="best")
    result<-rank[rank$my_ranks==1,c(1,2)]
    #View(result)
  else if(num=="worst")
    {result <- sub %>% 
    arrange(State, Mortality,Hospital.Name) %>%
    group_by(State) %>%
    mutate(my_ranks = order(Mortality, Hospital.Name))%>%
    slice(which.max(my_ranks))
    result<-result[,c(1,2)]
    #View(result)
    }
  else
    result<-rank[rank$my_ranks==num,c(1,2)]
    
}

#Test Run
View(tail(rankall("heart failure"), 10))
