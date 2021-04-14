#this script converts a Rmd to an answer csv and an R file to upload

#need to change names at step 1, 2 and 3 (if needed) and then add marks vector at 4


#Jobs when converting a model answer to format:

# model answer: ensure initial chunk has purl=FALSE on all non-question chunks other than the csv/libraries chunk which needs 'initial'
# blank answer sheet: ensure all non-question chunks have purl=FALSE
# ensure all question chunks are tagged {r qXX} where X is a digit (use 0s if not a 2digit number or q7a q7b etc)
# ensure the questions in white space all start with Q and same number of questions as question chunks
# if some questions are worth more than others, make a vector of maximum scores
# include any notes or alternative code within the model answer chunk (I'm looking in to making this several chunks)
# make sure that the ## isn't showing in .R files (there is an extra line needed if so)
# tell students not to delete, rename or add chunks, and to write all answers in chunks (even code)

library(tidyverse)

#1. Turn the model answer into an R file- change name here
knitr::purl('Model_Answer.Rmd', documentation=3)

#2 Knit the model answer Rmd to get a list of questions - change name here
person <- readLines('Model_Answer.R') # this is all the lines

brk <- '## ----'

tib <- tibble(text=person, marker=NA) %>% mutate(marker=(substr(text,1,7)==brk)) #marks each line that is a chunk header
tib <- tib %>% filter(text!="") #remove blank lines
tib <- tib %>% mutate(subquestion=if_else(marker==TRUE,substr(text, 8,11), "")) #changed from 10
tib <- tib %>% mutate(question=if_else(marker==TRUE,substr(text, 8,10), "")) 
#need to add a subq here?

for (g in 1: nrow(tib)){
  tib$question[g] <- if_else(tib$marker[g]==FALSE, tib$question[max(1,g-1)], tib$question[g])
}

for (g in 1: nrow(tib)){
  tib$subquestion[g] <- if_else(tib$marker[g]==FALSE, tib$subquestion[max(1,g-1)], tib$subquestion[g])
}

tib <- tib %>% mutate(code=if_else(marker==FALSE, question,""), 
                      codesub=if_else(marker==FALSE, subquestion, ""))

tib <- tib %>% filter(marker==FALSE) %>% select(text, question, subquestion)

#all good up to here

#filter out the initial chunk
tib <- tib %>% filter(question!="ini") %>% filter(question!=" in")

#turns the code list into final code

x <- tib

  #this extracts the first part before a # and puts all other parts in subsequent columns (this is tested for , % or + characters)
  new_table <- stringr::str_split(x$text, "\\#", simplify=TRUE) %>% as.tibble()
  
  #problem: is putting comment and code on same line? code means code is present
  
  #mark according to code or comment
  new_table <- new_table %>% mutate(type=if_else(V1=="", 'comment', 'code'))
  
  #merge with table
  x <- cbind(x, new_table)
  
  #if no & there is another line, need ';' #
  x <- x %>% mutate(text=trimws(V1, which='right'), #remove white space after last char on line
                    nc=nchar(text)) %>% #calc number of chars on line
    mutate(mark=if_else(type=='code',if_else(substr(text, nc, nc) %in% c(",","%", "+"), 'fragment','no'), 'comment'),
           text=if_else(mark=='no', paste(text, ';', sep=''), text))
  
  new_table <- new_table %>% select(-c(V1, type)) #If there are no commented lines this breaks$$$$$$$$$
  cols<-colnames(new_table)
  
  #need to add all the commented code togehter (if there are any comment lines)
  #this needs fixing
  if (length(new_table) != 0) {new_table <- new_table %>% unite('col', cols, sep=" ")} #here is the break
  
  x <- cbind(x, new_table)
  
  #ADD all back together somehow
  code_summary <- x %>% filter(type=='code') %>% group_by(question, subquestion) %>% summarise(total=paste(text, collapse=" "))
  
  if (length(new_table) != 0) { #not done if no comments
    comment_summary <- x %>% filter(col!="") %>% group_by(question, subquestion) %>% summarise(total=paste('#',col, collapse=" ")) #this adds extra #if code line above
  }
  
  if (length(new_table) != 0) {
    summary <- full_join(code_summary, comment_summary, by=c('question', 'subquestion'))} else {
      summary <- code_summary
    }
  
  summary[is.na(summary)] <- "" #removes the NAs
  
  if (length(new_table) != 0) {
    summary <- summary %>% 
      unite('code_final', total.x:total.y, sep=" ") } else {
        
        summary <- summary %>% mutate(code_final=total) %>% select(-total)
      }
  
  summary <- summary %>%
    arrange(question)
  
#  return(summary)
#}


#TAKE IT OFF THE FUNCTION - SOMETHING WRONG WITH MULTILINE STUFF

y <- summary #this needs to reference the function code

y <- y %>% select(-subquestion)

#3. Reading in the questions - change name here
quest <- readLines('Model_Answer.Rmd')

tibble<- tibble(text=quest) %>% mutate(question=substr(text,1,1)) %>% #identify the Qs
  filter(question=="Q")


#make unique vector of questions and add them to tibble
tibble <- tibble %>% mutate(question=unique(y$question))

#4. Add marks
final <- full_join(y, tibble) %>% mutate(marks=1)
#marks will need to be a vector (I have writted it as all 1s)

#5. Add marker names (default is all one marker)
#final <- full_join(y, tibble) %>% mutate(marker_name="Marker1")

final <- final %>% select(question=question, question_text=text, answer=code_final, 
                          marks=marks) #, marker_name=marker_name) for later

#6. write the file - don't change this name but ensure it is in the main file
write.csv(final, 'Answer_Sheet.csv', row.names = FALSE )



