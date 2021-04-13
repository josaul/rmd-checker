# function 1:
# display debugging messages in R if local, 
# or in the console log if remote
debug_msg <- function(...) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  txt <- paste(...)
  if (is_local) {
    message(txt)
  } else {
    shinyjs::logjs(txt)
  }
}


# function 2:
#turns the code list into final code - need to move to a separate file eventually
code_bind <- function(x) {
  
  #this extracts the first part before a # and puts all other parts in subsequent columns (this is tested for , % or + characters)
  new_table <- stringr::str_split(x$text, "\\#", simplify=TRUE) %>% as.tibble()
  
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
  code_summary <- x %>% filter(type=='code') %>% group_by(question) %>% summarise(total=paste(text, collapse=" "))
  
  if (length(new_table) != 0) { #not done if no comments
  comment_summary <- x %>% filter(type=='comment') %>% group_by(question) %>% summarise(total=paste('#',col, collapse=" ")) #this adds extra #if code line above
  }
  
  if (length(new_table) != 0) {
  summary <- full_join(code_summary, comment_summary, by='question')} else {
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
  
  return(summary)
}
