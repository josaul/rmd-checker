### Jo Saul's exam marking app ### current version

##notes about running this:

#warning do not refresh page as it clears the history

#right now to clear the list of scores you need to press refresh or reload app.

#to run it you need to save the exam attempts in the Exam Scripts folder 
#and the Model answer Rmd in the main folder. run answer_sheet_converter.R to create
#the R and csv files that you need from this.

#run back end script to convert to scores once downloaded all data 

#during data entry if you don't press ctrl enter, it won't save (i.e. press before you change questions)

#LIBRARIES (check if all are needed?) libs for other exams may need to be loaded in script? 

#master_dat is visible from the sandbox. not a big deal but to be aware of (could not remove from env)


## Nice to haves
#get the plots to hide in non-plot and vice versa for code output?

#move more parts to source files?

#add checks of chunk names being read in vs. a legal list?

#add something to shorten the candidate name or the column width in the table

#add a notes column so you can make notes about answers

# mutliple lines doesn't compute the first one it does if it is the last



#----LIBRARIES---- 

#you may need to add additional libraries if they are needed for the model answer script

library(shiny)
library(tidyverse)
library(DT)
library(shinydashboard)
library(knitr)
library(shinyjs)
library(broom)

#----FUNCTIONS----

source("R/func.R")
#this loads the function for transforming text and also the debug function

#----DATA----

#static data input into the app = Model answers and student answrs

# this runs the code to generate the objects assumed to be included in the Model answers
source("Model_Answer.R") # this file is saved in the main folder (has been converted from Rmd)

# this is the csv with the model answers saved in it (also converted from Rmd)
adfcsv <- read.csv('Answer_Sheet.csv', stringsAsFactors = FALSE) 

# data creation from student Rmd Files (need to turn all the Rmds into a dataframe) #####################

# Step 1: extract text from each R file

brk <- '## ----' #this is the text in the R files that precedes the chunk names
df <- data.frame() #starts with an empty dataframe for the loop below
files <- list.files(path='./ExamScripts', pattern = '*.Rmd', recursive = T) #creates list of all exam scripts

#--------this is where the big loop starts
for (h in 1:length(files)) {
  
  df_length <- nrow(df) #at 1 this is an empty df, each loop run adds the data for a new file
  
  knitr::purl(paste('./ExamScripts/',files[h], sep=""), documentation=1) #gets the file name for each Rmd
  
  Rfilename <- substr(files[h], 1, nchar(files[h])-2) # converts to a R file
  
  person <- readLines(Rfilename) # extracts all the text from that R file
  
  tib <- tibble(text=person, marker=NA) %>% mutate(marker=(substr(text,1,7)==brk)) #marks the lines that demarcate the questions
  tib <- tib %>% filter(text!="") #remove blank lines
  tib <- tib %>% mutate(question=if_else(marker==TRUE,substr(text, 8,10), "")) #extracts the question numbers
  
  #this line removes the ## at start of each line if this appears (not usually needed)
  #tib <- tib %>% mutate(text=substr(text, 3, nchar(text)))
  
  #Step 2: populate the question column
  
  for (g in 1: nrow(tib)){
    tib$question[g] <- if_else(tib$marker[g]==FALSE, tib$question[max(1,g-1)], tib$question[g])
  } #goes line by line to allocate questions to each line
  
  #Step 3: make a new column called code, for lines of code, labelled by question number 
  
  tib <- tib %>% mutate(code=if_else(marker==FALSE, question,""))
  
  #Step 4: remove the chunk labels and marker column
  
  tib <- tib %>% filter(marker==FALSE) %>% select(text, question)
  
  #Step 5: combine code
  
  y <- code_bind(tib) %>% mutate(person=files[h]) #find incomplete fragments, combine with ; combine comments all on one line)
  
  df <- bind_rows(df, y) #add this participant's data to the previous
  
  file.remove(Rfilename) #delete the R file
  
}

#--- end of loop

#name the columns
colnames(df) <- c('question', 'answer', 'person')

#arrange by question
df <- df %>% arrange(question)

#Create this object so that the input button for question can be populated
qs <- df %>% pull(question) %>% unique() # list of questions

#add an empty marks column
df$marks <- ""

#this has created the input df (all the exam answers)

#this process currently has no checks or debugging

######### end of data creation ########################################################################

#----TABS----

#main_tab ----

#this tab contains the model answers and editable student answers
main_tab <- tabItem(
  tabName = "main_tab",
  box(id = "answer panel", width=12, title = "Model Answers", collapsible = F,
      fluidRow(
        column(width=10, offset=2, textOutput("qtext"))
      ),
      fluidRow(
        column(width=2, 
               selectInput(inputId="qchoice",
                           label="Select Question",choices=as.character(qs), 
                           selected=as.character(qs[1]))
        ),
        column(10,
               DTOutput("Answers"))
      )
  ),
  box(id = "student responses", width=12, title = "Student Answers", collapsible = F,
      fluidRow(
        column(12,
               DTOutput("userTable"),
               style = "height:500px; overflow-y: scroll;")
      )
  )
)

# info_tab ----

#this tab contains instructions and will contain outputs
info_tab <- tabItem(
  tabName = "info_tab",
  h2("Instructions"),
  column(width=12, 
  p("Select the question you are going to mark, 
                 compare student answers to model answer and 
                 award marks in the 'marks' column by double clicking it.
                 You can use tab or arrows to go down the 
                  column and input marks."),
  p("When you are finished you press CTRL & ENTER to save the marks.
                 You can overwrite marks if change your mind. We advise
    that you 'save' entries in this way often."),
  p("If you do not enter a 0 it will think you haven't marked that question."),
  p("For the model answer, the code output will generate in the sidebar
                 (either text or plot depending on the code). If multiple
                options are possible you need to select each one to view it."),
  p("When you select the student code lines, their code will generate 
                in the sidebar too for comparison."),
  p("The 'sandbox' area of the sidebar is for copying/editing student code if
    you need to do so to check it or see if it works when modified."),
  p("The download button can be hit when you want to download your marks as a csv."),
  p("The plot below is a visual representation of progress, grey = marked items, 
      beige = not yet marked items. Black numbers are each person's avg score, blue numbers
         are each question's average score.")),

  
        box(id = "heatmap", width=12, title = "Summary of marking so far",
            plotOutput("heatmap"))
      )

#----UI----

ui <- dashboardPage(
  skin = "purple",
  
  #header houses the download button
  dashboardHeader(title = "Shiny checking app",
                  titleWidth = 500, #wider than normal sidebar
                  tags$li(class= "dropdown",
                          downloadButton("downloadData", "Download", class='dld-button'))
  ),
  
  #sidebar houses all the code outputs
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
                      .sidebar { height: 100vh; overflow-y: auto; }
                      " )
      )
    ),
    width = 500, #wider than normal sidebar
    sidebarMenu(
      id = "tabs",
      menuItem("Main", tabName = "main_tab",
               icon = icon("home")),
      menuItem("Info", tabName = "info_tab",
               icon = icon("info"))
    ),
    div(id="hide1",box(id = "mod_output", width=12, title = "Model Output",
        verbatimTextOutput("codeResultsA"))
    ),
    div(id="hide2", box(id = "student_output", width=12, title = "Student Output",
        verbatimTextOutput("codeResults"))
    ),
    div(id="hide3", box(id = "mod_plot", class="plot-box", width=12, title = "Model Plot",
        plotOutput("codePlotA", width="100%", height="100%"), # how much of the box is filled by the plot
        collapsible=T, collapsed=F
    )),
    div(id="hide4", box(id = "student_plot", class="plot-box", width=12, title = "Student Plot",
        plotOutput("codePlot", width="100%", height="100%"),
        collapsible=T, collapsed=F
    )),
    box(title='Sandbox',width=12,
        div(id="SB", textInput(inputId = "testcodeSB", 
                               label= "Sandbox", 
                               placeholder = "#copy and edit any code here to evaluate", 
                               width="400px")),
        tags$style(type="text/css", "#SB {color: black}")), #otherwise text is white
    
    box(width=12, id="SBresults", title="Sandbox Result",
        verbatimTextOutput("codeResultsSB"), collapsible=T),
    
    box(width=12,id="SBplot", class="plot-box", title="Sandbox Plot",
        plotOutput("codePlotSB", width="100%", height="100%"), collapsible=F, collapsed=F)
  ),   #end sidebar
  
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), # links to www/custom.css
      tags$script(src = "custom.js") # links to www/custom.js
    ),
    tabItems(
      main_tab,
      info_tab
    )
  ) # end body
) #end dbpage

#----SERVER----

server <- function(input, output, session){
  
  shinyEnv <- environment() #this is for the code to run in the sandbox etc with all the right objects
  
  sbEnv <- mget(ls()) #list of objects required by Answer Sheet. need to remove master_dat
  
  #----DATAFRAMES----
  
  adf <- data.frame(question=adfcsv %>% pull(question), answer=adfcsv %>% pull(answer), 
                    marks=adfcsv %>% pull(marks), 
                    stringsAsFactors = FALSE)
  
  #this is a static df but it is filtered

  dat <- data.frame(person=df%>%pull(person),
                    question=df%>%pull(question),
                    answer=df%>%pull(answer), 
                    marks=df%>%pull(marks), 
                    stringsAsFactors = FALSE)
  
  master_dat <- dat
  
  
  # RENDER TABLES -------
  
  output$qtext <- renderText(adfcsv %>% 
                               filter(question==input$qchoice) %>% 
                               pull(question_text) %>% 
                               pluck(1))
  
  output$Answers <- renderDataTable({
    adf %>% filter(question==input$qchoice)
  }, selection = 'single', #means you can only select one row at a time
      options = list(dom = 't'))
  
  
  output$userTable <- renderDataTable({
    dat1 <- master_dat %>% filter(question==input$qchoice) #? change from dat
    DT::datatable(isolate(dat1),
                  editable = list(target='column', disable=list(columns=1:3)), #here is where you lock unchanged columns
                  rownames = TRUE, #this has to match the value in observe
                  selection = 'single', #means you can only select one row at a time
                  options=list(dom='tp', 
                               pageLength = 100) #p for pagination, displays up to 100 rows
    )
  })
  
  
  
  ###Tracking Changes### this is the updating part
  
  rvs <- reactiveValues(
    data = NA #dynamic data object
  )
  
  # this renders the first instance of data and re-renders with each filter move
  observe({
    dat1 <- master_dat %>% filter(question==input$qchoice) 
    rvs$data <- dat1
    print('observe1 happened')
  })
  
  proxy = dataTableProxy('userTable') #mean the version you have edited
  
  #also occurs each time a filter occurs
  observe({
    DT::replaceData(proxy, rvs$data, rownames = TRUE, resetPaging = FALSE) #rownames must match output
    print('observe2 happened')
  })
  
  #fires when you change menu tabs (i.e. choose info tab to view the graph output)
  observeEvent(input$tabs, {
    #changes master_dat for the graph
  
    output$heatmap <- renderPlot({
      
      lastq <- "qtotal"
      firstp <- master_dat$person[1]
      
      data_person <- master_dat %>% group_by(person) %>% mutate(marks=as.numeric(marks)) %>%
        summarise(mean_marks=mean(marks, na.rm=T) %>% round(1)) %>% mutate(q=lastq)
      
      is.nan.data.frame <- function(x) #don'tknow how this works but it allows the line below to work
        do.call(cbind, lapply(x, is.nan))
      data_person[is.nan(data_person)] <- NA #turns NaNs into NAs
  
      data_q <- master_dat %>% group_by(question) %>% mutate(marks=as.numeric(marks)) %>% 
        summarise(mean_marks=mean(marks, na.rm=T) %>% round(1)) %>% mutate(person=firstp)
      data_q[is.nan(data_q)] <- NA #turns NaNs into NAs
      
      master_dat <- master_dat %>% mutate(done=if_else(marks=="", 0, 1) %>% as.factor())    
      
      #plots a grid of which marks been done and averages
      ggplot(master_dat, aes(x = question, y = person, fill = done)) +
        geom_tile(color='white') + theme_classic() + 
        scale_fill_manual(values=c('bisque3', 'azure3')) + 
        theme(legend.position='none') + 
        annotate(geom="text", x=lastq, y=data_person$person, label=data_person$mean_marks,
                 color="black") +
        annotate(geom="text", x=data_q$question, y=data_q$person, label=data_q$mean_marks,
                 color="blue") + labs(x="Question", y="Student")
      
    
    
  })
    
  })
  
  #updates each edit
  observeEvent(input$userTable_cell_edit, {
    print("an edit occured")
    rvs$data <<- editData(rvs$data, input$userTable_cell_edit, rownames = TRUE) #rownames must match output
    master_dat <<- full_join(master_dat, rvs$data, by=c('person', 'question', 'answer')) %>%
      mutate(marks=if_else(is.na(marks.y),marks.x, marks.y)) %>%
      select(-c(marks.x, marks.y))
    
    print(master_dat %>% select(-answer) %>% filter(marks!=""))
    
  })
  
  #this section evaluates the students answer code
  observeEvent(input$userTable_rows_selected, {
    sel <- input$userTable_rows_selected #sel is the row index number
    codeInput <- reactive({ rvs$data[sel,3] }) #column 3 contains code
    output$codeResults <- renderPrint({
      eval(parse(text=codeInput()), envir=shinyEnv)})
    output$codePlot <- renderPlot({
      eval(parse(text=codeInput()), 
           envir=shinyEnv)}) #weight and height here fix the plot size (can be larger than box)
    
    if(is.null(eval(parse(text=codeInput()), envir=shinyEnv))){
      print("hide")
      shinyjs::hide(id = "hide2") #hiding only works with divs not boxes!
      shinyjs::hide(id = "hide4")
    }else{
      print("show")
      shinyjs::show(id = "hide2") 
      shinyjs::show(id = "hide4")
    }
    
    
  })
  
  observeEvent(input$qchoice, {# need this one to select default to 1
    print('selection set to 1')
    codeInputA <- reactive({adf %>% filter(question==input$qchoice) %>% pull(answer) %>% pluck(1)})
    
    output$codeResultsA <- renderPrint({
      eval(parse(text=codeInputA()), envir=shinyEnv) #for code with text output
      
    })
    
    output$codePlotA <- renderPlot({
      eval(parse(text=codeInputA()), 
           envir=shinyEnv) #for code wiht graphic output
    })
    
    #this tests if code returns NULL or not, to try to hide pane             
    if(is.null(eval(parse(text=codeInputA()), envir=shinyEnv))){
      print("hide")
      shinyjs::hide(id = "hide1") #hiding only works with divs not boxes!
      shinyjs::hide(id = "hide3")
    }else{
      print("show")
      shinyjs::show(id = "hide1") 
      shinyjs::show(id = "hide3")
    }

  })
  
  # #this evaluates the model answer code
  observeEvent(input$Answers_rows_selected, {# need this one to change according to selection
    selA <- input$Answers_rows_selected #sel is the row index number
    print('model answer row selected')
    codeInputA <- reactive({adf %>% filter(question==input$qchoice) %>% pull(answer) %>% pluck(max(1, selA, na.rm=TRUE))})

    output$codeResultsA <- renderPrint({
      eval(parse(text=codeInputA()), envir=shinyEnv) #for code with text output

    })

    output$codePlotA <- renderPlot({
      eval(parse(text=codeInputA()),
           envir=shinyEnv) #for code wiht graphic output
    })
    
    print(selA)

  })
  

  
  # Download a csv
  output$downloadData <- downloadHandler(
    
    filename=paste('marking_file', Sys.Date(), '.csv', sep=""), 
    content = function(file) {
      write.csv(master_dat,file, row.names = FALSE) 
    }
  )
  
  # SANDBOX FEATURE ---------
  codeInputSB <- reactive({ input$testcodeSB })
  
  output$codeResultsSB <- renderPrint({
    eval(parse(text=codeInputSB()), envir=sbEnv)}) 
  
  output$codePlotSB <- renderPlot({
    eval(parse(text=codeInputSB()), envir=sbEnv)})
  
  
} #end of server

shinyApp(ui = ui, server = server)

