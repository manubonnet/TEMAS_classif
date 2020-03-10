#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("dplyr")

#remotes::install_github("juba/rainette",dependencies = T)
#install.packages("tidyr")

library(shiny)
library(rainette,attach.required = T)
library(quanteda)
library(miniUI)
options(shiny.maxRequestSize=1000*1024^2)
library("shinyWidgets")
library(stringr)
library(ggplot2)
library(purrr)
abcd <- "Waiting..."
class_nb <- 0
rainette_explor_css <- function() {
  "
#main {
  padding: 1em;
}
#side {
  background-color: #EEEEEE;
  padding: 2em 3em;
}
/* Syntax highlighting */
span.hl.str { color: #d14;}
span.hl.kwa { color: #099;}
span.hl.num { color: #099;}
span.hl.kwd { color: #333; font-weight: bold;}
span.hl.com { color: #888; font-style: italic;}
"
}


ui <- fluidPage(
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
  titlePanel("Clustering exploration"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(

      # Input: Select a file ----
      conditionalPanel("output.selected == '1'",
                       fluidRow(
                         column(10,h3("Step1: Import data")),
                         column(1,actionButton("re_1h", "",icon = icon("angle-double-up")),align="right")
                       ),
                       fileInput("file1", "Choose File",
                                 multiple = FALSE,
                                 accept = c("text/csv",
                                            "text/comma-separated-values,text/plain",
                                            ".csv")),
                       # Input: Select number of rows to display ----
                       actionButton(inputId = "sauve", label = "Save data")),
      conditionalPanel("output.selected == '1bis'",
                       fluidRow(
                         column(10,h3("Step1: Import data")),
                         column(1,actionButton("re_1", "",icon = icon("angle-double-down")),align="right")
                       )
      ),
      conditionalPanel("input.sauve & output.re_2 == '2'",                 
                       hr(),
                       fluidRow(
                         column(10,h3("Step2: Classification")),
                         column(1,actionButton("re_2h", "",icon = icon("angle-double-up")),align="right")
                       ),
                       actionButton(inputId = "test", label = "compute classificaction")
      ),
      conditionalPanel("output.re_2 == '2bis'",                 
                       hr(),
                       fluidRow(
                         column(10,h3("Step2: Classification")),
                         column(1,actionButton("re_2", "",icon = icon("angle-double-down")))
                       )
      ),
      conditionalPanel("input.test && output.re_3 == '3'",
                       hr(),
                       fluidRow(
                         column(10,h3("Step3: Classification parameters")),
                         column(1,actionButton("re_3h", "",icon = icon("angle-double-up")),align="right")
                       ),
                       # Horizontal line ----
                       sliderInput("k", label = "Number of clusters", 
                                   value = 4,
                                   min = 2, max = 6, step = 1),
                       
                       numericInput("n_terms", label = "Max number of terms to display",
                                    value = 20, min = 5, max = 30, step = 1),
                       checkboxInput("same_scales", label = "Force same scales", value = TRUE),
                       sliderInput("text_size", label = "Text size", value = 13, min = 6, max = 20, step = 1),
                       actionButton(inputId = "param", label = "Valid number of clusters"),
                       downloadButton("downloadPlot","Download graph")
      ),
      conditionalPanel("output.re_3 == '3bis'",
                       hr(),
                       fluidRow(
                         column(10,h3("Step3: Classification parameters")),
                         column(1,actionButton("re_3", "",icon = icon("angle-double-down")),align="right")
                       )
      ),
      conditionalPanel("input.param && output.re_4 == '4'",
                       hr(),
                       fluidRow(
                         column(10,h3("Step 4: Class and term selection")),
                         column(1,actionButton("re_4h", "",icon = icon("angle-double-up")),align="right")
                       ),
                       selectInput('in2', 'Select a class', class_nb, selectize=FALSE),
                       radioButtons("radio", label = h3("Risk factor type"),
                                    choices = list("Single-word risk factor (e.g. breastfeeding)" = 1, "Multiple-words risk factor (e.g. oral contaceptive)" = 2), 
                                    selected = 1),
                       conditionalPanel("input.radio==1",
                                        selectInput('in6', 'Select word(s)', abcd, multiple=TRUE, selectize=TRUE)),
                       conditionalPanel("input.radio==2",
                                        selectInput('in7', 'Main word (e.g. contraceptive)', c(Choose='', abcd) , multiple=F, selectize=TRUE),
                                        textInput("txt_comp", 'Complementary word (e.g. oral)', value = "", width = NULL,
                                                  placeholder = NULL),
                                        sliderInput("term_dist", label = "Maximum distance between Main and Complementary words \n (in number of characters)", value = 25, min = 5, max = 50, step = 5)),
                       actionButton(inputId = "extract", label = "Extraction")
      ),
      conditionalPanel("output.re_4 == '4bis'",
                       hr(),
                       fluidRow(
                         column(10,h3("Step 4: Class and term selection")),
                         column(1,actionButton("re_4", "",icon = icon("angle-double-down")),align="right")
                       )
                       ),
      conditionalPanel("input.extract",
                       hr(),
                       h3("Step 5:Download extracted database"),
                       h5("Number of articles extrated"),
                       textOutput("nb_articles"),
                       downloadButton("downloadData", "Download data") 
      ),           
      
      
      
    ),
    mainPanel(
      plotOutput("rainette_plot", height = "800px",width = "auto"),
      tableOutput("table2")
    )
  ))

server <- function(input, output, session) {
  
  output$selected <- renderText('1')
  outputOptions(output, "selected", suspendWhenHidden = FALSE)
  observeEvent(input$re_1,{
    output$selected <- renderText('1')
  })
  observeEvent(input$re_1h,{
    output$selected <- renderText('1bis')
  })
  output$re_2 <- renderText('2')
  outputOptions(output, "re_2", suspendWhenHidden = FALSE)
  observeEvent(input$re_2,{
    output$re_2 <- renderText('2')
  })
  observeEvent(input$re_2h,{
    output$re_2 <- renderText('2bis')
  })
  output$re_3 <- renderText('3')
  outputOptions(output, "re_3", suspendWhenHidden = FALSE)
  observeEvent(input$re_3,{
    output$re_3 <- renderText('3')
  })
  observeEvent(input$re_3h,{
    output$re_3 <- renderText('3bis')
  })
  
  output$re_4 <- renderText('4')
  outputOptions(output, "re_4", suspendWhenHidden = FALSE)
  observeEvent(input$re_4,{
    output$re_4 <- renderText('4')
  })
  observeEvent(input$re_4h,{
    output$re_4 <- renderText('4bis')
  })
  
  # get_groups <- function(res) {
  #   groups <- purrr::imap_dfc(res$uce_groups, ~ paste(.y, .x, sep="."))
  #   colnames(groups) <- seq_along(groups)
  #   return(groups)
  # }
  abcd = reactiveValues()
  abcd <- "Waiting..."
  data = reactiveValues()
  cond1 = reactiveValues()
  cond1 <- F
  cond2 = reactiveValues()
  cond2 <- 0
  
  observeEvent(input$sauve, {
    output$selected <- renderText('1bis')
    sendSweetAlert(
      session = session,
      btn_labels = NA,
      title = "Importing data...",
      text = "Please wait until \"Done !\" appears on your screen.",
      closeOnClickOutside = F,
      type = "warning"
    )
    data$table = data.table::fread(input$file1$datapath,
                                   sep = ";",
                                   quote = "\"",
                                   dec = ",",
                                   header = FALSE,
                                   fill = TRUE,
                                   stringsAsFactors = TRUE,
                                   integer64 = "numeric"
    )
    # data$table <- as.data.frame(data$table)
    data$table <- data$table[seq(2,nrow(data$table),by=2),]
    data$table <- cbind(as.factor(seq(1,nrow(data$table),by=1)),data$table)
    
    data$ira<-  import_corpus_iramuteq(input$file1$datapath,id_var = "abstract_")
    
    # Number of times we'll go through the loop
    
    sendSweetAlert(
      session = session,
      title = "Done !",
      text = "Le fichier a bien ete enregistre !",
      type = "success"
      
    ) 
    test <- head(data$table)
    output$table <- renderTable(test)
    output$nrow <- renderText(nrow(data$table))
  })
  
  observeEvent(input$test, {
    output$re_2 <- renderText('2bis')
    sendSweetAlert(
      session = session,
      btn_labels = NA,
      title = "Computation in progress...",
      text = "Please wait until \"Done !\" appears on your screen.",
      closeOnClickOutside = F,
      type = "warning"
    )
    data$dtm2 <- tokens(data$ira,what = "word",remove_numbers = T,
                        remove_punct = T)
    data$dtm1 <- tokens_remove(data$dtm2 , "\\p{Z}", valuetype = "regex")
    data$dtm <- dfm(data$dtm1, remove = stopwords("en"), tolower = TRUE, remove_punct = T,remove_numbers = T,remove_separators = TRUE)
    data$dtm <- dfm_wordstem(data$dtm, language = "english")
    data$dtm <- dfm_trim(data$dtm,min_termfreq = 20)
    
    data$res <- rainette2(data$dtm, uc_size1 = 10, uc_size2 = 15, max_k = 6, min_members = 50)
    res = reactiveValues()
    res <- data$res
    dtm = reactiveValues()
    dtm <- data$dtm
    res_name <- deparse(substitute(res))
    dtm_name <- deparse(substitute(dtm))
    max_n_groups = 6
    sendSweetAlert(
      session = session,
      title = "Done !",
      text = "Clusterisation complete !",
      type = "success"
    )  
    
    plot_final <- reactive({rainette2_plot(res, dtm, n_terms = input$n_terms, k = input$k, criterion = "chi2", type = "bar",
                                           free_scales = !input$same_scales, measure = "chi2", show_negative = "FALSE", text_size = input$text_size)})
    output$rainette_plot <- renderPlot({
      plot_final()
    })
    output$downloadPlot <- downloadHandler(
      filename = function() { paste("test", '.pdf', sep='') },
      content = function(file) {
        ggsave(file, plot = plot_final(), device = "pdf")
      }
    )
    # 
    observeEvent(input$param, {
      output$re_3 <- renderText('3bis')
      groups <- reactive({cutree_rainette2(res, k = input$k)})
      range <- reactive({seq(1,input$k,1)})
      updateSelectInput(session,"in2",choices = range())
      word <- rainette_stats(groups(), dtm,n_terms = 800, show_negative = F, measure = "chi2")
      observe({
        if (input$in2 == 1) {
          abcd <- word[[1]][,1]
        }
        if (input$in2 == 2) {
          abcd <- word[[2]][,1]
        }
        if (input$in2 == 3) {
          abcd <- word[[3]][,1]
        }
        if (input$in2 == 4) {
          abcd <- word[[4]][,1]
        }
        if (input$in2 == 5) {
          abcd <- word[[5]][,1]
        }
        if (input$in2 == 6) {
          abcd <- word[[6]][,1]
        }
        
        abcd <- sort(abcd)
        updateSelectInput(session,"in6",choices = abcd)
        updateSelectInput(session,"in7",choices = c(Choose='', abcd))
      })
    })
    observeEvent(input$extract, {
      output$re_4 <- renderText('4bis')
      sendSweetAlert(
        session = session,
        btn_labels = NA,
        title = "Extraction in progress...",
        text = "Please wait until \"Done !\" appears on your screen.",
        closeOnClickOutside = F,
        type = "warning"
      )
      if(input$radio == 1){
        groups <- cutree_rainette2(res, k = input$k)
        bons <- which(groups[-length(groups)]==input$in2)
        data$table_bon_group <- data$table[bons,]
        output$bons <- renderText(length(bons))
        data$table_bon_group <- as.data.frame(data$table_bon_group)
        # data$table_bon_group[,2] <- as.character(data$table_bon_group[,2])
        memory <- input$in6
        for (i in 1:length(input$in6)) {
          blabla <- str_detect(data$table_bon_group[,2] ,memory[i])
          data$table_bon_group <- cbind(data$table_bon_group,blabla)
        }
        data$table_bon_group <- cbind(data$table_bon_group,F)
        for (i in 1:length(input$in6)) {
          for (j in 1:nrow(data$table_bon_group)) {
            if (data$table_bon_group[j,(i+2)]){data$table_bon_group[j,(length(input$in6)+3)] <- T}
          }
        }
        data_final2 <- subset(data$table_bon_group,data$table_bon_group$F)
        data_final2 <- data_final2[,1:2]
      }
      if(input$radio == 2){
        groups <- cutree_rainette2(res, k = input$k)
        bons <- which(groups[-length(groups)]==input$in2)
        data$table_bon_group <- data$table[bons,]
        output$bons <- renderText(length(bons))
        data$table_bon_group <- as.data.frame(data$table_bon_group)
        # data$table_bon_group[,2] <- as.character(data$table_bon_group[,2])
        memory <- input$in7
        for (i in 1:length(input$in7)) {
          blabla <- str_detect(data$table_bon_group[,2] ,memory[i])
          data$table_bon_group <- cbind(data$table_bon_group,blabla)
        }
        data$table_bon_group <- cbind(data$table_bon_group,F)
        for (i in 1:length(input$in7)) {
          for (j in 1:nrow(data$table_bon_group)) {
            if (data$table_bon_group[j,(i+2)]){data$table_bon_group[j,(length(input$in7)+3)] <- T}
          }
        }
        txt_comp <- tolower(input$txt_comp)
        data_final2 <- subset(data$table_bon_group,data$table_bon_group$F)
        data_final2 <- data_final2[,1:2]
        dense <- str_locate_all(data_final2[,2],input$in7)
        nrow(dense[[1]])
        n <- nrow(data_final2)
        data_final2[,3] <- NA
        for(j in 1:n){
          test_dense <- F
          if(nrow(dense[[j]])!=0){
            k<-1
            while (test_dense == F & k <= nrow(dense[[j]])) {
              ss_dense <- str_sub(data_final2[,2][j],max(dense[[j]][k,1]-input$term_dist,1),dense[[j]][k,2]+input$term_dist)
              k <- k+1
              test_dense <- str_detect(ss_dense,txt_comp)
            }
          }
          
          data_final2[,3][j] <- test_dense
          
        }
        data_final2 <- subset(data_final2,data_final2[,3]==T)
        data_final2 <- data_final2[,1:2]
      }
      data_final <- cbind(str_split(data_final2[,2], "---", simplify = TRUE))
      output$table2 <- renderTable(data_final)
      sendSweetAlert(
        session = session,
        title = "Done !",
        text = "Extraction complete !",
        type = "success"
      )  
      date_jour <- str_sub(date(),start = 9,end = 10)
      date_mois <- str_sub(date(),start = 5,end = 7)
      date_annee <- str_sub(date(),start = 21,end = 24)
      date_heure <- str_c(str_sub(date(),start = 12,end = 13),"h", str_sub(date(),start = 15,end = 16))
      
      name_id <- str_c("shiny.classif_",memory[1],"_",date_jour,"_",date_mois, "_" , date_annee,"_" ,date_heure,".csv")
      output$nb_articles <- renderPrint(nrow(data_final))
      output$downloadData <- downloadHandler(
        filename = function() {
          name_id
        },
        content = function(file) {
          write.csv(data_final, file, row.names = FALSE)
        }
      )
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
