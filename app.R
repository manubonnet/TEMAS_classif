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
      h3("Step1: import data"),
      fileInput("file1", "Choose File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      # Input: Select number of rows to display ----
      actionButton(inputId = "sauve", label = "Save data"),
      conditionalPanel("input.sauve",
                       hr(),
                       h3("Step2: Classification"),
                       actionButton(inputId = "test", label = "compute classificaction")),
      conditionalPanel("input.test",
                       hr(),
                       h3("Step3: Classification parameters"),
                       # Horizontal line ----
                       sliderInput("k", label = "Number of clusters", 
                                   value = 4,
                                   min = 2, max = 6, step = 1),
                       
                       numericInput("n_terms", label = "Max number of terms to display",
                                    value = 20, min = 5, max = 30, step = 1),
                       checkboxInput("same_scales", label = "Force same scales", value = TRUE),
                       sliderInput("text_size", label = "Text size", value = 13, min = 6, max = 20, step = 1),
                       actionButton(inputId = "param", label = "Valid number of clusters"),
                       downloadButton("downloadPlot","Download graph"),
                       conditionalPanel("input.param",
                                        hr(),
                                        h3("Step 4: Class and term selection"),
                                        selectInput('in2', 'Select a class', class_nb, selectize=FALSE),
                                        selectInput('in6', 'Select word(s)', abcd, multiple=TRUE, selectize=TRUE),
                                        actionButton(inputId = "extract", label = "Extraction"),
                                        conditionalPanel("input.extract",
                                                         hr(),
                                                         h3("Step 5:Download extracted database"),
                                                         h5("Number of articles extrated"),
                                                         textOutput("nb_articles"),
                                                         downloadButton("downloadData", "Download data") 
                                        ),           
                       ),
                       
      )
    ),
    mainPanel(
      plotOutput("rainette_plot", height = "1000px"),
      tableOutput("table2")
    )
  ))

server <- function(input, output, session) {
  
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
  observeEvent(input$sauve, {
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
    data$dtm1 <- tokens_remove(data$dtm1 , "\\p{Z}", valuetype = "regex")
    data$dtm <- dfm(data$dtm1, remove = stopwords("en"), tolower = TRUE, remove_punct = T,remove_numbers = T,remove_separators = TRUE)
    data$dtm <- dfm_wordstem(data$dtm, language = "english")
    data$dtm <- dfm_trim(data$dtm,min_termfreq = 20)
    
    data$res <- rainette2(data$dtm, uc_size1 = 10, uc_size2 = 15, max_k = 6, min_members = 10)
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
      })
    })
    observeEvent(input$extract, {
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
      data_final <- subset(data$table_bon_group,data$table_bon_group$F)
      data_final <- data_final[,1:2]
      output$table2 <- renderTable(data_final)
      date_jour <- str_sub(date(),start = 9,end = 10)
      date_mois <- str_sub(date(),start = 5,end = 7)
      date_annee <- str_sub(date(),start = 21,end = 24)
      date_heure <- str_c(str_sub(date(),start = 12,end = 13),"h", str_sub(date(),start = 15,end = 16))
      
      name_id <- str_c("shiny.classif_",date_jour,"_",date_mois, "_" , date_annee,"_" ,date_heure,".csv")
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
