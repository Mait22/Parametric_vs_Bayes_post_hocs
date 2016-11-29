library(shiny)
library(dplyr)
library(markdown)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(scales)  
library(reshape2)
library(stringr)
library(stargazer)
library(Cairo)
library(datasets)

source("worker_functionsApp.R")




#HELPERS FOR PREDICTOR AND OUTCOM SLECTION
Predictor_col_finder <- function(df,down.crit = 2, up.crit = 8){
  
  col_names <- names(df)
  retur_results <- c()
  for(i in col_names){
    if(class(df[,i]) == "character" & length(unique(df[,i])) >= down.crit & length(unique(df[,i])) <= up.crit){
      retur_results[length(retur_results)+1] <- i
    }
  }
return(retur_results)  
}


Outcom_col_finder <- function(df){
  
  col_names <- names(df)
  retur_results <- c()
  for(i in col_names){
    if((class(df[,i]) == "numeric" | class(df[,i]) == "integer" )& length(df[,i]) > sum(is.na(df[,i]))){
      retur_results[length(retur_results)+1] <- i
    }
  }
  return(retur_results)  
}


#Pre-provided data factors
provided_data_factors <- list()
provided_data_outcomes <- list()
files <- dir(paste(getwd(),"/Data",sep = ""))
if(length(files) > 0){
  for(i in files){
    temp_data <- read.csv(paste(getwd(),"/Data/",i,sep = ""),header = TRUE,stringsAsFactors = FALSE)
    provided_data_factors[[i]] <- Predictor_col_finder(temp_data)
    provided_data_outcomes[[i]] <- Outcom_col_finder(temp_data)
  }
}





#Shiny server function
shinyServer(function(input,output,session){
  
  

  #DEFAULT UI
  output$data_selector <- renderUI({selectizeInput("data_selector_in","Select dataset to use:", 
                                                     choices = dir(paste(getwd(),"/Data",sep = "")),
                                                     multiple = FALSE, 
                                                     options = list(maxItems = 1),
                                                     selected = dir(paste(getwd(),"/Data",sep = ""))[1])})
  
  output$post_hoc_selector <- renderUI({selectizeInput("post_hoc_selector_in","Select parametric post hoc to use:",
                                                       choices = c("LSD.test", "HSD.test", "duncan.test"),
                                                       multiple = FALSE,
                                                       options = list(maxItems = 1),
                                                       selected = "LSD")})

  
  output$FAC_selector <- renderUI({selectizeInput("FAC_selector_in","Select independent variables:",
                                                  choices = provided_data_factors[[as.character(unlist(input$data_selector_in))]],
                                                  multiple = TRUE,
                                                  options = list(maxItems = 2),
                                                  selected = NULL)})

  
  output$VAL_selector <- renderUI({selectizeInput("VAL_selector_in","Select dependnet variable:",
                                                  choices = provided_data_outcomes[[as.character(unlist(input$data_selector_in))]],
                                                  multiple = FALSE,
                                                  options = list(maxItems = 1),
                                                  selected = NULL)})
  
  
  #DATA UPLOAD
  input_file <- reactive({
    input_file <- input$OwnData
    if (is.null(input_file)) {
      return(NULL)
    }
    read.csv(input_file$datapath,header = TRUE,stringsAsFactors = FALSE)
  })
  
  
  user.data <- observeEvent(input$upload,{
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Please wait! Adding data.Please be smart and vote for EKRE!", value = 0)
    if(is.null(input$OwnData) == TRUE){
      return(NULL)
    }  
    if(is.null(input$OwnData) == FALSE){
      file_name <- paste(gsub(x=input$user.dataset.name,pattern = ".",replacement = "",fixed = TRUE),".csv",sep = "")
      path <- paste(getwd(),"/Data/",file_name,sep = "")
      write.csv(input_file(),path,row.names = FALSE)
    }
  })
  
  
  user.data.factors <- eventReactive(input$upload,{})
  
  
  user.data.factors <- eventReactive(input$upload,{})

  
  
  #Updating UI elements
  observeEvent(input$upload,{

    updateSelectizeInput(session, "data_selector_in", label = "Select dataset to use:", choices = dir(paste(getwd(),"/Data",sep = "")),
                         selected = dir(paste(getwd(),"/Data",sep = ""))[1], options = list(maxItems = 1))
  
  })
  
  
  observeEvent(input$upload2,{

    provided_data_factors <- list()
    files <- dir(paste(getwd(),"/Data",sep = ""))
    if(length(files) > 0){
      for(i in files){
        temp_data <- read.csv(paste(getwd(),"/Data/",i,sep = ""),header = TRUE,stringsAsFactors = FALSE)
        provided_data_factors[[i]] <- Predictor_col_finder(temp_data)
      }
    }
    
    factors <- provided_data_factors[[as.character(unlist(input$data_selector_in))]]
    
    
    provided_data_outcomes <- list()
    files <- dir(paste(getwd(),"/Data",sep = ""))
    if(length(files) > 0){
      for(i in files){
        temp_data <- read.csv(paste(getwd(),"/Data/",i,sep = ""),header = TRUE,stringsAsFactors = FALSE)
        provided_data_outcomes[[i]] <- Outcom_col_finder(temp_data)
      }
    }
    
    vals <- provided_data_outcomes[[as.character(unlist(input$data_selector_in))]]
    

    updateSelectizeInput(session, "VAL_selector_in", label = "Select dependent variable:", choices = vals,
                         selected = NULL, options = list(maxItems = 1))
    
    updateSelectizeInput(session, "FAC_selector_in", label = "Select independent variables:", choices = factors,
                         selected = NULL, options = list(maxItems = 2))
    

    
  })
  
  
  
  
  
  
  dfc <- eventReactive(input$go,{
    file_name <- input$data_selector_in
    data <- read.csv(paste(getwd(),"/Data/",file_name,sep = ""),header = TRUE,stringsAsFactors = FALSE)
    return(data)
  })
  
  f1c <- eventReactive(input$go,{return(as.character(input$FAC_selector_in[1]))})
  f2c <- eventReactive(input$go,{
    if(length(input$FAC_selector_in)>1){
      return(as.character(input$FAC_selector_in[2]))
    }
    if(length(input$FAC_selector_in)  == 1){
      return(NULL)
    }
  })  
    
  varc <-  eventReactive(input$go,{return(as.character(input$VAL_selector_in))})
  
  
  output$debug1 <- renderText({dim(dfc())})
  output$debug2 <- renderText({(f1c())})
  output$debug3 <- renderText({(is.null(f2c()))})
  output$debug4 <- renderText({(varc())})
  
  
  
  aaa <- eventReactive(input$go,{
                       ac <- interaction.maker(df = dfc() ,factor1 = f1c(), factor2 = f2c(), value = varc())
                       aac <- post_hoc(ac, post.hoc.type = input$post_hoc_selector_in)
                       aaa <- post_hoc_plot(post.hoc.object = aac,interaction.object = ac, p.val.criteria = input$pvalue,BF.criteria = input$BF)
                       return(aaa)
  })                     


  
  
  
  
  
output$F1plotout <- renderPlot({aaa()@plot.f1})
output$F2plotout <- renderPlot({aaa()@plot.f2})  
output$Interplotout <- renderPlot({aaa()@plot.inter})  
  
  
  
  
  
  
  
  
})


  
  


  
# a <- interaction.maker(df = UserDataset2233 ,factor1 = "F1", factor2 = "F1", value = "VAL")
# aa
#   
#   
#   
# ac <- interaction.maker(df = UserDataset2233 ,factor1 = "F1", factor2 = "F2", value = "VAL", credvalue = 0.68)  
# aac <- post_hoc(ac, post.hoc.type = "LSD")  
#   
#   
# #   
# a <- interaction.maker(df = test_data,factor1 = "F1", factor2 = "F2",value = "VAL",credvalue = 0.68)
# aa <- post_hoc(a,post.hoc.type = "duncan.test")
# aaa <- post_hoc_plot(post.hoc.object = aa,interaction.object = a,p.val.criteria = 1,BF.criteria = 10)
#   
  
  
  
