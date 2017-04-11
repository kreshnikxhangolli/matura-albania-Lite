remove(list=ls())

library(shiny)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(xtable)

##################################################
# call RDS
if (!all(is.element(c("container.rds","programs_list.rds"), list.files("./RDS")))) source("./F03_embed_lists.r")

container <- readRDS("./RDS/container.rds")
programs_list <- readRDS("./RDS/programs_list.rds")
uni_list <- names(programs_list)
first_program <- programs_list[[uni_list[1]]][1]
school_type_list <- container[[uni_list[1]]][[first_program]][["coeff"]][["school"]][["exam_name"]] %>% unique()
exam_type_list <- container[[uni_list[1]]][[first_program]][["coeff"]][["exams"]][["exam_name"]] %>% unique()

########
# function for calculating the percentile
get_percentile <- function(x, xo) length(x[x <= xo])/length(x)*100


##################################################
# UI application
ui <- fluidPage(
   
   titlePanel("matura.al - Calculate Chances of Admission in Public Unis' Programs"),
   
   sidebarLayout(
      sidebarPanel(
          sidebarLayout(
                sidebarPanel(
                    numericInput(inputId = "gpa",
                                      label = "GPA:",
                                      value = 4,
                                      min = 4,
                                      max = 11.3,
                                      step = 0.1)),
                mainPanel(
                    selectInput(inputId = "school_type",
                        label = "High school category",
                        choices = school_type_list,
                        selected = school_type_list[1]))
            
          ),
         
         splitLayout(
            numericInput(inputId = "mand_1_grade",
                        label = "Math grade:",
                        value = 4,
                        min = 4,
                        max = 10,
                        step = 0.1),
            numericInput(inputId = "mand_2_grade",
                        label = "Language grade:",
                        value = 4,
                        min = 4,
                        max = 10,
                        step = 0.1)
         ),
         splitLayout(
              selectInput(inputId = "exam_1_choice",
                       label = "First choice",
                       choices = exam_type_list,
                       selected = exam_type_list[1]),
              numericInput(inputId = "exam_1_grade",
                         label = "Grade",
                         value = 4,
                         min = 4,
                         max = 10,
                         step = 0.1)
         ),
         splitLayout(
              selectInput(inputId = "exam_2_choice",
                                 label = "Second choice",
                                 choices = exam_type_list,
                                 selected = exam_type_list[1]),
              numericInput(inputId = "exam_2_grade",
                          label = "Grade",
                          value = 4,
                          min = 4,
                          max = 10,
                          step = 0.1)
         ),
         selectInput(inputId = "selected_uni",
                     label = "Choose University",
                     choices = uni_list,
                     selected = uni_list[1]),
         
         uiOutput(outputId = "reac_programs"), 
         submitButton(text= "Caclulate chances")
          
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
          fluidRow(plotOutput("p_graph")),
          fluidRow(tableOutput("table_results"))
      )
   )
)

##################################################
# UI application
server <- function(input, output) {
  
 # pass_df <- reactive({})
  
  re_results <- reactive({
    container <- container[[input$selected_uni]][[input$selected_program]]
    container
  })
  
  re_coeff_school <- reactive({
    coeff_school <- re_results()[["coeff"]][["school"]][re_results()$coeff$school$exam_name ==input$school_type,][["exam_coeff"]]
    coeff_school
  })
  
  re_coeff_exam_1 <- reactive({
    coeff_exam_1 <- re_results()[["coeff"]][["exams"]][re_results()$coeff$exams$exam_name ==input$exam_1_choice,][["exam_coeff"]]
    coeff_exam_1
  })
  
  re_coeff_exam_2 <- reactive({
    coeff_exam_2 <- re_results()[["coeff"]][["exams"]][re_results()$coeff$exams$exam_name ==input$exam_2_choice,][["exam_coeff"]]
    coeff_exam_2
  })
  
  re_score <- reactive({
    scs <- (26*input$gpa + 20*(input$mand_1_grade+input$mand_2_grade))*re_coeff_school()*5 + 
                      15*(re_coeff_exam_1()*input$exam_1_grade+re_coeff_exam_2()*input$exam_2_grade)*5
    scs
  })
  
  # expressions are not simplified on purpose so that one distinguishes max values
  re_score_max <- reactive({
    scs_max <- (26*11.38 + 20*(10+10))*re_coeff_school()*5 + 
      15*(re_coeff_exam_1()*10+re_coeff_exam_2()*10)*5
    scs_max
  })

  re_percentile <- reactive({
    
    scores_df <- re_results()[["scores"]]
    unique_years <- scores_df[["year"]] %>% unique()
    
    perc_ls <- list()
    for (i in unique_years){
      perc_ls[[i]] <- scores_df[scores_df[["year"]]==i,][["st_points"]] %>% get_percentile(re_score())
    }
    perc_ls
  })
   
  re_table_results <- reactive({
    length_dim <- length(re_percentile())+2
    sort_years <- sort(names(re_percentile()))
    results <- rep(0, length_dim)
    row_names <- rep("",length_dim)
    
    results[1] <- re_score()
    results[2] <- re_score_max()
    row_names[1] <- "Your score:"
    row_names[2] <- "Max score:"
    
    for (i in (1:length(re_percentile()))) {
      results[i+2] <- re_percentile()[[sort_years[i]]]
      row_names[i+2] <- paste("Percentile",sort_years[i])
    }
    
    results <- as.matrix(results, nrow = length(re_results())+2)
    rownames(results) <- row_names
    results
  })
  
   output$reac_programs <- renderUI({
     selectInput(inputId = "selected_program",
                 label = "Choose program",
                 choices = programs_list[[input$selected_uni]],
                 selected = programs_list[[input$selected_uni]][1])
   })
   
   output$p_graph <- renderPlot({
     score_graphs <- re_results()[["graph"]]
     
     out_list <- list()
     for (i in names(score_graphs[["the_plots"]])){
        if (score_graphs[["length_density"]][[i]] > 1) {
          out_list[[i]] <- score_graphs[["the_plots"]][[i]] + 
            geom_ribbon(data=subset(score_graphs[["the_plots"]][[i]][["data"]], x < re_score()),aes(ymax=y),
                        ymin=0,fill="grey",colour=NA)
        } 
     }
     
     print(grid.arrange(grobs = out_list,ncol=3))
   })
   
   output$table_results <- renderTable(re_table_results(),rownames = TRUE, colnames = FALSE)
   
}

##################################################
# Run the application 
shinyApp(ui = ui, server = server)

