#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("dplyr")
#install.packages("plotly")
#install.packages("DT")
#install.packages("shinycssloaders")
#install.packages("shinythemes")
#install.packages("knitr")
library(dashboardthemes)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinythemes)
library(tidyr)
library(dplyr)
library(DT)
library(bandit4abtest)
library(ggplot2)
library(plotly)
library(knitr)
library(tictoc)
# -------------------------------

rmdfiles <- c("Demo_Ctree_UCB.html")
sapply(rmdfiles, knit, quiet = T)


# Define UI ---------------
ui <- dashboardPage(skin = "blue",
  
  # Title
  dashboardHeader(title = "Bandit4abtest"),
  
  # Sidebar layout
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("hubspot"), startExpanded = TRUE,
               menuSubItem("Non Contextual", tabName = "non_contextual", icon = icon("bar-chart-o"), selected = TRUE),
               menuSubItem("Contextual", tabName = "contextual", icon = icon("bar-chart-o"))),
      menuItem("Import file", tabName = "import_file", icon = icon("file-import")),
      menuItem("Github", icon = icon("github"), href = "https://github.com/manuclaeys/bandit4abtest"),
      menuItem("CtreeUCB", tabName = "ctree", icon = icon("info-circle"))
    ),
    collapsed = TRUE
  ),
  
  # Dashboard Body ---
  dashboardBody(
    tabItems(
      
      # "Dashboard" tab content
      tabItem(tabName = "dashboard"),
      
      # 1st SubItem -- Non contextual
      tabItem(tabName = "non_contextual",
              fluidRow(
                column(
                  width = 5,
                  box(
                    solidHeader = F, width = NULL,
                    checkboxGroupInput("algo", "Select an algorithm",
                                       c()
                    ),
                    splitLayout(cellWidths = c("79%", "79%"),
                    actionButton("refresh_button", "Refresh plot", icon = icon("refresh")),
                    actionButton("submit_button", "Submit", icon = icon('paper-plane'),
                                 style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                  ),
                  
                  # Box input number of rounds and arms
                  box(
                    width = NULL,
                    numericInput(inputId = "rounds", "Number of rounds", value = 100),
                    sliderInput(inputId = "arms", label = "Number of arms", value = 2, min = 1, max = 10)
                  ),
                  
                  # Input for the distribution
                  box(
                    width = NULL,
                    selectInput(inputId = "distribution",
                                label = "Choose a distribution",
                                choices = list("Binomial" = "Binomial","Gaussian" = "Gaussian"))
                  )
                ),
                
                column(width = 7,
                       # TabPanel
                       box(height = "500px", width = NULL,
                           tabsetPanel(type = "tabs",
                                       tabPanel("Plot", plotlyOutput("plot_algo") %>% withSpinner(color="#0dc5c1")
                                       ),
                                       tabPanel("Details", column(width = 12, verbatimTextOutput("details"),
                                                                  style = "height:400px; overflow-y: scroll;overflow-x: scroll;")
                                       ),
                                       tabPanel("Time", column(width = 12, verbatimTextOutput("times"),
                                                                  style = "height:400px; overflow-y: scroll;overflow-x: scroll;")
                                       ),
                                       tabPanel("Rewards", column(width = 12, DT::dataTableOutput("rewards"),
                                                                  style = "height:400px; overflow-y: scroll;overflow-x: scroll;")
                                       )
                           )
                       ),
                       
                       # Create a box if the distribution is binomial or gaussian
                       uiOutput("proba_binom"),
                       uiOutput("proba_norm")
                )
              )
      ),
      
      # 2nd SubItem -- Contextual
      tabItem(tabName = "contextual",
              fluidRow(
                column(
                  width = 5,
                  box(
                    solidHeader = F, width = NULL,
                    checkboxGroupInput("algo_contextual", "Select an algorithm",
                                       c()
                    ),
                    actionButton("refresh_button_contextual", "Refresh plot", icon = icon("refresh"))
                  ),
                  
                  # Box input number of rounds and arms
                  box(
                    width = NULL,
                    numericInput(inputId = "rounds_contextual", "Number of rounds", value = 100),
                    sliderInput(inputId = "arms_contextual", label = "Number of arms", value = 2, min = 1, max = 10)
                  ),
                  
                  # Input for the distribution
                  box(
                    width = NULL,
                    selectInput(inputId = "distribution_contextual",
                                label = "Choose a distribution",
                                choices = list("Binomial" = "Binomial","Uniform (continus)" = "Uniform"))
                  )
                ),
                
                column(width = 7,
                       # TabPanel
                       box(height = "500px", width = NULL,
                           tabsetPanel(type = "tabs",
                                       tabPanel("Plot", plotlyOutput("plot_algo_contextual") %>% withSpinner(color="#0dc5c1")
                                       ),
                                       tabPanel("Details", column(width = 12, verbatimTextOutput("details_contextual"),
                                                                  style = "height:400px; overflow-y: scroll;overflow-x: scroll;")
                                       ),
                                       tabPanel("Time", column(width = 12, verbatimTextOutput("times_contextual"),
                                                                  style = "height:400px; overflow-y: scroll;overflow-x: scroll;")
                                       ),
                                       tabPanel("Rewards", column(width = 12, DT::dataTableOutput("rewards_contextual"),
                                                                  style = "height:400px; overflow-y: scroll;overflow-x: scroll;")
                                       )
                           )
                       ),
                       
                       # Create a box if the distribution is binomial or uniform
                       uiOutput("proba_binom_contextual"),
                       uiOutput("proba_unif_contextual"),
                       
                       # Create a box for contextual reward
                       uiOutput("contextual_reward")
                )
              )
      ),
      
      # TabItem -- Import File
      tabItem(tabName = "import_file",
        fluidRow(
        box(
              # Upload file
              fileInput("file1", "Choose CSV File",
                        multiple = TRUE,
                        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
              
              
              # Input: Checkbox if file has header ----
              checkboxInput("header", "Header", TRUE),
              
              # Input: Select separator ----
              radioButtons("sep", "Separator",
                           choices = c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "\t"),
                           selected = ","),
              
              # Input: Select quotes ----
              radioButtons("quote", "Quote",
                           choices = c(None = "",
                                       "Double Quote" = '"',
                                       "Single Quote" = "'"),
                           selected = '"'),
              
              
              # Input: Select number of rows to display ----
              radioButtons("disp", "Display",
                           choices = c(Head = "head",
                                       All = "all"),
                           selected = "head"),
              
        ),
      # Main panel for displaying outputs ----
        tableOutput("contents")
        )
      ),
      
      
      # TabItem -- About
      tabItem(tabName = "ctree",
              withMathJax(includeMarkdown("Demo_Ctree_UCB.md")))
    )
  )
)













# Define server logic ---------------
server <- function(input, output, session) {
  
  
  
##########  NON CONTEXTUAL PART ##########
  
  # If gaussian, hide Thompson Sampling
  observeEvent(input$distribution, {
    if(input$distribution == "Gaussian"){
      updateCheckboxGroupInput(session, "algo", "Select an algorithm",
                               c("UCB" = "UCB",
                                 "Epsilon Greedy" = "Epsilon Greedy",
                                 "EXP3" = "EXP3",
                                 "KL-UCB" = "KL-UCB",
                                 "Random" = "Random"))
    } else {
      updateCheckboxGroupInput(session, "algo", "Select an algorithm",
                               c("UCB" = "UCB",
                                 "Epsilon Greedy" = "Epsilon Greedy",
                                 "Thompson Sampling" = "Thompson Sampling",
                                 "EXP3" = "EXP3",
                                 "KL-UCB" = "KL-UCB",
                                 "Random" = "Random"))
    }
  })
  
  
  # User can choose the probability of success for each arm if the distribution is binomial
  output$proba_binom <- renderUI({
    if(input$distribution != "Binomial"){return()}
    box(
      title = "Select probability of success for each arm", width = NULL,
      lapply(seq(input$arms), function(j){
        column(width=3,
               lapply(1, function(i){
                 numericInput(inputId = paste0("prob_",j), label = paste0("Arm ",j),value = 0.1*j,
                              min = 0, max = 1, step = 0.1)  
               })
        )
      })
    )
  })
  # User can choose mean and variance for each arm if the distribution is gaussian
  output$proba_norm <- renderUI({
    if(input$distribution != "Gaussian"){return()}
    box(
      title = "Select mean and variance for each arm", width = NULL,
      lapply(seq(input$arms), function(j){
        column(width=3,
               lapply(1, function(i){
                 lapply(c("Mean", "Variance"), function(h){
                   numericInput(inputId = paste0(h,"_",j), label = paste0(h," Arm ",j), value = 1*j,
                                min = (if(h == "Mean"){-50} else {0}), # Variance > 0 
                                step = 1)
                 })
               })
        )
      })
    )
  })
  
  
  
  # Reactive rewards' dataframe
  visitor_reward <- reactive({ 
    set.seed(4434)
    df_r <- matrix(nrow = input$rounds, ncol = input$arms)
    sapply(seq(input$arms), function(j){
      sapply(seq(input$rounds), function(i){
        if (input$distribution == "Binomial"){
          df_r[i,j] <- rbinom(1, 1, input[[paste0("prob_",j)]])
        } else {
          df_r[i,j] <- rnorm(1, input[[paste0("Mean_",j)]], input[[paste0("Variance_",j)]])
        }
      })
    })
  })
  
  # Output rewards' dataframe
  output$rewards <- renderDataTable({
    datatable(visitor_reward(), options = list(paging = FALSE))
  })
  

######## Reactive alloc and cum_reg_alloc ######## 
  # UCB
  ucb_alloc  <- reactive({UCB(visitor_reward(),alpha = 1)})
  cum_reg_ucb_alloc  <- reactive({cumulativeRegretAverage(ucb_alloc()$choice, visitor_reward())})
  # Epsilon Greedy
  epsilonGreedy_alloc <- reactive({EpsilonGreedy(visitor_reward(), epsilon = 0.05)})
  cum_reg_epsilonGreedy_alloc  <- reactive({cumulativeRegretAverage(epsilonGreedy_alloc()$choice, visitor_reward())})
  # Thompson Sampling
  thompson_sampling_alloc <- reactive({
    if (input$distribution == "Gaussian"){return(NULL)
    } else {ThompsonSampling(visitor_reward())}
  })
  cum_reg_thompson_sampling_alloc <- reactive({
    if (input$distribution == "Gaussian"){return(NULL)
    } else {cumulativeRegretAverage(thompson_sampling_alloc()$choice, visitor_reward())}
  })
  # EXP3
  exp3_alloc <- reactive({EXP3(visitor_reward(), gamma=0.05)})
  cum_reg_exp3_alloc <- reactive({cumulativeRegretAverage(exp3_alloc()$choice, visitor_reward())})
  # KL-UCB
  klucb_alloc <- reactive({KLUCB(visitor_reward())})
  cum_reg_klucb_alloc <- reactive({cumulativeRegretAverage(klucb_alloc()$choice,visitor_reward())})
  # Random
  random_alloc <- reactive({UniformBandit(visitor_reward())})
  cum_reg_random_alloc <- reactive({cumulativeRegretAverage(random_alloc()$choice, visitor_reward())})
#################################################
  
  
  comp_reg <- reactive({
    if (input$distribution == "Binomial"){
      comp <- data.frame(1:input$rounds, cum_reg_ucb_alloc(), cum_reg_epsilonGreedy_alloc(),
                            cum_reg_thompson_sampling_alloc(), cum_reg_exp3_alloc(), cum_reg_klucb_alloc(), cum_reg_random_alloc())
    } else {
      comp <- data.frame(1:input$rounds, cum_reg_ucb_alloc(), cum_reg_epsilonGreedy_alloc(),
                            cum_reg_exp3_alloc(), cum_reg_klucb_alloc(), cum_reg_random_alloc())
    }
    comp
  })
  
  
  
  # Rename columns' name
  df_name_reg <- reactive({
    d <- comp_reg()
    if (input$distribution == "Binomial"){
      colnames(d) <- c("round", "UCB", "Epsilon Greedy", "Thompson Sampling", "EXP3", "KL-UCB", "Random")
    } else {
      colnames(d) <- c("round", "UCB", "Epsilon Greedy", "EXP3", "KL-UCB", "Random")
    }
    d
  })
  
  
  
  
  
  # Output plot
  observeEvent(input$refresh_button,
    {output$plot_algo <- renderPlotly({   
      validate(need(!is.null(input$algo), "Please select an algorithm to show a plot."))
      data <- gather(select(df_name_reg(), "round", input$algo), Algorithm, value, -round)
    
      # The plot (using ggplotly)
      ggplotly(
        ggplot(data,  aes(x=round, y = value, color = Algorithm)) +
          geom_line(linetype="dashed", size = 0.5) +
          scale_colour_manual(values =  c("UCB"="brown","Epsilon Greedy"="orange",
                                          "Thompson Sampling"="green","EXP3"="blue","KL-UCB"="pink", "Random"="black")) +
          xlab("Time T") +
          ylab("Cumulative regret")
      )
    })},
    ignoreNULL = F
)
  
  # Creating list for choices
  list_choices <- reactive({
    if (input$distribution == "Binomial"){
      l_choices <- list("UCB" = ucb_alloc()$S, "Epsilon Greedy" = epsilonGreedy_alloc()$S, 
                        "Thompson Sampling" = thompson_sampling_alloc()$S, "EXP3" = exp3_alloc()$S,
                        "KL-UCB"=klucb_alloc()$S,"Random" = random_alloc()$S)
    } else {
      l_choices <- list("UCB" = ucb_alloc()$S, "Epsilon Greedy" = epsilonGreedy_alloc()$S, 
                        "EXP3" = exp3_alloc()$S, "KL-UCB"=klucb_alloc()$S, "Random" = random_alloc()$S)
    }
    l_choices
  })
  

  # Output details : 
  output$details <- renderPrint({
    validate(need(!is.null(input$algo), "Please select an algorithm to show details."))
    list_choices()[input$algo]
  })
  
  
  # Creating list for times values
  list_times <- reactive({
    if (input$distribution == "Binomial"){
      l_times<- list("UCB" = ucb_alloc()$time, "Epsilon Greedy" = epsilonGreedy_alloc()$time, 
                     "Thompson Sampling" = thompson_sampling_alloc()$time, "EXP3" = exp3_alloc()$time,
                     "KL-UCB"=klucb_alloc()$time,"Random" = random_alloc()$time)
    } else {
      l_times <- list("UCB" = ucb_alloc()$time, "Epsilon Greedy" = epsilonGreedy_alloc()$time, 
                      "EXP3" = exp3_alloc()$time, "KL-UCB"=klucb_alloc()$time, "Random" = random_alloc()$time)
    }
    l_times
  })
  
  
  # Output times : 
  output$times <- renderPrint({
    validate(need(!is.null(input$algo), "Please select an algorithm to show times"))
    list_times()[input$algo]
  })
  
  
  
########## END NON CONTEXTUAL PART ##########
  
  
  
########## CONTEXTUAL PART ##########
  
  # If uniform continus, hide Thompson Sampling and TSLinUCB
  observeEvent(input$distribution_contextual, {
    if(input$distribution_contextual == "Uniform"){
      updateCheckboxGroupInput(session, "algo_contextual", "Select an algorithm",
                               c("UCB" = "UCB",
                                 "Epsilon Greedy" = "Epsilon Greedy",
                                 "EXP3" = "EXP3",
                                 "Random" = "Random",
                                 "LinUCB" = "LinUCB",
                                 "CtreeUCB" = "CtreeUCB"))
    } else {
      updateCheckboxGroupInput(session, "algo_contextual", "Select an algorithm",
                               c("UCB" = "UCB",
                                 "Epsilon Greedy" = "Epsilon Greedy",
                                 "Thompson Sampling" = "Thompson Sampling",
                                 "EXP3" = "EXP3",
                                 "Random" = "Random",
                                 "LinUCB" = "LinUCB",
                                 "TSLinUCB" = "TSLinUCB",
                                 "CtreeUCB" = "CtreeUCB"))
    }
  })
  
  
  # User can choose the probability of success for each arm if the distribution is binomial
  output$proba_binom_contextual <- renderUI({
    if(input$distribution_contextual != "Binomial"){return()}
    box(
      title = "Select probability of success for each arm", width = NULL,
      lapply(seq(input$arms_contextual), function(j){
        column(width=3,
               lapply(1, function(i){
                 numericInput(inputId = paste0("prob_contextual_",j), label = paste0("Arm ",j),value = 0.1*j,
                              min = 0, max = 1, step = 0.1)  
               })
        )
      })
    )
  })
  
  # User can choose minimum and maximum for each arm if the distribution is Uniform
  output$proba_unif_contextual <- renderUI({
    if(input$distribution_contextual != "Uniform"){return()}
    box(
      title = "Select min and max for each arm's Uniform distribution", width = NULL,
      lapply(seq(input$arms_contextual), function(j){
        column(width=3,
               lapply(1, function(i){
                 lapply(c("Min", "Max"), function(h){
                   numericInput(inputId = paste0(h,"_",j), label = paste0(h," for arm ",j), value = 0.5*j, step = 1)
                 })
               })
        )
      })
    )
  }) 
  
  output$contextual_reward <- renderUI({
    box(
      title = "Select contextual reward", width = NULL,
      lapply(seq(input$arms_contextual), function(j){
        column(width=3,
               lapply(1, function(i){
                 lapply(c("Min", "Max"), function(h){
                   numericInput(inputId = paste0(h,"_context_reward",j), label = paste0(h," for contextual reward ",j),
                                value = 0.7*j, step = 1)
                 })
               })
        )
      })
      
    )
  })
  
  
  # Contextual reward
  df_adv_rwd <- reactive({
    df_adv <- matrix(nrow = input$rounds_contextual, ncol = input$arms_contextual)
    sapply(seq(input$arms_contextual), function(j){
      sapply(seq(input$rounds_contextual), function(i){
          df_adv[i,j] <- runif(1, min = input[[paste0("Min_context_reward",j)]], max = input[[paste0("Max_context_reward",j)]])
      })
    })
  })
  
  # Strategy 
  df_arm_strat <- reactive({
    df_strat <- matrix(nrow = input$arms_contextual, ncol = input$arms_contextual)
    sapply(seq(input$arms_contextual), function(j){
      sapply(seq(input$arms_contextual), function(i){
          df_strat[i,j] <- round(runif(1, min = -1, max = 1), digits = 1)
      })
    })
  })
  
  # Arm reward
  visitorRwd <- reactive({ 
    set.seed(4434)
    df_vis_rew <- matrix(nrow = input$rounds_contextual, ncol = input$arms_contextual)
    sapply(seq(input$arms_contextual), function(j){
        if (input$distribution_contextual == "Binomial"){
            arm <- df_arm_strat()[,j]
            df_vis_rew[,j] <- vapply(1/(1+exp(-crossprod(t(df_adv_rwd()),arm))), function(x) rbinom(1, 1, x), as.integer(1L))
        } else {
          arm <- df_arm_strat()[,j]
          df_vis_rew[,j] <- crossprod(t(df_adv_rwd()),arm) + runif(input$rounds_contextual, input[[paste0("Min_",j)]], input[[paste0("Max_",j)]])
        }
      })
  })  
  
  
  # Output rewards' dataframe
  output$rewards_contextual <- renderDataTable({
    datatable(visitorRwd(), options = list(paging = FALSE))
  })
  
  # Create data frame
  df_adv_rewards <- reactive({
    as.data.frame(df_adv_rwd())
  })
  
  visitorReward <- reactive({
    as.data.frame(visitorRwd())
  })
  
  
  ######## Reactive alloc and cum_reg_alloc ######## 
  # UCB
  ucb_alloc1  <- reactive({ UCB(visitorReward() ,alpha = 1) })
  cum_reg_ucb_alloc1  <- reactive({ cumulativeRegretAverage(ucb_alloc1()$choice, visitorReward() ,dt = df_adv_rewards()) })
  # Epsilon Greedy
  epsilonGreedy_alloc1 <- reactive({ EpsilonGreedy(visitorReward(),epsilon  = 0.05) })
  cum_reg_epsilonGreedy_alloc1  <- reactive({ cumulativeRegretAverage(epsilonGreedy_alloc1()$choice, visitorReward(), dt = df_adv_rewards()) })
  # Thompson Sampling
  thompson_sampling_alloc1 <- reactive({
    if (input$distribution_contextual == "Uniform"){return(NULL)
    } else { ThompsonSampling(visitorReward()) }
  })
  cum_reg_thompson_sampling_alloc1 <- reactive({
    if (input$distribution_contextual == "Uniform"){return(NULL)
    } else { cumulativeRegretAverage(thompson_sampling_alloc1()$choice, visitorReward(), dt = df_adv_rewards()) }
  })
  # EXP3
  exp3_alloc1 <- reactive({ EXP3(visitorReward(), gamma=0.05) })
  cum_reg_exp3_alloc1 <- reactive({ cumulativeRegretAverage(exp3_alloc1()$choice, visitorReward(), dt = df_adv_rewards()) })
  # Random
  random_alloc1 <- reactive({ UniformBandit(visitorReward()) })
  cum_reg_random_alloc1 <- reactive({ cumulativeRegretAverage(random_alloc1()$choice, visitorReward(), dt = df_adv_rewards()) })
  # LinUCB
  linucb_contextual_alloc1 <- reactive({ LINUCB(df_adv_rewards(), visitorReward()) })
  cum_reg_linucb_contextual_alloc1 <- reactive({ cumulativeRegretAverage(linucb_contextual_alloc1()$choice, visitorReward(), dt = df_adv_rewards()) })
  # TSLinUCB
  thompson_sampling_contextual_alloc1 <- reactive({
    if (input$distribution_contextual == "Uniform"){return(NULL)
    } else { TSLINUCB(df_adv_rewards(), visitorReward()) }
  })
  cum_reg_thompson_sampling_contextual_alloc1 <- reactive({
    if (input$distribution_contextual == "Uniform"){return(NULL)
    } else { cumulativeRegretAverage(thompson_sampling_contextual_alloc1()$choice, visitorReward(), dt = df_adv_rewards()) }
  })
  # CtreeUCB
#  controle_param <-  reactive({ ctreeucb_parameters_control_default(dt = df_adv_rewards(), visitor_reward = visitorReward(), learn_size=1500,  alpha=1, ctree_control_val= partykit::ctree_control(teststat = "quadratic")) })
#  ctreeucb_alloc1 <- reactive({ ctreeucb(df_adv_rewards(), visitorReward(), ctree_parameters_control = controle_param()) })
#  cum_reg_ctreeucb_alloc1 <- reactive({ cumulativeRegret(ctreeucb_alloc1()$choice, visitorReward()) })
  
  
  
  ##############################################
  
  
  comp_reg1 <- reactive({
    if (input$distribution_contextual == "Binomial"){
      comp1 <- data.frame(1:input$rounds_contextual, cum_reg_ucb_alloc1(), cum_reg_epsilonGreedy_alloc1(),
                          cum_reg_thompson_sampling_alloc1(), cum_reg_exp3_alloc1(), cum_reg_random_alloc1(),
                          cum_reg_linucb_contextual_alloc1(), 
                          cum_reg_thompson_sampling_contextual_alloc1()
                          #cum_reg_ctreeucb_alloc1()
                          )
    } else {
      comp1 <- data.frame(1:input$rounds_contextual, cum_reg_ucb_alloc1(), cum_reg_epsilonGreedy_alloc1(), 
                          cum_reg_exp3_alloc1(), cum_reg_random_alloc1(), cum_reg_linucb_contextual_alloc1()
                          #cum_reg_ctreeucb_alloc1()
                          )
    }
    comp1
  })
  
  
  
  
  # Rename columns' name
  df_name_reg1 <- reactive({
    d1 <- comp_reg1()
    if (input$distribution_contextual == "Binomial"){
      colnames(d1) <- c("round", "UCB", "Epsilon Greedy", "Thompson Sampling", "EXP3", "Random", "LinUCB",
                        "TSLinUCB"
                       #"CtreeUCB"
                       )
    } else {
      colnames(d1) <- c("round", "UCB", "Epsilon Greedy", "EXP3", "Random", "LinUCB"
                       #"CtreeUCB"
                       )
    }
    d1
  })
  
  
  
  # Output plot
  observeEvent(input$refresh_button_contextual,
    output$plot_algo_contextual <- renderPlotly({   
      validate(need(!is.null(input$algo_contextual), "Please select an algorithm to show a plot."))
      data1 <- gather(select(df_name_reg1(), "round", input$algo_contextual), Algorithm, value, -round)
    
      # The plot (using ggplotly)
      ggplotly(
        ggplot(data1,  aes(x=round, y = value, color = Algorithm)) +
          geom_line(linetype="dashed", size = 0.5) +
          scale_colour_manual(values =  c("UCB"="brown","Epsilon Greedy"="orange",
                                          "Thompson Sampling"="green","EXP3"="blue","Random"="black",
                                          "LinUCB" = "red", "TSLinUCB" = "purple",
                                          "CtreeUCB" = "grey")) +
          xlab("Time T") +
          ylab("Cumulative regret")
      )
    }),
    ignoreNULL = F
  ) 
  
  
  
  list_choice1 <- reactive({
    if (input$distribution_contextual == "Binomial"){
      l_choice1 <- list("UCB" = ucb_alloc1()$S, "Epsilon Greedy" = epsilonGreedy_alloc1()$S,
                             "Thompson Sampling" = thompson_sampling_alloc1()$S, "EXP3" = exp3_alloc1()$S,
                             "Random" = random_alloc1()$S, "LinUCB" = linucb_contextual_alloc1()$S,
                             "TSLinUCB" = thompson_sampling_contextual_alloc1()$S
      )
    } else {
      l_choice1 <- list("UCB" = ucb_alloc1()$S, "Epsilon Greedy" = epsilonGreedy_alloc1()$S, "EXP3" =  exp3_alloc1()$S,
                             "Random" = random_alloc1()$S, "LinUCB" = linucb_contextual_alloc1()$S
      )
    }
    l_choice1
  })
  
  # Output contextual details : 
  output$details_contextual <- renderPrint({
    validate(need(!is.null(input$algo_contextual), "Please select an algorithm to show details."))
    list_choice1()[input$algo_contextual]
  })
  
  
  list_time1 <- reactive({
    if (input$distribution_contextual == "Binomial"){
      l_time1 <- list("UCB" = ucb_alloc1()$time, "Epsilon Greedy" = epsilonGreedy_alloc1()$time,
                        "Thompson Sampling" = thompson_sampling_alloc1()$time, "EXP3" = exp3_alloc1()$time,
                        "Random" = random_alloc1()$time, "LinUCB" = linucb_contextual_alloc1()$time,
                        "TSLinUCB" = thompson_sampling_contextual_alloc1()$time
      )
    } else {
      l_time1 <- list("UCB" = ucb_alloc1()$time, "Epsilon Greedy" = epsilonGreedy_alloc1()$time, "EXP3" =  exp3_alloc1()$time,
                        "Random" = random_alloc1()$time, "LinUCB" = linucb_contextual_alloc1()$time
      )
    }
    l_time1
  })
  
  # Output contextual details : 
  output$times_contextual <- renderPrint({
    validate(need(!is.null(input$algo_contextual), "Please select an algorithm to show times."))
    list_time1()[input$algo_contextual]
  })
  
  
  
########## END CONTEXTUAL PART ##########
  
  

  
######## UPLOAD FILE PART ###########
  
    output$contents <- renderTable({
      req(input$file1)
    
      dataset <- read.csv(input$file1$datapath,
                          header = input$header,
                          sep = input$sep,
                          quote = input$quote)
    
     if(input$disp == "head") {
        return(head(dataset))
      }
      else {
        return(dataset)
      }
    })  
  
####### END UPLOAD PART ##########
  
  
  
  
  
  
}




# Run the app ---------------
shinyApp(ui = ui, server = server)
