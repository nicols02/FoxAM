#run this app from GitHub by typing the commands:
#install.packages('shiny')
#install.packages('shinyMatrix')
#install.packages('ggplot2')
#install.packages('reshape2')
#shiny::runGitHub( "FoxAM", "nicols02")

#check if required packages are installed, if not, install and attach to the library
if (library(shiny,logical.return=TRUE)==FALSE) {
  install.packages("shiny")
  library("shiny")
}
if (library(shinyMatrix,logical.return=TRUE)==FALSE) {
  install.packages("shinyMatrix")
  library("shinyMatrix")
}
if (library(ggplot2,logical.return=TRUE)==FALSE) {
  install.packages("ggplot2")
  library("ggplot2")
}
if (library(reshape2,logical.return=TRUE)==FALSE) {
  install.packages("reshape2")
  library("reshape2")
}


# library(shiny)
# library(shinyMatrix)
# library(ggplot2)
# library(reshape2)

source("generate_transition_matrices.R", local=TRUE)#local=environment() )


######### SET UP THE DATA COLLECTION MATRICES ###################
#clean/empty matrix
specMatInit <- matrix(data=0, nrow= 6, ncol=2)
#prefilled for species threat (currently just made up for demonstration)
#specMatInit <- matrix(data= c(0.02, 0.3, 0.015, 0.35, 0.05, 0.25, 0.025, 0.3, 0.15, 0.1, 0.1, 0.15), nrow=6, ncol=2, byrow=TRUE)
specMatInit <- matrix(data= c(0.02, 0.8, 0.015, 0.8, 0.05, 0.25, 0.025, 0.3, 0.15, 0.02, 0.1, 0.01), nrow=6, ncol=2, byrow=TRUE)

colnames(specMatInit) <- c("P(Extinct)", "P(High)")
rownames(specMatInit) <- c("(Not Present, Low)",
                           "(Not Present, High)",
                           "(Low, Low)",
                           "(Low, High)",
                           "(High, Low)",
                           "(High, High)")

#clean/empty matrix
threatMat1 <- matrix(data=0, nrow=2, ncol=2) 
#prefilled for fox threat
threatMat1 <- matrix(data= c(0.875, 0.283333, 0.866667, 0.3), nrow=2, ncol=2) #values obtained from experts (AM_elicitation_combined.xlsx)
colnames(threatMat1) <- c("P(High|A0)", "P(High|A5)")
rownames(threatMat1) <- c("(Stable, High)", "(Increasing, High)")

#clean/empty matrix
threatMat2 <- matrix(data=0, nrow=2, ncol=6)
#prefilled for fox threat
threatMat2 <- matrix(data= c(0.433333333,	0.566666667,	0.666666667,	0.675, 0.816666667, 0.866666667,
                             0.425,	0.525,	0.616666667,	0.658333333,	0.791666667,	0.883333333),
                             nrow=2, ncol=6) #values obtained from experts (AM_elicitation_combined.xlsx)
colnames(threatMat2) <- c("P(Low|A0)", "P(Low|A1)","P(Low|A2)","P(Low|A3)","P(Low|A4)","P(Low|A5)")
rownames(threatMat2) <- c("(Stable, Low)", "(Decreasing, Low)")


#add some variables-- we can make these reactive later
benefitRatio= c(-20,0,0)
n.Foxmodels <- 9
n.Speciesmodels <- 3
foxModel.names <- paste("F",1:n.Foxmodels, sep="")
speciesModel.names <- paste("S",1:n.Speciesmodels, sep="")

n.actions <- 6
actions.list <- c( "do_nothing", sapply(1:(n.actions-1), function(i) paste("a",i, sep="")))

#############################################################
#Build UI#
ui <- fluidPage(
  titlePanel("Universal Adaptive Management"),
  sidebarLayout(
    sidebarPanel( width= 6,
      div(style="display: inline-block;vertical-align:top; width: 150px;",numericInput("maxT", "Length of simulation",value=20,min=1, max=NA, step=1)),
      div(style="display: inline-block;vertical-align:top; width: 180px;",numericInput("nSims", "Number of simulations",value=30,min=1, max=NA, step=1)),
      br(),            #line break
      #numericInput("maxT", "Length of simulation",value=20,min=1, max=NA, step=1, width= '100px'),
      #numericInput("nSims", "Number of simulations",value=30,min=1, max=NA, step=1, width= '100px'),
      
      strong("Threat Elicitation"),
      #elicit threat information
      matrixInput("threatMat1a",class="numeric",
                  value= threatMat1,
                  rows= list(names = TRUE),
                  cols= list(names= TRUE),
                  copy = TRUE,
                  paste = TRUE),
      
      matrixInput("threatMat2a",class="numeric",
                  value= threatMat2,
                  rows= list(names = TRUE),
                  cols= list(names= TRUE),
                  copy = TRUE,
                  paste = TRUE),
      #elicit species information
      strong("Species Elicitation"),
      matrixInput("SpeciesMat",class="numeric",
                  value= specMatInit,
                  rows= list(names = TRUE),
                  cols= list(names= TRUE),
                  copy = TRUE,
                  paste = TRUE),
      

      radioButtons("foxModLabel", "Fox Model", foxModel.names, selected = "F9", inline = TRUE),
      radioButtons("spModLabel", "Species Model", speciesModel.names, selected = "S2", inline=TRUE),
      
      checkboxGroupInput("actionLabel", "Simulated Actions", choices= actions.list,
                         selected = c("do_nothing", "a5"), width = NULL)
      
     # checkboxGroupInput(inputId, label, choices = NULL, selected = NULL,
       #                  inline = FALSE, width = NULL, choiceNames = NULL,
        #                 choiceValues = NULL)#radioButtons("actionLabel", "Simulated Actions", actions.list, selected = "a5", inline=TRUE),
      
      ), #end sidebarpanel
    


    mainPanel(width= 6,
      #plotOutput("SimPlot"),
      plotOutput("SimPlot2")
    )
  )
)

##########################################
#write server#
server <- function(input, output) {
  
  df_all <- reactive({
    true.model <- c(input$foxModLabel, input$spModLabel)
    initialState <- c("HighF", "LowSp", "HighF", true.model[1], true.model[2])
    
    Transition.matrices <- get.transition(input$SpeciesMat, input$threatMat1a, input$threatMat2a)
    #create the df for ggplot
    
    
    prepare.plot(benefitRatio, input$nSims, input$maxT, true.model, initialState,Transition.matrices,input$actionLabel)
  })
  
  output$SimPlot2 <- renderPlot({
    true.model <- c(input$foxModLabel, input$spModLabel)
    initialState <- c("HighF", "LowSp", "HighF", true.model[1], true.model[2])
    
    varnames <- c("fox_0", "species_0", "foxPrev_0","foxModel_0", "speciesModel_0", "reward")
    #make a dummy dataframe containing the y limits for the plots
    ddummy <-  data.frame(time=1, series=rep(varnames[-(3:5)], each=2), 
                          value=c(rep(c(1:2,1,3), times=1), #fox_0 ranges from 1:2, species_0 from 1:3, ditto xxPrev_0 vars
                                  -20,0)) #limit on the reward (arbitrary lower bound, what should this be?)
    
    #plot simulation variables
    modelName <- paste("model",initialState[4],initialState[5], sep="_")
    plotdf.all <- ggplot(data= df_all(), aes(x=time)) +
      geom_line(aes(y=lower, group=action, colour= factor(action)), linetype="dashed", size=0.8) +
      geom_line(aes(y=upper,group=action, colour= factor(action)), linetype="dashed", size=0.8)+
      geom_line(aes(y=mean,group=action, colour= factor(action)), size=1.2) +
      geom_blank(data= ddummy, aes(x=time, y=value)) +
      facet_wrap(vars(series), scales="free_y", ncol=1)+
      ggtitle(modelName) +
      ylab("") +
      theme(plot.title = element_text(hjust = 0.5))  #centres the plot title
    plotdf.all <- plotdf.all+ labs(color='Action')
    print(plotdf.all)

  }, height = 1000, units="px")
}
###########################################
#call Shiny app#
shinyApp(ui = ui, server = server)