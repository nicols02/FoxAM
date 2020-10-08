#run this app from GitHub by typing the commands:
#install.packages('shiny')
#install.packages('shinyMatrix')
#install.packages('ggplot2')
#install.packages('reshape2')
#shiny::runGitHub( "FoxAM", "nicols02")
#also needs sarsop installed

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
source("sarsop_parse_Shiny.R", local=TRUE)#local=environment() )


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
threatMat1 <- matrix(data=0, nrow=1, ncol=2) 
#prefilled for fox threat
threatMat1 <- matrix(data= c(0.875, 0.283333), nrow=1, ncol=2) #values obtained from experts (AM_elicitation_combined.xlsx)
colnames(threatMat1) <- c("P(High|A0)", "P(High|A3)")
rownames(threatMat1) <- c("High")

#clean/empty matrix
threatMat2 <- matrix(data=0, nrow=1, ncol=4)
#prefilled for fox threat
threatMat2 <- matrix(data= c(0.433333333,	0.666666667,	0.675, 0.866666667),
                     nrow=1, ncol=4) #values obtained from experts (AM_elicitation_combined.xlsx)
colnames(threatMat2) <- c("P(Low|A0)", "P(Low|A1)","P(Low|A2)","P(Low|A3)")
rownames(threatMat2) <- c("Low")

#add some variables-- we can make these reactive later

n.Foxmodels <- 5
n.Speciesmodels <- 3
foxModel.names <- paste("F",1:n.Foxmodels, sep="")
speciesModel.names <- paste("S",1:n.Speciesmodels, sep="")

n.actions <- 4
actions.list <- c( "do_nothing", sapply(1:(n.actions-1), function(i) paste("a",i, sep="")))

CostRatio= matrix(c(0,1,1.18,2.18), nrow=1) #cost of actions-- maybe make this reactive?
colnames(CostRatio) <- actions.list
rownames(CostRatio) <- "Action cost"

#specify initial belief
initThreatBel <- matrix(data=rep(1/n.Foxmodels, times= n.Foxmodels), nrow=1, ncol= n.Foxmodels) #assume initial belief is uniform for prefill, allow reactive later
initSpeciesBel <-  matrix(data=(1/n.Speciesmodels), nrow=1, ncol= n.Speciesmodels) 
colnames(initThreatBel) <- foxModel.names
colnames(initSpeciesBel) <- speciesModel.names

#shorten the initial belief values for input to the UI (allows reactive input without getting sum >1)
initSpeciesBel_short <- matrix(initSpeciesBel[,-length(initSpeciesBel)], nrow=1, ncol= (n.Speciesmodels-1))
colnames(initSpeciesBel_short) <- head(speciesModel.names,-1)

initThreatBel_short <- matrix(initThreatBel[,-length(initThreatBel)], nrow=1, ncol= (n.Foxmodels-1))
colnames(initThreatBel_short) <- head(foxModel.names,-1)

#############################################################
#Build UI#
ui <- fluidPage(
  
  navbarPage("Universal Adaptive Management",
  
    tabPanel("Simulate",
      sidebarLayout(
        sidebarPanel( width= 6,
          div(style="display: inline-block;vertical-align:top; width: 150px;",numericInput("maxT", "Length of simulation",value=20,min=1, max=NA, step=1)),
          div(style="display: inline-block;vertical-align:top; width: 180px;",numericInput("nSims", "Number of simulations",value=30,min=1, max=NA, step=1)),
          br(),            #line break
          div(style="display: inline-block;vertical-align:top; width: 150px;",numericInput("recoverProb", "Recovery Prob",value=0,min=0, max=1, step=0.1)),
          div(style="display: inline-block;vertical-align:top; width: 150px;",numericInput("costExt", "Cost of Extinction",value=20,min=0, max=NA, step=1)),
          br(),            #line break
          #numericInput("maxT", "Length of simulation",value=20,min=1, max=NA, step=1, width= '100px'),
          #numericInput("nSims", "Number of simulations",value=30,min=1, max=NA, step=1, width= '100px'),
          matrixInput("actionCost",class="numeric",
                      value= CostRatio,
                      rows= list(names = TRUE),
                      cols= list(names= TRUE),
                      copy = TRUE,
                      paste = TRUE),
          br(),
          
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
          
    
          radioButtons("foxModLabel", "Fox Model", foxModel.names, selected = "F5", inline = TRUE),
          radioButtons("spModLabel", "Species Model", speciesModel.names, selected = "S2", inline=TRUE),
          
          checkboxGroupInput("actionLabel", "Simulated Actions", choices= actions.list,
                             selected = c("do_nothing", "a3"), width = NULL),
          
          #reate action buttons for generating the pomdpx file and solving the POMDP with sarsop
          actionButton("getPOMDPX", "generate POMDPX file"),
          
          actionButton("solvePOMDP", "Solve the POMDP"),
          
         
          ), #end sidebarpanel
        
  
  
        mainPanel(width= 6,
                  plotOutput("SimPlot2", height= "1000px")
                  
        )
      ) #close tabPanel
    ), #close Navbar page
    
    tabPanel("Belief simulation",
             strong("Simulated belief plots"),
             br(), #add a new line
             textOutput("TrueModelReport"),
             plotOutput("SimPlot3", height= "300px"),
             br(), #add a new line
             br(),
             textOutput("threatBelTerminal"),
             plotOutput("SimPlot4", height= "300px"),
             br(), #add a new line
             br(),
             textOutput("speciesBelTerminal"),
             br(),
             strong("Action Selection Frequency during simulation"),
             plotOutput("SimPlot_action", height= "300px")
             ),
    
    tabPanel("Explore POMDP Solution",
             strong("Current state"),
             br(), #add a new line
             div(style="display: inline-block;vertical-align:top; width: 150px;",
                 radioButtons("threatState", "Fox State", choiceNames= list("Low", "High"), 
                              choiceValues= list("LowF", "HighF"), selected = "LowF", inline = FALSE)),
            
             div(style="display: inline-block;vertical-align:top; width: 180px;",
                  radioButtons("speciesState", "Species State", choiceNames= list("Loc. Extinct", "Low", "High"), 
                               choiceValues= list("LocExtSp", "LowSp", "HighSp"), selected = "LowSp", inline = FALSE)),
             br(),
             
             strong("Initial Belief: Threat models"),
             #elicit current belief information-- threat
             matrixInput("initThreatBel_shorta",class="numeric",
                         value= initThreatBel_short,
                         rows= list(names = TRUE),
                         cols= list(names= TRUE),
                         copy = TRUE,
                         paste = TRUE),
             
             textOutput("showBel1"),
             br(),
             
             strong("Initial Belief: Species models"),
             #elicit current belief information--species
             matrixInput("initSpeciesBel_shorta",class="numeric",
                         value= initSpeciesBel_short, #remove last value and compute reactively to ensure sum to 1
                         rows= list(names = TRUE),
                         cols= list(names= TRUE),
                         copy = TRUE,
                         paste = TRUE),
             
             textOutput("showBel2"),
             
             br(),
          actionButton("getOptimalAct", "Get Optimal Action"),
          textOutput("OptActionVal") ,

          
          strong("Next state"),
          br(), #add a new line
          div(style="display: inline-block;vertical-align:top; width: 150px;",
              radioButtons("threatState2", "Fox State", choiceNames= list("Low", "High"), 
                           choiceValues= list("LowF", "HighF"), inline = FALSE)),
          
          div(style="display: inline-block;vertical-align:top; width: 180px;",
              radioButtons("speciesState2", "Species State", choiceNames= list("Loc. Extinct", "Low", "High"),
                           choiceValues= list("LocExtSp","LowSp", "HighSp"), inline = FALSE)),
          br(),
          br(),
          textOutput("action.print"),
          textOutput("newBelief.th"),
          textOutput("newBelief.sp"),
          
          
          HTML(
            paste(
              h6("(Note that if you select a state where the species goes from Locally Extinct to extant (and have zero recovery probability), the belief will return NaN)")
            ) )
            
    ) #close tabpanel
              

      
    
  ) #close UI
)

##########################################
#write server#
server <- function(input, output, session) {
  
  df_all <- reactive({
    benefitRatio= c(-input$costExt,0,0)
    
    true.model <- c(input$foxModLabel, input$spModLabel)
    initialState <- c("HighF", "LowSp", true.model[1], true.model[2])
    
    Transition.matrices <- get.transition(input$SpeciesMat, input$threatMat1a, input$threatMat2a, input$recoverProb)
    #create the df for ggplot
    
    beliefSpecies <- c(input$initSpeciesBel_shorta, 1-sum(input$initSpeciesBel_shorta))
    beliefThreat <- c(input$initThreatBel_shorta, 1-sum(input$initThreatBel_shorta))
    longBel <- getLongFormatBelief(beliefThreat, beliefSpecies) #convert to long format belief
    
    prepare.plot(benefitRatio, input$actionCost, input$nSims, input$maxT, true.model, initialState,Transition.matrices,input$actionLabel, longBel)
  })
  
  df_belief <- reactive({
    true.model <- c(input$foxModLabel, input$spModLabel)
    initialState <- c("HighF", "LowSp", true.model[1], true.model[2])
    Transition.matrices <- get.transition(input$SpeciesMat, input$threatMat1a, input$threatMat2a, input$recoverProb)
    benefitRatio= c(-input$costExt,0,0)
    
    beliefSpecies <- c(input$initSpeciesBel_shorta, 1-sum(input$initSpeciesBel_shorta))
    beliefThreat <- c(input$initThreatBel_shorta, 1-sum(input$initThreatBel_shorta))
    longBel <- getLongFormatBelief(beliefThreat, beliefSpecies) #convert to long format belief
    benefitRatio <- c(-input$costExt,0,0)
    policy.filename <- paste("./pomdp_solved/ShinySolution_", paste(benefitRatio, collapse="_"),".policy", sep="")
    policy <- read.policy(policy.filename)
    
    simulate.MOMDP.belief(input$nSims, input$maxT, initialState, longBel, policy, Transition.matrices, benefitRatio)
      
  })
  
  df_simMOMDPactions <- reactive({
    benefitRatio= c(-input$costExt,0,0)
    
    true.model <- c(input$foxModLabel, input$spModLabel)
    initialState <- c("HighF", "LowSp", true.model[1], true.model[2])
    
    Transition.matrices <- get.transition(input$SpeciesMat, input$threatMat1a, input$threatMat2a, input$recoverProb)
  
    policy.filename <- paste("./pomdp_solved/ShinySolution_", paste(benefitRatio, collapse="_"),".policy", sep="")
    policy <- read.policy(policy.filename)
    
    beliefSpecies <- c(input$initSpeciesBel_shorta, 1-sum(input$initSpeciesBel_shorta))
    beliefThreat <- c(input$initThreatBel_shorta, 1-sum(input$initThreatBel_shorta))
    longBel <- getLongFormatBelief(beliefThreat, beliefSpecies) #convert to long format belief
    
    simulate.MOMDP(input$nSims, input$maxT, initialState, longBel, policy, Transition.matrices, benefitRatio)
    
  })
  
  #beliefSpeciesA <- reactive({input$initSpeciesBel_shorta})#, 1-sum(input$initSpeciesBel_shorta))})
  #beliefThreatA <- reactive({input$initThreatBel_shorta})#, 1-sum(input$initThreatBel_shorta))})
  
  #generate the POMDPX file reactively
  observeEvent(input$getPOMDPX, {
    print("starting writing POMDPX file")
    sarsop_parse(input$SpeciesMat, input$threatMat1a, input$threatMat2a, input$recoverProb, benefitRatio=c(-input$costExt,0,0), input$actionCost)
    print("finished writing to \"./pomdpx_files/filename\"")
  })
  
  #solve the POMDPX after creating the POMDPX
  observeEvent(input$solvePOMDP, {
    print("starting solving POMDP file: please wait")
    
    #input pomdpx always has the same name (output from sarsop_parse, called by POMDPX button)
    datfileName <- "./pomdpx_files/sarsop_input_ShinyGrab.pomdpx"
    
    #output the solution into the /pomdp_solved directory; name according to benefitRatio
    benefitRatio=c(-input$costExt,0,0)
    outfileName <- paste("./pomdp_solved/ShinySolution_", paste(benefitRatio, collapse="_"),".policy", sep="")
    
    #write an external command for sarsop to run and call it using system(cmd)
    precision <- 1e-1  #set precision for sarsop
    cmd <- paste("./sarsop/src/pomdpsol.exe \"", datfileName, "\" --precision ", precision, " --timeout 100 --output ", outfileName, sep="")
    system(cmd)
    
    print(paste("finished solving. Writing to:", outfileName, collapse=""))
  })
  
  optAct.reactVals <- reactiveValues(optAct2= "do_nothing")#,
                                     #  #make the optimal action a reactive value and initialise it
  
  output$showBel1 <- renderText({
    beliefThreat <- c(input$initThreatBel_shorta, 1-sum(input$initThreatBel_shorta))
    paste(c("The current threat belief is:", beliefThreat, collapse=""))})
  
  output$showBel2 <- renderText({
    beliefSpecies <- c(input$initSpeciesBel_shorta, 1-sum(input$initSpeciesBel_shorta))
    paste(c("The current species belief is:", beliefSpecies, collapse=""))})
  
  
 
  #get the optimal action when the button is pressed
  observeEvent(input$getOptimalAct, {
    print("retrieving optimal action...")
    #print(beliefThreat)
    beliefSpecies <- c(input$initSpeciesBel_shorta, 1-sum(input$initSpeciesBel_shorta))
    beliefThreat <- c(input$initThreatBel_shorta, 1-sum(input$initThreatBel_shorta))
    longBel <- getLongFormatBelief(beliefThreat, beliefSpecies) #convert to long format belief
    benefitRatio <- c(-input$costExt,0,0)
    policy.filename <- paste("./pomdp_solved/ShinySolution_", paste(benefitRatio, collapse="_"),".policy", sep="")
    policy <- read.policy(policy.filename)
    OptAct <- getOptAction(policy, longBel, n.actions)
    #set value of reactive optact.reactVals$optAct2 so we can access this later
    optAct.reactVals$optAct2 <- OptAct
    #OptBel <- updateBelief(longBel, prevState, nextState, OptAct,Transition.matrices)
    output$OptActionVal <- renderText({paste("The optimal action is ", OptAct)})
    print("found optimal action")
  })  
  
  toListen <- reactive({
    list(input$threatState2,input$speciesState2)
  })  #listen for both states to be input before updating
  
  observeEvent(toListen(), {  #react when both buttons pushed
     beliefSpecies <- c(input$initSpeciesBel_shorta, 1-sum(input$initSpeciesBel_shorta))
     beliefThreat <- c(input$initThreatBel_shorta, 1-sum(input$initThreatBel_shorta))
     longBel <- getLongFormatBelief(beliefThreat, beliefSpecies) #convert to long format belief
     print(paste(c("Initial Threat Belief: ",round(beliefThreat,3)), collapse = " "))  #output to console for error checking
     print(paste(c("Initial Species Belief: ", round(beliefSpecies,3)),collapse=" "))
     
     
     initState <- c(input$threatState, input$speciesState)
     nextState <- c(input$threatState2, input$speciesState2)
     action <-  optAct.reactVals$optAct2  #reacts based on the value after pushing the getOptAction button, otherwise default value do_nothing
     Transition.matrices <- get.transition(input$SpeciesMat, input$threatMat1a, input$threatMat2a, input$recoverProb)
     
     newBel <- updateBelief(longBel, initState, nextState, action,Transition.matrices)
     
     #convert long form belief back to short forms
     newBel.short <- extract.modelBelief2(newBel)
     newThreatBel <- newBel.short[[1]] 
     newSpeciesBel <- newBel.short[[2]] 
     print("belief updated")
     
     
     output$action.print <- renderText({paste("The action applied was", action, collapse="")})
     output$newBelief.th <- renderText({
       paste(c("The new threat belief is", round(newThreatBel,3), collapse=""))})
     output$newBelief.sp <- renderText({
       paste(c("The new species belief is ", round(newSpeciesBel,3), collapse=""))})
     
   })
  

  
  #render the plots
  output$SimPlot2 <- renderPlot({
    true.model <- c(input$foxModLabel, input$spModLabel)
    initialState <- c("HighF", "LowSp", true.model[1], true.model[2])
    
    varnames <- c("fox_0", "species_0",  "foxModel_0", "speciesModel_0", "reward")
    #make a dummy dataframe containing the y limits for the plots
    ddummy <-  data.frame(time=1, series=rep(varnames[-(3:4)], each=2), 
                          value=c(rep(c(1:2,1,3), times=1), #fox_0 ranges from 1:2, species_0 from 1:3, 
                                  -20,0)) #limit on the reward (arbitrary lower bound, what should this be?)
    
    #plot simulation variables
    modelName <- paste("model",initialState[3],initialState[4], sep="_")
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
  
  
  output$SimPlot_action <- renderPlot({
    actionDat.long<-melt(df_simMOMDPactions()$action.freq[,-1])
    colnames(actionDat.long)[3] <- "Frequency"
    plotAction <- ggplot(actionDat.long, aes(x = Var2, y = Var1)) + 
      geom_raster(aes(fill=Frequency)) + 
      scale_fill_gradient(low="grey90", high="red") +
      labs(x="Time", y="Action", title="MOMDP Action Selection Freq.") +
      theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                         axis.text.y=element_text(size=9),
                         plot.title=element_text(size=11))
    print(plotAction)
    
  }, height = 300, units="px")
  
  output$threatBelTerminal <- renderText({  #print out terminal simulated belief vector
    end.BelT <- df_belief()[[2]][df_belief()[[2]]$time== input$maxT+1,]$mean  #get the mean beliefs of each model at the terminal time
    paste(c("Terminal Threat Belief:", round(end.BelT,3), collapse=""))})
  
  
  
  # #render the belief plots
  output$TrueModelReport <- renderText({  #"true" model contained in initial state
    true.model <- c(input$foxModLabel, input$spModLabel)
    paste(c("True model:", true.model, collapse=""))})
  
  output$SimPlot3 <- renderPlot({
    df.Belief.plot.fox <- df_belief()[[2]]
    #plot Fox marginal belief
    # df.Belief.plot.fox <- df_belief$df.Belief.fox
    df.Belief.plot.fox$series <- factor(df.Belief.plot.fox$series)
    plotdf.Bel.fox <- ggplot(data= df.Belief.plot.fox, aes(x=time))+
      geom_line(aes(y=lower, group=series, colour= factor(series)), linetype="dashed", size=0.8) +
      geom_line(aes(y=upper,group=series, colour= factor(series)), linetype="dashed", size=0.8)+
      geom_line(aes(y=mean,group=series, colour= factor(series)), size=1.2) +
      ylim(0,1) +
       ggtitle("Threat Model Belief") +
       ylab("Belief")
     plotdf.Bel.fox <- plotdf.Bel.fox+ labs(color='Model') 
     print(plotdf.Bel.fox)
    }, height = 300, units="px")
  
  output$threatBelTerminal <- renderText({  #print out terminal simulated belief vector
    end.BelT <- df_belief()[[2]][df_belief()[[2]]$time== input$maxT+1,]$mean  #get the mean beliefs of each model at the terminal time
    paste(c("Terminal Threat Belief:", round(end.BelT,3), collapse=""))})
  
  
  output$SimPlot4 <- renderPlot({
    #plot Species marginal belief
    df.Belief.plot.species <- df_belief()[[3]]
    df.Belief.plot.species$series <- factor(df.Belief.plot.species$series)
     plotdf.Bel.species <- ggplot(data= df.Belief.plot.species, aes(x=time))+
       geom_line(aes(y=lower, group=series, colour= factor(series)), linetype="dashed", size=0.8) +
       geom_line(aes(y=upper,group=series, colour= factor(series)), linetype="dashed", size=0.8)+
       geom_line(aes(y=mean,group=series, colour= factor(series)), size=1.2) +
       ylim(0,1) +
       ggtitle("Species Model Belief") +
       ylab("Belief")
     plotdf.Bel.species <- plotdf.Bel.species+ labs(color='Model') 
     print(plotdf.Bel.species)
  }, height = 300, units="px")
  
  output$speciesBelTerminal <- renderText({  #print out terminal simulated belief vector
    end.BelS <- df_belief()[[3]][df_belief()[[3]]$time== input$maxT+1,]$mean  #get the mean beliefs of each model at the terminal time
    paste(c("Terminal Species Belief:", round(end.BelS,3), collapse=""))})
  
  #   true.model <- c(input$foxModLabel, input$spModLabel)
  #   initialState <- c("HighF", "LowSp", true.model[1], true.model[2])
  #   
  #   varnames <- c("fox_0", "species_0",  "foxModel_0", "speciesModel_0", "reward")
  #   #make a dummy dataframe containing the y limits for the plots
  #   ddummy <-  data.frame(time=1, series=rep(varnames[-(3:4)], each=2), 
  #                         value=c(rep(c(1:2,1,3), times=1), #fox_0 ranges from 1:2, species_0 from 1:3, ditto xxPrev_0 vars
  #                                 -20,0)) #limit on the reward (arbitrary lower bound, what should this be?)
  #   
  #   #plot simulation variables
  #   modelName <- paste("model",initialState[3],initialState[4], sep="_")
  #   plotdf.all <- ggplot(data= df_all(), aes(x=time)) +
  #     geom_line(aes(y=lower, group=action, colour= factor(action)), linetype="dashed", size=0.8) +
  #     geom_line(aes(y=upper,group=action, colour= factor(action)), linetype="dashed", size=0.8)+
  #     geom_line(aes(y=mean,group=action, colour= factor(action)), size=1.2) +
  #     geom_blank(data= ddummy, aes(x=time, y=value)) +
  #     facet_wrap(vars(series), scales="free_y", ncol=1)+
  #     ggtitle(modelName) +
  #     ylab("") +
  #     theme(plot.title = element_text(hjust = 0.5))  #centres the plot title
  #   plotdf.all <- plotdf.all+ labs(color='Action')
  #   print(plotdf.all)
  #   
  # }, height = 1000, units="px")
}
###########################################
#call Shiny app#
shinyApp(ui = ui, server = server)