# Prepare work space ####
#Load necessary packages
library(shiny)
library(ggplot2)
library(ecostudy)
library(rhandsontable)

# Island Biogeography Functions ####
#Define two simple R functions to calculate immigration and extinction rates based on user input
#These will be used by shiny to generate plots
#Model used to calculate immigration rate: λ = I - (I/P) * S
#Model used to calculate extinction rate: μ = (E/P) * S
# S: A vector with increasing number of species on the island, corresponding to the x axis of the island biogeography plot.
# P: The number of immigrants available on mainland. P will be kept constant here.
# I: Maximum immigration rate from the mainland. I will be defined by user to calculate λ under different conditions.
# E: Maximum extinction rate on island. E will be defined by user to calculate μ under different conditions. 

P <- 100
S <- seq(from = 0, to = 100, by = 10)

#Immigration Rates Function
iRates <- function(I){
  rate <- vector()
  for (i in 1:length(S)){
    rate[i] <- I - (I/P) * S[i]
  }
  return(rate)
}

#Extinction Rates Function
eRates <- function(E){
  rate <- vector()
  for (i in 1:length(S)){
    rate[i] <- (E/P) * S[i]
  }
  return(rate)
}

#Define one more function to calculate the equilibrium species richness
equil <- function(I, E){
  eq <- (I*P)/(I+E)
  
  return(eq)
}


# Life Table Functions ####
#Make a matrix object (lt), which will be the life table that users can fill in on the Shiny app
#For simplicity in this function, we will define the starting population size and number of age classes a priori
#Define two function: 
  # (1) lifTab: Calculate desired life table columns (lx, mx and Rx) from user input values (ax and Fx)
  # (2) popGrowth: Calcualtes R and a table with Nt for the specified time steps
# ax: number of offspring surviving to age x
# Fx: number of female offspring born to females that survive to age x
# ls: proportion of individuals surviving to age x
# mx: proportion of female offspring born to females of age x
# Rx: reproductive rate for females of age x
# R: population's basic reproduction rate
# Nt: population size at time t in the future



#Life Table Function
lifTab <- function(ax, Fx, init){
  #init: inital population size
  lx <- ax/init
  mx <- Fx/ax
  Rx <- lx*mx
  
  results <- list (lx = lx, mx = mx, Rx = Rx)
  return(results)
}


popGrowth <- function(R, time, init){
  #lt: user inputted life table with lx, mx and Rx already calculated, and columns named lx, mx and Rx
  #time: desired time steps for which to calculate population trajectory
  pop <- matrix(ncol = 2, nrow = time, dimnames = list(c(1:time), c("Time", "NT")))
  pop[,"Time"] <- c(0:(length(pop[,"Time"])-1))
  pop[1, "NT"] <- init
  pop[2,"NT"] <- init * R
  #Calculate Nt
  for(i in 3:length(pop[,1])){
    pop[i, "NT"] <- pop[i-1, "NT"] * R
  }
  
  results <- list(R = R, popTraj = pop)
  return(results)
}
  

# Set up ui object ####
#Create panels corresponding to the home page and the Island Biogography, Lotka-Volterra and Life Tables models

uiMods <- navbarPage("MODELS", id = "inTabset",
                     #The homepage has three buttons that allow you to quickly navigate to the page for one of the preferred models
                     #Users can also navigate between pages with the tabs created at the top of the page.
                     tabPanel(div(img(width = 20, src = "HomeLogos.png"), "Home", value = "homePanel"),
                              fluidRow(column(12, 
                                              tags$h2("MODELS IN ECOLOGY AND EVOLUTION", style = "color: white;"), 
                                              tags$h4("Welcome to BIO120's Interactive Model App. With this tool you will be able to explore the behaviour of some fundamental models in ecology and evolution. Select one of the models below to begin.", style = "color: #C5C5C5;"),
                                              align = "center", style = "height:275px; background-color: #242424;padding:120px;")
                                       ),
                              fluidRow(
                                column(12,
                                       div(style="display: inline-block; width: 25%;", tags$button(id = 'JumpToP2', class = "btc action-button", HTML('<center><img src="IslandBioHOME.png" width="100%" alt="Nature" class="responsive"></center>'), style = "background:none; border:0px; padding:0px;")),
                                       div(style="display: inline-block; width: 25%;", tags$button(id = 'JumpToP3', class = "btc action-button", HTML('<center><img src="LotkaVolterraHOME.png" width="100%" alt="Nature" class="responsive"></center>'), style = "background:none; border:0px; padding:0px;")),
                                       div(style="display: inline-block; width: 25%;", tags$button(id = 'JumpToP4', class = "btc action-button", HTML('<center><img src="PopDynHOME.png" width="100%" alt="Nature" class="responsive"></center>'), style = "background:none; border:0px; padding:0px;")),
                                       style = "height:650px; background-color: whitesmoke; padding:20px;", align = "center"
                                       )),
                              fluidRow(column(12,
                                       tags$p(HTML(paste0("This app was built by Viviana Astudillo and Megan Frederickson using the R packages ", tags$a(href="https://shiny.rstudio.com/", "shiny"), ", ", tags$a(href="https://cran.r-project.org/web/packages/ggplot2/index.html", "ggplot2"), ", ", tags$a(href="https://github.com/jrowen/rhandsontable", "rhandsontable"),  
                                                          ", and ", tags$a(href="https://github.com/jarioksa/ecostudy", "ecostudy"), "."))),
                                       tags$p(HTML(paste0("Copyright \uA9 2019 by Viviana Astudillo and Megan Frederickson"))),
                                       tags$p("BIO120  |  DEPARTMENT OF ECOLOGY & EVOLUTIONARY BIOLOGY  |  UNIVERSITY OF TORONTO"), style = "background-color: whitesmoke;", align = "center"))
                              
                              ),
                     
                     #Island Biogeography Tab (id = panel2)
                     #Create reactive inputs of type "slider" (dist: reactive input for distance of island to mainland, size: reactive input for island size) 
                     #Create reactive output of type "plot" that responds to changes in the dist and size sliders. 
                     tabPanel(value = "panel2",
                              div(img(width = 20, src = "IslandBioLOGO.png"), "Island Biogeography"), 
                              fluidRow(column(12, HTML('<center><img src="IslandBioHeader.png" width="100%" alt="Nature" class="responsive"></center>')), style = "background-color: whitesmoke;"),
                              fluidRow(column(12,
                                              tags$br(), # adds line break
                                              tags$p("MacArthur and Wilson's Theory of Island Biogeography is a simple model of the number of species that live on an island. Species arrive on the island as immigrants from a continental species pool and species are lost from the island when they go locally extinct. 
                                                  The equilibrium species richness is the number of species on the island when new species colonize the island at the same rate that existing species go extinct. This occurs when the colonization and extinction curves intersect (the dashed line in the figure)."), 
                                              tags$p("Two main factors influence colonization and extinction rates:", strong("distance from the mainland"), "and", strong("island size."), "More distant islands are harder to reach than closer islands. As a result, colonization rate decreases with increasing distance from the mainland. Large islands have more resources than small islands, and thus support more species. Therefore, the extinction rate decreases with increasing island size."),
                                              tags$p(tags$b("Use the controls on the left, below, to explore how changing the distance and size of an island affects the number of species on the island at equilibrium."),
                                              tags$p("This model applies to any island-like habitat in which patches of suitable habitat occur within an inhospitable matrix, such as forest fragments. What kind of forest fragments would you expect to have the most species?"))
                              ), style = "background-color: whitesmoke;"),
                              tags$hr(), #Adds horizontal line
                              fluidRow(column(4, tags$h3("Controls"), align = "center"), column(7, tags$h3("Model"), align = "center"), offset = 1),
                              fluidRow(column(4, wellPanel(sliderInput(inputId = "dist", label = "Distance from Mainland (km)", value = 1, min = 1, max = 5),
                                                          sliderInput(inputId = "size", label = "Island Size (km²)", value = 1, min = 1, max = 5))), 
                                       column(7, plotOutput(outputId = "IslandBioPLOT", height = "400px"), style = "border: 2px double whitesmoke;"), offset = 1),
                              tags$br(),
                              tags$br(), # adds line break
                              HTML('<center><img src="IslandBioFooter.png" width="100%" alt="Nature" class="responsive"></center>'),
                              tags$br()
                     ),
                     
                     #Lokta-Volera Panel (id = panel3)
                     #Create reactive inputs of type "slider" (alpha: impact of Sp2 on Sp1, beta: impact of Sp1 on Sp2, K1: Sp1 carrying capacity, K2: Sp2 carrying capacity, N1: Sp1 initial population size, N2: Sp2, initial population size) 
                     #Create reactive output of type "plot" that responds to changes in the competition model (alpha, beta, K1 & K2) and initial population (N1 & N2) parameters. 
                     tabPanel(value = "panel3",
                              div(img(width = 20, src = "LotkaVolterraLOGO.png"), "Lotka-Volterra"),
                              fluidRow(column(12, HTML('<center><img src="LotkaVolterraHeader.png" width="100%" alt="Nature" class="responsive"></center>')), style = "background-color: whitesmoke;"),
                              fluidRow(column(12,
                                              tags$br(), # adds line break0
                                              tags$p("The Lotka-Volterra model is a simple model of the population growth of two competing species. In the absence of competitors, each
                                                     species grows according the logistic equation, eventually reaching its species-specific carrying capacity. Competition between the two species has four
                                                     possible outcomes: 1) species 1 wins and species 2 goes extinct, 2) species 2 wins and species 1 goes extinct, 3) the two species coexist at equilibrium, or 4) the outcome
                                                     depends on the initial population sizes of the two species."), 
                                              tags$p(tags$b("Use the controls on the left, below, to explore how competition between sparrows (species 1) and squirrels (species 2) affects their population dynamics.
                                                            Use the sliders to adjust the values of the competition coefficients (alphas), carrying capacities, and initial population sizes for both species.")),
                                                    tags$p("What makes squirrels out-compete sparrows? What makes sparrows out-compete squirrels? And what allows the two species to stably coexist?"),
                                                     tags$p(HTML(paste0("Note: the subscripts identify the species (e.g., K", tags$sub(1), " is the carrying capacity of species 1). For the competition coefficients, alpha", tags$sub(12)," quantifies the
                                                            impact of species 2 on species 1, and alpha", tags$sub(21), " quantifies the impact of species 1 on species 2.")))), style = "background-color: whitesmoke;"),
                              tags$hr(), #Adds horizontal line
                              fluidRow(column(4, tags$h3("Controls"), align = "center"), column(7, tags$h3("Model"), align = "center"), offset = 1),
                              fluidRow(column(4, wellPanel(sliderInput(inputId = "alpha", label = HTML(paste0("Impact of Squirrel on Sparrow (alpha", tags$sub(12), ")")), value = 0, min = 0, max = 1.5, step = 0.5),
                                                           sliderInput(inputId = "beta", label = HTML(paste0("Impact of Sparrow on Squirrel (alpha", tags$sub(21), ")")), value = 0, min = 0, max = 0.9, step = 0.3),
                                                           sliderInput(inputId = "K1", label = HTML(paste0("Sparrow Carrying Capacity (K",tags$sub(1),")")), value = 50, min = 50, max = 100, step = 50),
                                                           sliderInput(inputId = "K2", label = HTML(paste0("Squirrel Carrying Capacity (K",tags$sub(2),")")), value = 30, min = 30, max = 60, step = 30),
                                                           sliderInput(inputId = "N1", label = HTML(paste0("Sparrow Initial Population Size (N", tags$sub(1), ")")), value = 10, min = 10, max = 110, step = 50),
                                                           sliderInput(inputId = "N2", label = HTML(paste0("Squirrel Initial Population Size (N",tags$sub(2), ")")), value = 10, min = 10, max = 110, step = 50)
                                                           )), 
                                       column(7, HTML('<center><img src="LotkaVolterra-Animals.png" width="60%" alt="Nature" class="responsive"></center>'),
                                              plotOutput(outputId = "lvPLOT", height = "400px"), style = "border: 2px double whitesmoke; padding: 30px;"), offset = 2),
                              tags$br(),
                              tags$br(), # adds line break
                              HTML('<center><img src="LotkaVolterraFooter.png" width="100%" alt="Nature" class="responsive"></center>'),
                              tags$br()
                     ),
                     
                     #Life Table (id = panel4)
                     #Create reactive inputs of type "table" (users input values for ax and Fx) 
                     #Creat reactive ouputs that responds to changes in the life table (ax & Fx) of type:
                      #(1) "table": calcualtes the missing lx, mx and Rx values
                      #(2) "plot": makes a plot showing age-structured survivability and reproduction and another showing projected population growth through time
                     tabPanel(value = "panel4",
                              div(img(width = 20, src = "PopDynLOGO.png"), "Life Tables"),
                              fluidRow(column(12, HTML('<center><img src="PopDynHeader.png" width="100%" alt="Nature" class="responsive"></center>')), style = "background-color: whitesmoke;"),
                              fluidRow(column(12,
                                              tags$br(), # adds line break
                                              tags$p("In real populations, not all individuals have the same probability of giving birth or dying. 
                                                     Instead, fecundity and survivorship depend on age. A life table summarizes age-specific fecundity 
                                                     and survivorship rates. We can use these rates to predict how population size will change over time."), 
                                              tags$p(tags$b("Use the life table below to explore how age-specific survivorship and fecundity 
                                                      affect population growth. Input new values in the life table, and click on the Update Plots button to see the resulting survivorship, fecundity, and population growth curves.")),
                                              tags$p("What makes a population grow? What makes a population decline? Given what 
                                                     you learned in lecture 18, what numbers correspond to an r versus K life history strategy?"),
                                              tags$p("Note: The number of survivors in each age class must always be a positive, non-zero number. 
                                                     The number of offspring born to each age class can be zero, but must not be a negative number. 
                                                     Also, make sure that the number of surviving individuals stays the same or declines with age; 
                                                     individuals that were dead at age 2 cannot be alive at age 3, so the number of survivors cannot 
                                                     increase with age.")
                                              ),
                              style = "background-color: whitesmoke;"),
                              tags$hr(), #Adds horizontal line
                              fluidRow(column(12, tags$h3("Life Table"), align = "center")),
                              tags$br(),
                              #Life Table: Column Names
                              fluidRow(column(1), 
                                       column(2, tags$h5("Age"), align = "center"), 
                                       column(2, tags$h5("Survivors"), align = "center"), 
                                       column(2, tags$h5("Offspring"), align = "center"), 
                                       column(2, tags$h5(HTML(paste("l", tags$sub("x"), sep = ""))), align = "center"), 
                                       column(2, tags$h5(HTML(paste("m", tags$sub("x"), sep = ""))), align = "center"), 
                                       column(1)),
                              fluidRow(column(1),
                                       column(2, p(em("x"), align="center")), 
                                       column(2, p(em("Total individuals surviving to age x"), align = "center")),
                                       column(2, p(em("Total female offspring born to all females of age x"), align = "center")), 
                                       column(2, p(em("Probability of surviving to age x"), align = "center")), 
                                       column(2, p(em("Number of female offspring per female of age x"), align = "center")),
                                       column(1)),
                              fluidRow(column(1), column(10, tags$hr()), column(1)),
                              #Life Table: Row 0
                              fluidRow(tags$style(HTML('#ax0{height: 30px; width: 75%; position: absolute; margin-left: 12.5%} #Fx0{height: 30px; width: 75%; position: absolute; margin-left: 12.5%}')),
                                       column(1),
                                       column(2, tags$h5("0"), align = "center", style = "background-color: whitesmoke; padding: 5px;"),
                                       column(2, numericInput(inputId = "ax0", label = NULL, value = 1000, min = 1000, max = 1000), align = "center", style = "background-color: whitesmoke; padding-top: 10px; padding-bottom: 20px;"),
                                       column(2, numericInput(inputId = "Fx0", label = NULL, value = 0, min = 1, max = 1000), align = "center", style = "background-color: whitesmoke; padding-top: 10px; padding-bottom: 20px;"),
                                       column(2, textOutput(outputId = "lx0"), align = "center", style = "font-weight: bold; background-color: whitesmoke; padding: 12.5px;"),
                                       column(2, textOutput(outputId = "mx0"), align = "center", style = "font-weight: bold; background-color: whitesmoke; padding: 12.5px;"),
                                       column(1)),
                              #Life Table: Row 1
                              fluidRow(tags$style(HTML('#ax1{height: 30px; width: 75%; position: absolute; margin-left: 12.5%} #Fx1{height: 30px; width: 75%; position: absolute; margin-left: 12.5%}')),
                                       column(1),
                                       column(2, tags$h5("1"), align = "center", style = "padding: 5px;"),
                                       column(2, numericInput(inputId = "ax1", label = NULL, value = 900, min = 1, max = 1000), align = "center", style = "padding-top: 10px; padding-bottom: 20px;"),
                                       column(2, numericInput(inputId = "Fx1", label = NULL, value = 200, min = 1, max = 1000), align = "center", style = "padding-top: 10px; padding-bottom: 20px;"),
                                       column(2, textOutput(outputId = "lx1"), align = "center", style = "font-weight: bold; padding: 12.5px;"),
                                       column(2, textOutput(outputId = "mx1"), align = "center", style = "font-weight: bold; padding: 12.5px;"),
                                       column(1)),
                              #Life Table: Row 2
                              fluidRow(tags$style(HTML('#ax2{height: 30px; width: 75%; position: absolute; margin-left: 12.5%} #Fx2{height: 30px; width: 75%; position: absolute; margin-left: 12.5%}')),
                                       column(1),
                                       column(2, tags$h5("2"), align = "center", style = "background-color: whitesmoke; padding: 5px;"),
                                       column(2, numericInput(inputId = "ax2", label = NULL, value = 850, min = 1, max = 1000), align = "center", style = "background-color: whitesmoke; padding-top: 10px; padding-bottom: 20px;"),
                                       column(2, numericInput(inputId = "Fx2", label = NULL, value = 750, min = 1, max = 1000), align = "center", style = "background-color: whitesmoke; padding-top: 10px; padding-bottom: 20px;"),
                                       column(2, textOutput(outputId = "lx2"), align = "center", style = "font-weight: bold; background-color: whitesmoke; padding: 12.5px;"),
                                       column(2, textOutput(outputId = "mx2"), align = "center", style = "font-weight: bold; background-color: whitesmoke; padding: 12.5px;"),
                                       column(1)),
                              #Life Table: Row 3
                              fluidRow(tags$style(HTML('#ax3{height: 30px; width: 75%; position: absolute; margin-left: 12.5%} #Fx3{height: 30px; width: 75%; position: absolute; margin-left: 12.5%}')),
                                       column(1),
                                       column(2, tags$h5("3"), align = "center", style = "padding: 5px;"),
                                       column(2, numericInput(inputId = "ax3", label = NULL, value = 800, min = 1, max = 1000), align = "center", style = "padding-top: 10px; padding-bottom: 20px;"),
                                       column(2, numericInput(inputId = "Fx3", label = NULL, value = 500, min = 1, max = 1000), align = "center", style = "padding-top: 10px; padding-bottom: 20px;"),
                                       column(2, textOutput(outputId = "lx3"), align = "center", style = "font-weight: bold; padding: 12.5px;"),
                                       column(2, textOutput(outputId = "mx3"), align = "center", style = "font-weight: bold; padding: 12.5px;"),
                                       column(1)),
                              #Life Table: Row 4
                              fluidRow(tags$style(HTML('#ax4{height: 30px; width: 75%; position: absolute; margin-left: 12.5%} #Fx4{height: 30px; width: 75%; position: absolute; margin-left: 12.5%}')),
                                       column(1),
                                       column(2, tags$h5("4"), align = "center", style = "background-color: whitesmoke; padding: 5px;"),
                                       column(2, numericInput(inputId = "ax4", label = NULL, value = 300, min = 1, max = 1000), align = "center", style = "background-color: whitesmoke; padding-top: 10px; padding-bottom: 20px;"),
                                       column(2, numericInput(inputId = "Fx4", label = NULL, value = 100, min = 1, max = 1000), align = "center", style = "background-color: whitesmoke; padding-top: 10px; padding-bottom: 20px;"),
                                       column(2, textOutput(outputId = "lx4"), align = "center", style = "font-weight: bold; background-color: whitesmoke; padding: 12.5px;"),
                                       column(2, textOutput(outputId = "mx4"), align = "center", style = "font-weight: bold; background-color: whitesmoke; padding: 12.5px;"),
                                       column(1)),
                              #Life Table: Row 5
                              fluidRow(tags$style(HTML('#ax5{height: 30px; width: 75%; position: absolute; margin-left: 12.5%} #Fx5{height: 30px; width: 75%; position: absolute; margin-left: 12.5%}')),
                                       column(1),
                                       column(2, tags$h5("5"), align = "center", style = "padding: 5px;"),
                                       column(2, numericInput(inputId = "ax5", label = NULL, value = 10, min = 1, max = 1000), align = "center", style = "padding-top: 10px; padding-bottom: 20px;"),
                                       column(2, numericInput(inputId = "Fx5", label = NULL, value = 5, min = 1, max = 1000), align = "center", style = "padding-top: 10px; padding-bottom: 20px;"),
                                       column(2, textOutput(outputId = "lx5"), align = "center", style = "font-weight: bold; padding: 12.5px;"),
                                       column(2, textOutput(outputId = "mx5"), align = "center", style = "font-weight: bold; padding: 12.5px;"),
                                       column(1)),
                              tags$br(),
                              tags$br(),
                              tags$br(),
                              fluidRow(column(1), column(10, actionButton("go", "Update Plots"), align = "center", style = "background-color: #BCC37A; padding: 10px;"), column(1)),
                              tags$br(),
                              tags$br(),
                              fluidRow(column(12, tags$h3("Model"), align = "center"), offset = 1),
                              fluidRow(column(6, plotOutput(outputId = "lxmxPLOT", height = "400px"), style = "border: 2px double whitesmoke; padding: 30px;"),
                                       column(6, plotOutput(outputId = "popPLOT", height = "400px"), style = "border: 2px double whitesmoke; padding: 30px;"),
                                       align = "center"),
                              tags$br(),
                              tags$br(), # adds line break
                              HTML('<center><img src="PopDynFooter.png" width="100%" alt="Nature" class="responsive"></center>'),
                              tags$br()
                     )
                     
)


# Define function(s) for the app ####
#Tell R what to do with the user inputs in order to generate the desired reactive output
#Save output objects to be displayed to user to the list object output$.
#Assign input values to the list object input$ and make the render* command dependant on input$. 
#As a result, every time the user changes the input value, output$ is updated based on the new input$ value. 

server <- function(input, output, session){
#NAGIVATE TO MODEL TABS FROM HOMEPAGE
#Connect the 3 buttons on the homepage to the 3 model pannels, so that when the user clicks on one, they are taken to the corresponding model page
    #Island Biogeography (i.e. panel2)
    observeEvent(input$JumpToP2, {
      updateTabsetPanel(session, "inTabset", selected = "panel2")
      })
      
    #Lotka-Voltera (i.e. panel3)
    observeEvent(input$JumpToP3, {
      updateTabsetPanel(session, "inTabset", selected = "panel3")
      })
    
    #Life Tables (i.e. panel4)
    observeEvent(input$JumpToP4, {
      updateTabsetPanel(session, "inTabset", selected = "panel4")
      })
  
#MODEL OUTPUTS
    #Island Biogeography (i.e. panel2)
    #Calculate I (immigration) and E (extinction) rates with the functions defined above.
    #Also use the equilibrium function defined above to calcualte equilibrium I and E rates and S (species richness) at equilibrium
    #Output a plot showing S at different values of I and E, as well as S at equilibrium S   
    output$IslandBioPLOT <- renderPlot({
      I <- (input$dist * (-1)) + 6
      E <- (input$size * (-1)) + 6
      imRates <- iRates(I)
      exRates <- eRates(E)
      xy <- lm(imRates~S)
      Sequil <- equil(I, E)
      yequil <- (coef(xy)[2]*Sequil)+coef(xy)[1]
      rates <- data.frame(species = c(rep(S, 2), rep(Sequil, 2)), Rate = c(imRates, exRates, c(0.0, yequil)), Legend = c(rep("Colonization", 11), rep("Extinction", 11), rep("Equilibrium", 2)))
      ggplot(data = rates, aes(x = species, y = Rate, group = Legend, color = Legend)) +
        geom_line(aes(linetype = Legend)) + 
        scale_color_manual(name = "Legend", values = c("Colonization" = "blue", "Extinction" = "red", "Equilibrium" = "black")) +
        scale_linetype_manual(name = "Legend", values = c("Colonization" = "solid", "Extinction" = "solid", "Equilibrium" = "dashed")) +
        guides(name = "Legend", values = c("Colonization", "Extinction", "Equilibrium"), linetype = guide_legend(reverse = TRUE), color = guide_legend(reverse = TRUE)) +
        ylim(0, 5) + 
        ylab("Colonization or extinction rate") + theme(axis.title.y = element_text(size = 15, hjust = 0.5, vjust = 0.3)) + 
        xlab("Number of species on the island") + theme(axis.title.x = element_text(size = 15, hjust = 0.5, vjust = 0.3)) +
        theme_bw(base_size=12.5)+
        theme(panel.grid = element_blank(), legend.title = element_blank() , legend.position = "bottom", legend.spacing.x = unit(.70, "cm"), legend.text = (element_text(size = 14))) 
    })
    
    #Lokta-Voltera (i.e. panel3)
    #Calculate the LV competition model based on user input alpha, beta, L1 and K2 parameters
    #Plot the trajectory of the two species over time based on the user-defined LV competition model
    output$lvPLOT <- renderPlot({
      mod <- lotkacomp(alpha = input$alpha, beta = input$beta, K1 = input$K1, K2 = input$K2)
      plot(traj(mod, input$N1, input$N2), main = "Trajectory of Populations Through Time", ylim = c(0, 120), ylab= "Population size (N)", xlab = "Time (days)")
    })
    
    #Life Tables (i.e. panel4)
    #Makes fillable life table and calculates missing columns from user inputted values
    #Set up Life Table
    #The life table is then used to generate two population dynamics plots
    axUser <- reactive(c(input$ax0, input$ax1, input$ax2, input$ax3, input$ax4, input$ax5))
    FxUser <- reactive(c(input$Fx0, input$Fx1, input$Fx2, input$Fx3, input$Fx4, input$Fx5))
    ageClasses <- c(0:5) #Age Classes 
    lt <- matrix (ncol =6, nrow = 6, dimnames = list(c(1:6), c("Age", "ax", "Fx", "lx", "mx", "Rx")))
    lt[,"Age"] <- c(0:5)
    ltUser <- reactiveValues(mtx = lt)
    observeEvent(eventExpr = c(input$ax0, input$ax1, input$ax2, input$ax3, input$ax4, input$ax5, input$Fx0, input$Fx1, input$Fx2, input$Fx3, input$Fx4, input$Fx5),
      for(i in 1:length(ageClasses)){
      ltUser$mtx[i, "ax"] <- axUser()[i]
      ltUser$mtx[i, "Fx"] <- FxUser()[i]
      ltUser$mtx[i, "lx"] <- lifTab(ax = axUser()[i], Fx = FxUser()[i], init = axUser()[1])$lx
      ltUser$mtx[i, "mx"] <- lifTab(ax = axUser()[i], Fx = FxUser()[i], init = axUser()[1])$mx
      ltUser$mtx[i, "Rx"] <- lifTab(ax = axUser()[i], Fx = FxUser()[i], init = axUser()[1])$Rx
      })
    
   output$lx0 <- renderText(ltUser$mtx[1, "lx"]); output$mx0 <- renderText(ltUser$mtx[1, "mx"])
   output$lx1 <- renderText(ltUser$mtx[2, "lx"]); output$mx1 <- renderText(ltUser$mtx[2, "mx"])
   output$lx2 <- renderText(ltUser$mtx[3, "lx"]); output$mx2 <- renderText(ltUser$mtx[3, "mx"])
   output$lx3 <- renderText(ltUser$mtx[4, "lx"]); output$mx3 <- renderText(ltUser$mtx[4, "mx"])
   output$lx4 <- renderText(ltUser$mtx[5, "lx"]); output$mx4 <- renderText(ltUser$mtx[5, "mx"])
   output$lx5 <- renderText(ltUser$mtx[6, "lx"]); output$mx5 <- renderText(ltUser$mtx[6, "mx"])
   
   randomVals <- eventReactive(input$go, {
     runif(input$n)
   })
   pp <- eventReactive(input$go, {
     runif(c(input$ax0,input$ax1, input$ax2, input$ax3, input$ax4, input$ax5, input$Fx1, input$Fx2, input$Fx3, input$Fx4, input$Fx5))
     {
       ltgg <- data.frame(Age = ltUser$mtx[,"Age"], lx = ltUser$mtx[, "lx"], mx = ltUser$mtx[, "mx"])
       ggplot(data = ltgg, aes(x = Age)) +
         geom_line(aes(y = lx, colour = "Survivorship")) +
         geom_line(aes(y = mx, colour = "Fecundity")) +
         scale_y_continuous (sec.axis = sec_axis(~.*5, name = expression("Female offspring per female of age x (m"[x]*")"))) +
         scale_colour_manual(values = c("blue", "red")) +
         labs(title = "Survivorship & Fecundity Schedules", y = expression("Probability of surviving to age x (l"[x]*")"), x = "Age (x)") +
         theme_bw(base_size = 12.5) +
         theme(panel.grid = element_blank(), legend.position=c(0.8, 0.9), legend.title=element_blank(), legend.background=element_blank(), plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 20)), axis.title.y.left = element_text(margin = margin(r = 10)), axis.title.y.right = element_text(margin = margin(l = 15)))
       }
     })
           
   output$lxmxPLOT <- renderPlot({
     pp()
   })
   
  pp2 <- eventReactive(input$go, {
    runif(c(input$ax0,input$ax1, input$ax2, input$ax3, input$ax4, input$ax5, input$Fx1, input$Fx2, input$Fx3, input$Fx4, input$Fx5))
    { 
      R <- round(sum(ltUser$mtx[, "Rx"]),  3)
      xlxmx <- ltUser$mtx[, "lx"]*ltUser$mtx[, "mx"]*ltUser$mtx[, "Age"]
      Generation = round(sum(xlxmx)/R,3)
      little_r <- round(log(R)/Generation, 3)
       popG <- data.frame(popGrowth(R = R, time = 10, init = ltUser$mtx[1, "ax"])$popTraj) 
      ggplot(data = popG, aes(x = Time, y = NT)) +
        geom_line() +
        scale_colour_manual(values = "black") +
        labs(title = "Population Trajectory", y = "Population size (N)", x = "Time (generations)") +
        annotate("text", x=(max(popG$Time)-(max(popG$Time) - min(popG$Time))/8), ((max(popG$NT)-min(popG$NT))/8), label=paste0("R0 = ", R, "\nT = ", Generation, "\nr = ", little_r))+
        theme_bw(base_size = 12.5) +
        theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 20)), axis.title.y = element_text(margin = margin(r = 10)))
      }
    })
  
    output$popPLOT <- renderPlot({
      pp2()
    })
   
}


# Combine ui object and function(s) into a shiny app ####
shinyApp(ui = uiMods, server = server)
