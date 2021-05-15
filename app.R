library(shiny)
# Lattice is used for levelplot()
library(lattice)

## UI selector for different app options
appTabs <- tabsetPanel(
                id = "apptype",
                type = "hidden",
                tabPanel("move",uiOutput("typeopt_move")),
                tabPanel("typeweakness",uiOutput("typeweak")),
                tabPanel("partyattack",uiOutput("typeopt_attack")),
                #tabPanel("partydefend",uiOutput("typeopt_defend")),
                tabPanel("viewchart"," ")
)

ui <- fluidPage(
    #Application Title
    sidebarLayout(
        sidebarPanel(
            titlePanel("Pokemon Type Checker"),
            #Input for
            helpText("Check the effectiveness of a move, check weaknesses of a Pokemon, check max effectiveness of party attacks (party attack coverage), or view type chart"),

            #Select generation
            selectInput("genchoice","Generation: ",c(1:8),1),
            
            #Select app purpose
            radioButtons("chooseapp","Choose Action:",choices=c("Check Move"="move","Check Pokemon Weakness"="typeweakness","Party Coverage"="partyattack",#"Party Resistance"="partydefend",
            "View Typechart"="viewchart"),selected="viewchart"),
            
            actionButton("check","Check!"),

            #Variable output
            appTabs
            #,textOutput("gen_choice")
        )
    ,
      mainPanel(
          plotOutput("outplot",height="800px",inline=FALSE)
      )
    ,
    position=c("left"),
    fluid=TRUE
    )
)

server <- function(input,output,session){
    # Read in and store the typecharts
    gen1.raw <- read.csv("Gen1.csv",header=TRUE,row.names=1)
    gen2_5.raw <- read.csv("Gen2-5.csv",header=TRUE,row.names=1)
    gen6_.raw <- read.csv("Gen6-.csv",header=TRUE,row.names=1)

    gen1 <- gen1.raw[sort(rownames(gen1.raw)),sort(colnames(gen1.raw))]
    gen2_5 <- gen2_5.raw[sort(rownames(gen2_5.raw)),sort(colnames(gen2_5.raw))]
    gen6_ <- gen6_.raw[sort(rownames(gen6_.raw)),sort(colnames(gen6_.raw))]

    #Initialiize the generation and the typechart (default gen1)
    generation <- reactiveVal(1)
    typechart <- reactiveVal(gen1)
    
    # Update the variable input with user choice of app type
    observeEvent(input$chooseapp,{
        updateTabsetPanel(inputId="apptype",selected=input$chooseapp)
    })

    # Update the generation choice with user choice of generation
    observeEvent(input$genchoice,{
        generation(input$genchoice)

        if(generation()==1){
            typechart(gen1)
        } else if(generation()>=2 & generation()<=5){
            typechart(gen2_5)
        } else if(generation()>=6){
            typechart(gen6_)
        } else{
            typechart(gen1)
        }
    })

    
    # Colors for regular, super, not very, no effect
    power.colors <- c("grey","darkred","red","white","green","green4")

    # Dividing lines for the graph colors for levelplot
    atlist <- c(0,0.2,0.45,0.95,1.95,3.95,4.45)

    # Update the view if the user clicks "check"
    observeEvent(input$check,{
        # Choose outcome based on chooseapp input
        
        if(input$chooseapp=="move"){
            # Check power of a type against all other types
            attack.effect <- check_attack(attack_type=input$move_typechoice,typechart())
            output$outplot <- renderPlot({
                    levelplot(as.matrix(attack.effect),col.regions=power.colors,at=atlist,colorkey=NULL,ylab=NULL,xlab="Defending Type",scales=list(x=list(rot=90)),
                       panel=function(...) {
                       arg <- list(...)
                       panel.levelplot(...)
                       panel.text(arg$x, arg$y, round(arg$z,1))})
            })
        }else if(input$chooseapp=="typeweakness"){
            # Check weakness/resistance for a single pokemon
            
            pokemon.type <- c()
            if(input$pkmntype1 != "None"){
                pokemon.type <- c(pokemon.type,input$pkmntype1)
            }
            if(input$pkmntype2 != "None"){
                pokemon.type <- c(pokemon.type,input$pkmntype2)
            }

            #Party_weakness needs a list() object, and the typechart for the selected generation
            pokemon.weakness <- party_weakness(list(pokemon.type),typechart())
            
            output$outplot <- renderPlot({
                    levelplot(as.matrix(pokemon.weakness),col.regions=power.colors,at=atlist,colorkey=NULL,ylab=NULL,xlab="Attacking Type",scales=list(x=list(rot=90)),
                       panel=function(...) {
                       arg <- list(...)
                       panel.levelplot(...)
                       panel.text(arg$x, arg$y, round(arg$z,1))})
            })
            
        }else if(input$chooseapp=="partyattack"){
            # Check the maximum effectiveness of all move types known by the party
            attack.coverage <- party_strengths(input$attack_typechoice,typechart())
            
            output$outplot <- renderPlot({
                    levelplot(as.matrix(attack.coverage),col.regions=power.colors,at=atlist,colorkey=NULL,ylab=NULL,xlab="Defending Type",scales=list(x=list(rot=90)),
                       panel=function(...) {
                       arg <- list(...)
                       panel.levelplot(...)
                       panel.text(arg$x, arg$y, round(arg$z,1))})
                })
        ## } else if(input$chooseapp=="partydefend"){
        ##     input.types <- list(
        ##         c(input$pkmn1type1,input$pkmn1type2),
        ##         c(input$pkmn2type1,input$pkmn2type2),
        ##         c(input$pkmn3type1,input$pkmn3type2),
        ##         c(input$pkmn4type1,input$pkmn4type2),
        ##         c(input$pkmn5type1,input$pkmn5type2),
        ##         c(input$pkmn6type1,input$pkmn6type2)
        ##     )
            
                ## output$outplot <- NULL
                
        } else{
            # Just print the type chart
                output$outplot <- renderPlot({
                    levelplot(t(as.matrix(typechart())),col.regions=power.colors,at=atlist,colorkey=NULL,ylab="Attacking Type",xlab="Defending Type",scales=list(x=list(rot=90)),
                       panel=function(...) {
                       arg <- list(...)
                       panel.levelplot(...)
                       panel.text(arg$x, arg$y, round(arg$z,1))})
            })
                
        }
        
    })

    output$gen_choice <- renderText(generation())

    
    output$typeopt <- renderText(paste0(rownames(typechart())))
    output$typeopt_move <- renderUI({
        radioButtons(inputId="move_typechoice",label="Choose Move Type:",choices=as.list(rownames(typechart())))
    })

    output$typeweak <- renderUI({
        div(fluidRow(column(width=6,"Choose Pokemon Type:")),
             fluidRow(column(width=4,selectInput(inputId="pkmntype1",label=NULL,choices=c("None",rownames(typechart())),selected="None")),column(width=4,selectInput(inputId="pkmntype2",label=NULL,choices=c("None",rownames(typechart())),selected="None")))
        )
    })
    

    output$typeopt_attack <- renderUI({
        checkboxGroupInput(inputId="attack_typechoice",label="Choose Party Attack Types:",choices=as.list(rownames(typechart())))
    })

    ## output$typeopt_defend <- renderUI({
    ##     div(
    ##     fluidRow(column(width=6,"")),
    ##     fluidRow(column(width=6,"Choose Party Types:")),
    ##     fluidRow(column(width=4,selectInput(inputId="pkmn1type1",label=NULL,choices=c("None",rownames(typechart())),selected="None")),column(width=4,selectInput(inputId="pkmn1type2",label=NULL,choices=c("None",rownames(typechart())),selected="None"))),
    ##     fluidRow(column(width=4,selectInput(inputId="pkmn2type1",label=NULL,choices=c("None",rownames(typechart())),selected="None")),column(width=4,selectInput(inputId="pkmn2type2",label=NULL,choices=c("None",rownames(typechart())),selected="None"))),
    ##     fluidRow(column(width=4,selectInput(inputId="pkmn3type1",label=NULL,choices=c("None",rownames(typechart())),selected="None")),column(width=4,selectInput(inputId="pkmn3type2",label=NULL,choices=c("None",rownames(typechart())),selected="None"))),
    ##     fluidRow(column(width=4,selectInput(inputId="pkmn4type1",label=NULL,choices=c("None",rownames(typechart())),selected="None")),column(width=4,selectInput(inputId="pkmn4type2",label=NULL,choices=c("None",rownames(typechart())),selected="None"))),
    ##     fluidRow(column(width=4,selectInput(inputId="pkmn5type1",label=NULL,choices=c("None",rownames(typechart())),selected="None")),column(width=4,selectInput(inputId="pkmn5type2",label=NULL,choices=c("None",rownames(typechart())),selected="None"))),
    ##     fluidRow(column(width=4,selectInput(inputId="pkmn6type1",label=NULL,choices=c("None",rownames(typechart())),selected="None")),column(width=4,selectInput(inputId="pkmn6type2",label=NULL,choices=c("None",rownames(typechart())),selected="None")))
    ##     )
    ## })
    
    

    ## output$typeselector <- renderUI({
    ##             typelist <- rownames(gen1)
    ##             checkboxGroupInput("movetype","Move Type:",as.list(typelist))
    ##     })
    
}

## Defining useful functions

## Check effect of attack against defending types
attack_effectiveness <- function(attack_type,defend_types,type_chart){
    return(prod(type_chart[attack_type,defend_types]))
}

## Check effectiveness of attack against single defending type
check_attack <- function(attack_type,type_chart){
    typelist <- rep(0,nrow(type_chart))
    names(typelist) <- rownames(type_chart)
    for(i in 1:length(typelist)){
        typelist[[i]] <- attack_effectiveness(attack_type,names(typelist)[[i]],type_chart)
    }
    return(typelist)
}

## Check max effectiveness of all party move types
party_strengths <- function(party_type_list,type_chart){
    typelist <- rep(0,nrow(type_chart))
    names(typelist) <- rownames(type_chart)
    for(i in 1:length(typelist)){
        for(j in 1:length(party_type_list)){
            type_effective <- attack_effectiveness(party_type_list[[j]],names(typelist)[[i]],type_chart)
            if(type_effective > typelist[[i]]){
                typelist[[i]] <- type_effective
            }
        }
    }

    return(typelist)
}

## Check resistance/weakness of party (not used)
party_weakness <- function(party_pokemon_types,type_chart){
    weakness <- rep(0,nrow(type_chart))
    names(weakness) <- rownames(type_chart)
    resistance <- rep(4,nrow(type_chart))
    names(resistance) <- rownames(type_chart)
    for(i in 1:length(party_pokemon_types)){
        for(j in 1:nrow(type_chart)){
            type_effective <- attack_effectiveness(attack_type=rownames(type_chart)[j],defend_types=party_pokemon_types[[i]],type_chart)
            if(type_effective > weakness[[j]]){
                weakness[[j]] <- type_effective
            }
            if(type_effective < resistance[[j]]){
                resistance[[j]] <- type_effective
            }
        }
    }

#    partymakeup <- cbind(weakness,resistance)
#    colnames(partymakeup) <- c("Weakness","Resistance")

    return(resistance)
}

## Useless function--make color palette based on color values (deprecated by figuring out at= in levelplot()
## makecolorpalette <- function(type_eff){
##     allvals <- sort(unique(type_eff))
    
##     color_palette <- c()
    
##     k <- 0
    
##     for(i in 1:length(allvals)){
##         if(allvals[[i]]==0){
##             color_palette <- c(color_palette,rep("grey",k+1))
##             k <- k+1
##         }
##         else if(allvals[[i]]==0.25){
##             color_palette <- c(color_palette,rep("darkred",k+1))
##             k <- k+1
##         }else if(allvals[[i]]==0.5){
##             color_palette <- c(color_palette,rep("red",k+1))
##             k <- k+1
##         } else if(allvals[[i]]==1){
##             color_palette <- c(color_palette,rep("white",k+1))
##             k <- k+1
##         } else if (allvals[[i]]==2){
##             color_palette <- c(color_palette,rep("green",k+1))
##             k <- k+1
##         } else{
##             color_palette <- c(color_palette,rep("green4",k+1))
##             k <- k+1
##         }
##     }

##     last.color <- color_palette[length(color_palette)]
##     first.color <- color_palette[1]
    
##     #color_palette <- c(first.color,color_palette)

##     return(color_palette)
## }

shinyApp(ui=ui,server=server)
