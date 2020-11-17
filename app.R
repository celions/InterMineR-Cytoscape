## app.R ##

#Packages----
# load("Packages.R")

#Libraries----
library(plotly)
library(zip)
library(filesstrings)
library(shinydashboard)
library(shinycustomloader)
library(dashboardthemes)
library(doParallel)
library(MUVR)
library(DT)
library(mixOmics)
library(shinyalert)
library(httr)
library(curl)
library(RCurl)
library(XML)
library(RJSONIO)
library(sqldf)
library(Biostrings)
library(igraph)
library(S4Vectors)
library(methods)
library(xml2)
library(RCy3)
library(png)
library(fs)
library(zoom)
library(RCyjs)
library(cyjShiny)
library(htmlwidgets)
library(graph)
library(jsonlite)
library(R.utils)
library(cytoscape)
library(igraph)
library(shinyBS)
library(devtools)
library(webshot)
library(shinyCheckboxTree)
library(shinyFeedback)
library(rintrojs)
#library(rlist)
#library(InterMineR) #this line is giving an error, meanwhile I solve the problem you can load the functions saved in workspace_app.RData opening this file. 

#Load functions----
#load("workspace_app.RData")
#source("graphToJSON_function.R")
#source("InterMineR_functions.R")

# View----
#user interface should contain what the users see
ui <- dashboardPage(
  
  # title of browser tab
  title = "Data Visualizations with RCytoscape",
  
  
  dashboardHeader(
    title = tags$img(src = "intermine.png", width = "100%"),
    dropdownMenuOutput("dropdownmenu")
  ),
  
  dashboardSidebar(
    
    sidebarMenu( # inside the sidebar menu create a new menu tab
      id = "sideTabs", 
      ## add items to menu, set tabs names or icons
      menuItem("Home", tabName = "home", icon = icon("home")),
      #just as an example of a pop up message when hover (only valid for elements with ID)
      #same issue with functions of the package "cicerone"
      bsTooltip("mine_template", "Choose the mine.",
                "top", options = list(container = "body")),
      selectInput(
        inputId = "mine_template",
        shiny::HTML("<span style='color: white'>Select Mine</span></p>"),
        choices = names(listMines()),
        selected = "HumanMine"
      ),
      menuItem("1. Create your query", icon = icon("pencil-alt"),
               menuSubItem('1.1 Templates', tabName = 'templates'),
               menuSubItem('1.2 Query Builder', tabName = 'builder')
      ),
      menuItem("2. Run your query", tabName = "results", icon = icon("table")),
      menuItem("3. Visualize your results", tabName = 'visualization', icon = icon("chart-line")),
      menuItem("4. Style your Network Charts", tabName = "overlaid", icon = icon("palette")),
      menuItem("Saved Networks", tabName = "download", icon = icon("download"))
    )
  ),
  
  dashboardBody(
    
    # Add class fixed to header and sidebar
    tags$script(HTML("$('body').addClass('fixed');")),
    
    tabItems( # Create a tab items menu
      
      tabItem( # Create a new tab
        tabName = "home",
        includeMarkdown("home.md")
          ),
      
      tabItem( # add new item inside tab files
        tabName = "templates",
        
        introjsUI(),
        
        useShinyFeedback(), # include shinyFeedback
        
        tags$h3("Template queries"),
        tabPanel("Template queries", fluid = TRUE, 
                 
                 sidebarLayout(
                   sidebarPanel(
                     fluidRow(
                       column( # create a column
                         width = 12, # fill width
                         
                         uiOutput("template_mine"),
                         hr(),
                         uiOutput("template_choice"),
                         br(),
                         br(),
                         br(),
                         actionButton("help1","", icon = icon("question"), style="color: #000; background-color: #bdff80; border-color: #bdff80")
                         
                       ))),
                   mainPanel(
                     fluidRow(
                       uiOutput("index_choice"),
                       hr(),
                       br(),
                       uiOutput("template_constraint_summary"),
                       bsTooltip("template_constraint_summary","Summarize the information about the constraints contained by an object of the class InterMineR.","top")
                     )
                   )
                 )
                 
                 
        )
        
      ),
      
      tabItem(
        tabName = "builder", # Add items inside tab plots
        
        introjsUI(),
        
        useShinyFeedback(), # include shinyFeedback
        
        tags$h3("Query Builder"),
        br(),
        useShinyalert(),
        
        fluidRow(
          column(
            width = 12,
            uiOutput("builder_mine")
          ),
          hr(),
          box(
            width = 12,
            column( #essentials
              width = 4,
              uiOutput("builder_choice"),
              div(style="display:inline-block",actionButton("set_begin","SET"),actionButton("help2","", icon = icon("question"), style="color: #000; background-color: #bdff80; border-color: #bdff80")),
              br(),
              br(),
              uiOutput("builder_select_view_1"),
              selectInput(
                inputId = "operator_0",
                label = "Constraint operator:",
                choices = c("=", "!=", "LOOKUP", "ONE OF", "NONE OF", ">", "<", ">=", "<=", "LIKE"),
                multiple = TRUE
              ),
              textInput(
                inputId = "value_0",
                label = "Value(s) separated by commas for the constraint",
                value = ""
              ),
              div(style="display:inline-block; float:center",
                  actionButton("constraint_button_1", "CONSTRAINTS")),
              conditionalPanel(
                "input.constraint_button_1",
                selectInput(
                  inputId = "b_constraint_1",
                  label = "Type of data for another constraint",
                  choices = c(),
                  multiple = TRUE
                ),
                selectInput(
                  inputId = "operator_1",
                  label = "Operator",
                  choices = c("=", "!=", "LOOKUP", "ONE OF", "NONE OF", ">", "<", ">=", "<=", "LIKE"),
                  multiple = TRUE
                ),
                textInput(
                  inputId = "value_1",
                  label = "Value(s) separated by commas",
                  value = ""
                )
              )
            ),
            column(
              width = 4,
              uiOutput("builder_2_select")
            ),
            column(
              width = 4,
              uiOutput("builder_3_select")
            )
            
          ),
          column(
            width = 12,
            div(style="float:right",
                  actionButton("add_3","Overlay extra data")
                ),
            bsModal("add_data_type","Overlaying extra data","add_3",size = "large",
                    {fluidRow(
                      column(
                        width = 6,
                        uiOutput("builder_4_select")
                      ),
                      column(
                        width = 6,
                        uiOutput("builder_5_select")
                      )
                    )
                    })
          ),
          br(),
          br(),
          br(),
          br(),
          column(
            width = 12,
            div(style="display: inline-block;vertical-align:top; width: 150px;",uiOutput("builder_order")),
            div(style="display: inline-block;vertical-align:top; width: 60px;",HTML("<br>")),
            div(style="display: inline-block;vertical-align:top; width: 150px;",selectInput("desc_asc","and sort results in",
                                                                                            c("Ascending"="ASC","Descending"="DESC")))
            
          ),
          br(),
          column(
            width = 12,
            div(style="display: float:left",
                actionButton("setQ","SET QUERY", style="color: #fff; background-color: #6633FF; border-color: #6633FF"))
          ),
          bsModal("modal_queries", "SET QUERY", "setQ", size ="large",actionButton("delete_constraints", "Delete Rows"),tags$br(), tags$br(), dataTableOutput("table_constraints")),
          br(),
          hr(),
          conditionalPanel(
            "input.setQ",
            uiOutput("builder_constraint_summary"),
            bsTooltip("builder_constraint_summary","Summarize the information about the constraints contained by an object of the class InterMineR.","top")
          )
        )
      ),
      
      tabItem(
        tabName = "results",
        
        introjsUI(),
        
        useShinyFeedback(), # include shinyFeedback
        
        tags$h3("Query Results"),
        
        tabPanel( # Create a new tab
          title = "Table of results:", value="table",
          uiOutput("result_table")
        )
      ),
      
      tabItem(
        tabName = "visualization",
        
        introjsUI(),
        
        tags$h3("Cytoscape Network Viewer"),
        tabPanel("Cytoscape Network Viewer", fluid = TRUE, 
                 sidebarLayout(
                   sidebarPanel(
                     fluidRow(
                       column(
                         width = 12,
                         tags$legend("RCytoscape visualization options:"),
                         div(style="display:inline-block",
                             selectInput("doLayout", "Select Layout:",
                                         choices=c(#"",
                                           "cola",
                                           "cose",
                                           "circle",
                                           "concentric",
                                           "breadthfirst",
                                           "grid",
                                           "random",
                                           "dagre",
                                           "cose-bilkent")),
                             style="float:right"),
                         bsTooltip("doLayout", "Layout the current graph using the specified strategy."),
                         br(),
                         selectInput("selectName", "Select Node by ID:", choices = c()),
                         selectInput("selectName_2", "Select Node by attribute:", choices = c("")),
                         selectInput("selectName_2_attr","Values of the attribute:", choices = c()),
                         br(),
                         actionButton("help4","", icon = icon("question"), style="color: #000; background-color: #bdff80; border-color: #bdff80"),
                         br(),
                         br(),
                         br(),
                         br(),
                         div(style="display:inline-block; float:right",
                             textInput("filenameViewer", "Save as PNG with name:"),
                             actionButton("downloadviewer", "Save as PNG", style="color: #fff; background-color: #80aaff; border-color: #80aaff")),
                         bsTooltip("filenameViewer", "If no name is provided a default name will be given.")
                       )
                     )
                   ),
                   mainPanel(
                     fixedRow(
                       column(
                         width = 12,
                         box(
                           border = 0, 
                           width = 12,
                           height = 600,
                           cyjShinyOutput('cyjShiny', height = '600'),
                           style = "max-height: 100%; border: 0px solid; outline: none; outline-width: 0;"
                         ),
                         br(),
                         box(
                           width = 12,
                           div(style="display:inline-block",
                               actionButton("fitSelected", "Zoom Selected"),
                               bsTooltip("fitSelected", "Set zoom and center of the graph display so that the currently selected nodes fill the display."),
                               actionButton("fit", "Reset View"),
                               bsTooltip("fit", "Set zoom and center of the graph display so that graph fills the display."),
                               actionButton("sfn", "Select First Neighbor"),
                               bsTooltip("sfn", "Select First Neighbors of the currently selected nodes."),
                               actionButton("invertSelection", "Invert Selected"),
                               bsTooltip("invertSelection", "Invert the selection of the currently selected nodes."),
                               actionButton("clearSelection", "Unselect Nodes"),
                               bsTooltip("clearSelection", "All node and edge selections removed."),
                               style="float:right"),
                           br(),
                           div(style="display:inline-block; float:right",
                               actionButton("hideSelection", "Remove Selected"),
                               bsTooltip("hideSelection", "All selected nodes and their edges are hidden."),
                               actionButton("showAll", "Show All"),
                               bsTooltip("showAll", "All nodes and their edges are shown."),
                               style="float:right"),
                           br(),
                           br(),
                           div(style="display:inline-block; float:right",
                               actionButton("getSelectedNodes", "Get Selected Nodes"),
                               bsTooltip("getSelectedNodes", "Get the selected nodes IDs."),
                               style="float:right"),
                           br(),
                           br(),
                           htmlOutput("selectedNodesDisplay"),
                           hr(),
                           div(style="display:inline-block; float:right",
                               actionButton("goOverlaid1", "Go to Overlaying",
                                            style="color: #fff; background-color: #3366ff; border-color: #3366ff")
                           )
                         )
                         
                       )
                     )
                     
                   ),
                   fluid = FALSE))
      ),
      
      
      tabItem(
        tabName = "overlaid",
        
        introjsUI(),
        
        tags$h3("Style your Network Chart"),
        
        tabPanel("Cytoscape Network Style", fluid = TRUE, 
                 sidebarLayout(
                   sidebarPanel(
                     fluidRow(
                       column(
                         width = 12,
                         tags$legend("Options for Node's body:"),
                         selectInput("select_parameter", "Set Parameter", choices = 
                                       c("", "Background colour"="background-color",
                                         "Shape"="shape",
                                         "Size"="size"
                                       )),
                         bsTooltip("select_parameter", "Choose which feature from the graph you want to modify."),
                         selectInput("select_parameter_option", "Value", choices = c()),
                         bsTooltip("select_parameter_option", "Select the new value for the modification."),
                         hr(),
                         tags$p("Select Nodes by..."),
                         selectInput("selectid", "ID:", choices = c(), multiple = TRUE),
                         br(),
                         selectInput("selectName_3", "Attribute:", choices = c()),
                         bsTooltip("selectName_3", "To see more attributes return to Run your query Section and choose more node's attributes."),
                         selectInput("selectName_3_attr","Value", choices = c(), multiple = TRUE),
                         br(),
                         hr(),
                         div(style="display:inline-block",
                             actionButton("button_set", "Customize the network!", style="color: #fff; background-color: #ff9900; border-color: #ff9900"),
                             actionButton("help5","", icon = icon("question"), style="color: #000; background-color: #bdff80; border-color: #bdff80"),
                             style="float:right"),
                         tags$hr(),
                         br(),
                         br(),
                         br(),
                         br(),
                         div(style="display:inline-block; float:right",
                             actionButton("history","History of changes")),
                         bsModal("modal_history", "History of Changes", "history", size ="large",actionButton("deleteRows", "Delete Rows"),tags$br(), tags$br(), dataTableOutput("table1"))),
                       hr())
                   )
                   ,
                   mainPanel(
                     fluidRow(
                       column(
                         width = 12,
                         div(style="display:inline-block; float:right",
                             selectInput("layoutcytoscape", "Select Layout:",
                                         choices=c(
                                           'breadthfirst',
                                           'random', 
                                           'preset', 
                                           'grid', 
                                           'circle', 
                                           'concentric', 
                                           'cose',
                                           'cola'))),
                         bsTooltip("layoutcytoscape", "Layout the current graph using the specified strategy."),
                         cytoscapeOutput("network", height = "600px"),
                         br(),
                         br(),
                         conditionalPanel("isFinite(input.selectName_3_attr)",
                                          #http://bioconductor.riken.jp/packages/3.9/bioc/vignettes/RCyjs/inst/doc/RCyjs.html
                                          #https://js.cytoscape.org/#style/mappers
                                          actionButton("gradient_color","Apply a Gradient Color"),#mapData(weight, 0, 100, blue, red)
                                          actionButton("gradient_size", "Apply a Gradient Size")), 
                         br(),
                         br(),
                         br(),
                         br(),
                         div(style="display:inline-block; float:right",textInput("imagenameStyle", "Save as PNG with name:"),
                             downloadButton("ggsave_graph", "Save as PNG", style="color: #fff; background-color: #80aaff; border-color: #80aaff"),
                             #actionButton("save_graph_eps", "Save as EPS", style="color: #fff; background-color: #80aaff; border-color: #80aaff"),
                             bsTooltip("imagenameStyle", "If no name is provided a default name will be given."),
                             textInput("filenameStyle", "Save the ZIP folder with name:"),
                             bsTooltip("filenameStyle", "If no name is provided a default name will be given."),
                             downloadButton("downloadstyle", "Save as ZIP", style="color: #fff; background-color: #80aaff; border-color: #80aaff"),
                             actionButton("goDownload", "Go to Saved Workflows",
                                          style="color: #fff; background-color: #3366ff; border-color: #3366ff"),
                             bsTooltip("downloadstyle", "The ZIP folder contains the results of the query in a csv file, two csv files with the basic components and the modifications done to the Network and a JSON file of the Network.", "top"))
                         
                       )
                     )
                   )))
      ),
      
      tabItem(
        tabName = "download",
        
        introjsUI(),
        
        tags$h3("Saved Workflows"),
        tabPanel("Cytoscape Network Style", fluid = TRUE, 
                 fluidRow(
                   box(
                     width = 12,
                       column(
                         width = 12,
                         fileInput("file", "Upload Zip file", accept = ".zip"),
                         bsTooltip("file", "Here, you can upload saved networks and see the results of the query in a table and the interactive Network with your last modifications."),
                         actionButton("unzip", "Unzip files", style="color: #fff; background-color: #3366ff; border-color: #3366ff"),
                         bsTooltip("unzip", "Press this button to display the results table and the network."),
                         br(), # add a line break
                         br(),
                         textOutput("zipError") # Output error
                       )
                     
                   ),
                   conditionalPanel("input.unzip",{
                   box(
                     width = 12,
                     column(
                       width = 12,
                       withLoader(DTOutput("resultstable_save", height = "100%"), loader = "loader6"),
                       br(),
                       hr(),
                       cytoscapeOutput("network_saved", height = "600px")
                     )
                   )
                   })
                 )
                     
                   ))
      )
  )
)

#Server: where the data is processed-----
server <- function(input, output, session){
  # Close background process when user close app
  if (!interactive()) {
    session$onSessionEnded(function() {
      stopApp()
      q("no")
    })
  }
  ##### Error logs  #####
  ###### Redirect buttons #####
  # Update to another tab when user click's go to plots button
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("intro_text.html"),
      easyClose = TRUE,
      footer = tagList(
        div(style = "float:center;",
            actionButton(inputId = "intro", label = "HOME", icon = icon("info-circle"),style="color: #fff; background-color: #37D624; border-color: #37D624"))
      )
    ))
  })
  observeEvent(input$intro,{
    removeModal()
  })
  observeEvent(input$goResults, {
    
    newtab <- switch(input$sideTabs, "templates" = "results", "results" = "templates")
    updateTabItems(session, "sideTabs", newtab)
    
    updateTabsetPanel(session, "plotPanel",
                      selected = "results")
  })
  
  observeEvent(input$goBuilder, {
    
    newtab <- switch(input$sideTabs, "builder" = "results", "results" = "builder")
    updateTabItems(session, "sideTabs", newtab)
  })
  
  observeEvent(input$goInteraction, {
    
    newtab <- switch(input$sideTabs, "results" = "visualization", "visualization" = "results")
    updateTabItems(session, "sideTabs", newtab)
  })
  
  observeEvent(input$goOverlaid1, {
    
    newtab <- switch(input$sideTabs, "visualization"="overlaid", "overlaid"="visualization")
    updateTabItems(session, "sideTabs", newtab)
  })
  
  observeEvent(input$goDownload, {
    
    # Change sidebar tab to plot
    newtab <- switch(input$sideTabs, "overlaid" = "download", "download" = "overlaid")
    updateTabItems(session, "sideTabs", newtab)
  })
  
  output$result_table <- renderUI({
    #
    fluidRow(
      box(
        width = 12,
        column(
          width = 12,
          tags$legend("Returning results from a query against data held inside the mine."),
          withLoader(DTOutput("resultstable", height = "100%"), loader = "loader6"),
          bsTooltip("resultstable","Table containing the data which were retrieved from the InterMine instance."),
          div(style="display:inline-block",actionButton("options_button", "Set Nodes and Edges",style="color: #fff; background-color: #ff9900; border-color: #ff9900"),
              actionButton("help3","", icon = icon("question"), style="color: #000; background-color: #bdff80; border-color: #bdff80"))
          )),
      conditionalPanel(
        "input.options_button",
        box(
          width = 12,
          tags$legend("Set up the node and edges data frames:"),
          fluidRow(
            column(
              width = 6,
              tags$legend("Target"),
              selectInput("id_nodes", "id:", choices = c()),
              bsTooltip("id_nodes","Target node names."),
              selectInput("nodes_attributes", "Nodes attributes:", choices = c(),
                          multiple = TRUE),
              bsTooltip("nodes_attributes","Assign the supplied node attribute values to the graph structure for filter purpose.")
              ),
            column(
              width = 6,
              tags$legend("Source"),
              selectInput("id_edges", "source:", choices = c()),
              bsTooltip("id_nodes","Source node names."),
              selectInput("edges_attributes", "Edges attributes:", choices = c(),
                          multiple = TRUE))
          )),
        box(
          width = 12,
          div(style="display:inline-block; float:right",
              
              actionButton("goInteraction", "Create the Network",
                           style="color: #fff; background-color: #3366ff; border-color: #3366ff")
          ))
        
      )
    )
  })
  output$int_network <- renderUI({
    fluidRow(
      box(
        width = 12,
        column(
          width = 4,
          tags$legend("RCytoscape visualization options:"),
          div(style="display:inline-block",
              selectInput("doLayout", "Select Layout:",
                          choices=c(#"",
                            "cose",
                            "cola",
                            "circle",
                            "concentric",
                            "breadthfirst",
                            "grid",
                            "random",
                            "dagre",
                            "cose-bilkent")),
              actionButton("general_button","General options"),
              style="float:right"),
          conditionalPanel(
            "input.general_button",
            br(),
            selectInput("selectName", "Select Node by ID:", choices = c()),
            div(style="display:inline-block",
                actionButton("invertSelection", "Invert Selected"),
                actionButton("sfn", "Select First Neighbor"),
                style="float:right"),
            br(),
            div(style="display:inline-block",
                actionButton("fit", "Fit Graph"),
                actionButton("fitSelected", "Fit Selected"),
                style="float:right"),
            br(),
            div(style="display:inline-block",
                actionButton("hideSelection", "Hide Selected"),
                actionButton("showAll", "Show All"),
                style="float:right"),
            br(),
            div(style="display:inline-block",
                actionButton("getSelectedNodes", "Get Selected Nodes"),
                actionButton("clearSelection", "Unselect Nodes"),
                style="float:right"),
            br(),
            htmlOutput("selectedNodesDisplay")),
          
          hr()),
        column(
          width = 8,
          cyjShinyOutput('cyjShiny', height = '100%'),
          style = "background-color:#4d3a7d;" 
          #change the background color or do sth to save it with white background
        )),
      box(
        width = 12,
        div(style="display:inline-block; float:right",
            textInput("filenameViewer", "Save as PNG with name:"),
            actionButton("downloadviewer", "Save as PNG"),
            actionButton("goOverlaid1", "Go to Overlaying",
                         style="color: #fff; background-color: #3366ff; border-color: #3366ff")
        )))
  })
  
  
  get_2_choices <- function(nodes_list){
    output$builder_2_select <- renderUI({
      tagList(
        tags$h4(paste0("2nd level: ", input$b_choice)),
        tags$b("Type of data to be returned"),
        checkboxTreeInput("b_select_2",
                          nodes = nodes_list),
        bsTooltip("b_select_2", "Select from this tree the data you want to see in the results table. The 3rd level will depend on what you select here, so maybe you need to select data types in this tree and then delete them in the last step (SET QUERY) to see more levels.",
                  placement = "top"),
        tags$br(),
        tags$b("Type of data for the constraint"),
        checkboxTreeInput("b_constraint_2",
                          nodes = nodes_list),
        bsTooltip("b_constraint_2", "Select from this tree the data you want to constraint against.", placement = "top"),
        br(),
        br(),
        selectInput(
          inputId = "operator_2",
          label = "Operator",
          choices = c("=", "!=", "LOOKUP", "ONE OF", "NONE OF", ">", "<", ">=", "<=", "LIKE"),
          multiple = TRUE
        ),
        bsTooltip("operator_2", "Select the operators in the same order as the data is displayed in the above tree.", placement = "top"),
        textInput(
          inputId = "value_2",
          label = "Value(s) separated by commas",
          value = ""
        ),
        bsTooltip("value_2", "Type the values of the constraints separeted by a semicolon (;) if they are for different data types of the tree and so different constraints or by a comma (,) if they are going to be included in the same constraint."),
        actionButton("set_choice3","SET")
      )
      
    })
  }
  
  get_3_choices <- function(nodes_list){
    output$builder_3_select <- renderUI({
      tagList(
        tags$h4("3rd level: "),
        tags$b("Type of data to be returned"),
        checkboxTreeInput("b_select_3",
                          nodes = nodes_list),
        tags$br(),
        tags$b("Type of data for the constraint"),
        checkboxTreeInput("b_constraint_3",
                          nodes = nodes_list),
        selectInput(
          inputId = "operator_3",
          label = "Operator",
          choices = c("=", "!=", "LOOKUP", "ONE OF", "NONE OF", ">", "<", ">=", "<=", "LIKE"),
          multiple = TRUE
        ),
        textInput(
          inputId = "value_3",
          label = "Value(s) separated by commas",
          value = ""
        )
      )
    })
  }
  
  get_4_choices <- function(nodes_list){
    output$builder_4_select <- renderUI({
      tagList(
        tags$h4("4th level: "),
        tags$b("Type of data to be returned"),
        checkboxTreeInput("b_select_4",
                          nodes = nodes_list),
        tags$br(),
        tags$b("Type of data for the constraint"),
        checkboxTreeInput("b_constraint_4",
                          nodes = nodes_list),
        selectInput(
          inputId = "operator_4",
          label = "Operator",
          choices = c("=", "!=", "LOOKUP", "ONE OF", "NONE OF", ">", "<", ">=", "<=", "LIKE"),
          multiple = TRUE
        ),
        textInput(
          inputId = "value_4",
          label = "Value(s) separated by commas",
          value = ""
        ),
        actionButton("set_choice5","SET")
      )
    })
  }
  
  get_5_choices <- function(nodes_list){
    output$builder_5_select <- renderUI({
      tagList(
        tags$h4("5th level: "),
        tags$b("Type of data to be returned"),
        checkboxTreeInput("b_select_5",
                          nodes = nodes_list),
        tags$br(),
        tags$b("Type of data for the constraint"),
        checkboxTreeInput("b_constraint_5",
                          nodes = nodes_list),
        selectInput(
          inputId = "operator_5",
          label = "Operator",
          choices = c("=", "!=", "LOOKUP", "ONE OF", "NONE OF", ">", "<", ">=", "<=", "LIKE"),
          multiple = TRUE
        ),
        textInput(
          inputId = "value_5",
          label = "Value(s) separated by commas",
          value = ""
        )
      )
    })
  }
  
  get_select_view_1 <- function(){
    output$builder_select_view_1 <- renderUI({
      selectInput(
        inputId = "b_select_1",
        label = "Type of data to be returned",
        choices = c(),
        multiple = TRUE
      )
      
    })
  }
  
  get_select_constraint_1 <- function(){
    output$builder_select_constraint_1 <- renderUI({
      selectInput(
        inputId = "b_constraint_1",
        label = "Type of data for another constraint",
        choices = c(),
        multiple = TRUE
      )
    })
  }
  
  get_select_view_2 <- function(){
    output$builder_select_view_2 <- renderUI({
      selectInput(
        inputId = "b_select_2",
        label = "Type of data to be returned",
        choices = c(),
        multiple = TRUE
      )
    })
  }
  
  get_select_constraint_2 <- function(){
    output$builder_select_constraint_2 <- renderUI({
      selectInput(
        inputId = "b_constraint_2",
        label = "Type of data for the constraint",
        choices = c(),
        multiple = TRUE
      )
    })
  }
  
  get_select_view_3 <- function(){
    output$builder_select_view_3 <- renderUI({
      selectInput(
        inputId = "b_select_3",
        label = "Type of data to be returned",
        choices = c(),
        multiple = TRUE
      )
    })
  }
  
  get_select_constraint_3 <- function(){
    output$builder_select_constraint_3 <- renderUI({
      selectInput(
        inputId = "b_constraint_3",
        label = "Type of data for the constraint",
        choices = c(),
        multiple = TRUE
      )
    })
  }
  
  get_select_view_4 <- function(){
    output$builder_select_view_4 <- renderUI({
      selectInput(
        inputId = "b_select_4",
        label = "Type of data to be returned",
        choices = c(),
        multiple = TRUE
      )
    })
  }
  
  get_select_constraint_4 <- function(){
    output$builder_select_constraint_4 <- renderUI({
      selectInput(
        inputId = "b_constraint_4",
        label = "Type of data for the constraint",
        choices = c(),
        multiple = TRUE
      )
    })
  }
  
  get_select_view_5 <- function(){
    output$builder_select_view_5 <- renderUI({
      selectInput(
        inputId = "b_select_5",
        label = "Type of data to be returned",
        choices = c(),
        multiple = TRUE
      )
    })
  }
  
  get_select_constraint_5 <- function(){
    output$builder_select_constraint_5 <- renderUI({
      selectInput(
        inputId = "b_constraint_5",
        label = "Type of data for the constraint",
        choices = c(),
        multiple = TRUE
      )
    })
  }
  
  
  get_order <- function(){
    output$builder_order <- renderUI({
      selectInput(
        inputId = "b_order",
        label = "Order by",
        choices = ""
      )
    })
  }
  
  get_index_val <- function(){
    output$index_choice <- renderUI({
      fluidPage(
        column(
          width = 6,
          selectInput(
            inputId = "m.index_t1",
            label = "Choose the path:",
            choices = "",
            multiple = TRUE
          ),
          textInput(
            inputId = "values_t1",
            label = "Type"
          ),
          br(),
          selectInput(
            inputId = "m.index_t2",
            label = "Choose the path:",
            choices = "",
            multiple = TRUE
          ),
          textInput(
            inputId = "values_t2",
            label = "Type",#,
            #value = "DNA repair, cellular response to DNA damage stimulus"
            value = NULL
          )
        ),
        column(
          width = 6,
          selectInput(
            inputId = "m.index_t3",
            label = "Choose the path:",
            choices = "",
            multiple = TRUE
          ),
          textInput(
            inputId = "values_t3",
            label = "Type",#,
            #value = "DNA repair, cellular response to DNA damage stimulus"
            value = NULL
          ),
          br(),
          selectInput(
            inputId = "m.index_t4",
            label = "Choose the path:",
            choices = "",
            multiple = TRUE
          ),
          textInput(
            inputId = "values_t4",
            label = "Type",#,
            #value = "DNA repair, cellular response to DNA damage stimulus"
            value = NULL
          )
        ) 
      )
      
    }) 
  }
  get_constraint_val <- function(){
    
    output$constraint_1 <- renderUI({
      fluidRow(
        selectInput(
          inputId = "operator_1",
          label = "Operator",
          choices = c("=", "!=", "LOOKUP", "ONE OF", "NONE OF", ">", "<", ">=", "<=", "LIKE"),
          multiple = TRUE
        ),
        textInput(
          inputId = "value_1",
          label = "Value or values separated by commas",
          value = ""
        ))})
    output$constraint_2 <- renderUI({
      fluidRow(
        selectInput(
          inputId = "operator_2",
          label = "Operator",
          choices = c("=", "!=", "LOOKUP", "ONE OF", "NONE OF", ">", "<", ">=", "<=", "LIKE"),
          multiple = TRUE
        ),
        textInput(
          inputId = "value_2",
          label = "Value or values separated by commas",
          value = ""
        ))})
    output$constraint_3 <- renderUI({
      fluidRow(
        selectInput(
          inputId = "operator_3",
          label = "Operator",
          choices = c("=", "!=", "LOOKUP", "ONE OF", "NONE OF", ">", "<", ">=", "<=", "LIKE"),
          multiple = TRUE
        ),
        textInput(
          inputId = "value_3",
          label = "Value or values separated by commas",
          value = ""
        ))})
    output$constraint_4 <- renderUI({
      fluidRow(
        selectInput(
          inputId = "operator_4",
          label = "Operator",
          choices = c("=", "!=", "LOOKUP", "ONE OF", "NONE OF", ">", "<", ">=", "<=", "LIKE"),
          multiple = TRUE
        ),
        textInput(
          inputId = "value_4",
          label = "Value or values separated by commas",
          value = ""
        ))})
    output$constraint_5 <- renderUI({
      fluidRow(
        selectInput(
          inputId = "operator_5",
          label = "Operator",
          choices = c("=", "!=", "LOOKUP", "ONE OF", "NONE OF", ">", "<", ">=", "<=", "LIKE"),
          multiple = TRUE
        ),
        textInput(
          inputId = "value_5",
          label = "Value or values separated by commas",
          value = ""
        ))})
  }
  
  
  ##### Change theme #####
  
  # call to module to change the appearance of the page
  #callModule(module = serverChangeTheme, id = "moduleChangeTheme")
  
  
  ##### Render view #####
  ##### Visualization data type #####
  
  modality <- reactiveVal()
  observeEvent(input$sideTabs, {
    if (input$sideTabs == "builder") {
      shinyalert(
        title = "This is the Query Builder modality",
        text = "It is only encourage for experienced users.",
        type = "",
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        showCancelButton = FALSE,
        showConfirmButton = TRUE,
        inputType = "text",
        inputValue = "",
        inputPlaceholder = "",
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        cancelButtonText = "Cancel",
        timer = 0,
        animation = TRUE,
        imageUrl = NULL,
        imageWidth = 100,
        imageHeight = 100,
        className = "",
        callbackR = function(x) { modality(TRUE) },
        callbackJS = NULL,
        inputId = "shinyalert"
      )}
  })
  observeEvent(input$sideTabs, {
    if (input$sideTabs == "templates") {
      modality(NULL)
    }
  })
  
  
  im <- ""
  im <- reactive({initInterMine(mine = listMines()[input$mine_template])})
  model_im <- reactive({getModel(initInterMine(mine = listMines()[input$mine_template]))})
  
  output$template_mine <- renderUI({
    tags$h4(paste0("You are querying in ", input$mine_template, "."))
  })
  
  output$template_choice <- renderUI({
      selectInput(
        inputId = "t_choice",
        label = "Choose a template query:",
        choices = getTemplates(im())[1],
        selected = "Gene_Interactions2"
      )
    
  })
  
  observeEvent(input$help1,
               if (input$sideTabs == "templates") {
                 introjs(session, options = list(
                   steps = data.frame(element = c("#template_choice", "#m.index_t1","#values_t1"),
                                      intro = c("When you select an element from this list a template query is given.",
                                                "To modify pre-defined queries with constraints, start by choosing a bioterm.",
                                                "Here type the new value for the constraint."))
                 ))
               }
  )
  
  
  q_reactive <- eventReactive(input$t_choice,{
    getTemplateQuery(im(), input$t_choice)
  })
  
  get_index_val()
  observeEvent(input$t_choice,{
    updateSelectInput(session, "m.index_t1",
                      choices = q_reactive()[[1]]$where$path
    )})
  observeEvent(c(input$m.index_t1,input$values_t1), {
    req(input$m.index_t1)
    if(nchar(input$values_t1) < 1) {
      showFeedbackDanger(
        inputId = "values_t1",
        text = 'Type the value for the constraint.'
      )
    } else {
      hideFeedback(inputId = "values_t1")
    }
  })
  observeEvent(input$t_choice,{
    updateSelectInput(session, "m.index_t2",
                      choices = q_reactive()[[1]]$where$path
    )})
  observeEvent(c(input$m.index_t2,input$values_t2), {
    req(input$m.index_t2)
    if(nchar(input$values_t2) < 1) {
      showFeedbackDanger(
        inputId = "values_t2",
        text = 'Type the value for the constraint.'
      )
    } else {
      hideFeedback(inputId = "values_t2")
    }
  })
  observeEvent(input$t_choice,{
    updateSelectInput(session, "m.index_t3",
                      choices = q_reactive()[[1]]$where$path
    )})
  observeEvent(c(input$m.index_t3,input$values_t3), {
    req(input$m.index_t3)
    if(nchar(input$values_t3) < 1) {
      showFeedbackDanger(
        inputId = "values_t3",
        text = 'Type the value for the constraint.'
      )
    } else {
      hideFeedback(inputId = "values_t3")
    }
  })
  observeEvent(input$t_choice,{
    updateSelectInput(session, "m.index_t4",
                      choices = q_reactive()[[1]]$where$path
    )})
  observeEvent(c(input$m.index_t4,input$values_t4), {
    req(input$m.index_t4)
    if(nchar(input$values_t4) < 1) {
      showFeedbackDanger(
        inputId = "values_t4",
        text = 'Type the value for the constraint.'
      )
    } else {
      hideFeedback(inputId = "values_t4")
    }
  })
  q_query_reactive <- eventReactive(c(input$t_choice,input$m.index_t1,input$m.index_t2,
                                      input$m.index_t3,input$m.index_t4,
                                      input$values_t1,input$values_t2,input$values_t3,
                                      input$values_t4),{
                                        q <- q_reactive()
                                        ind_1 <- input$m.index_t1
                                        value_1 <- input$values_t1
                                        if(!(is.null(ind_1))){
                                          q_constraints <- setConstraints(
                                            values = list(c(input$values_t1)),
                                            modifyQueryConstraints = q,
                                            m.index = match(ind_1,q[[1]]$where$path)
                                          )
                                          ind_2 <- input$m.index_t2
                                          value_2 <- input$values_t2
                                          if(!(is.null(ind_2))){
                                            q_constraints <- setConstraints(
                                              values = list(value_1, value_2),
                                              modifyQueryConstraints = q,
                                              m.index = c(match(ind_1,q[[1]]$where$path), 
                                                          match(ind_2,q[[1]]$where$path))
                                            )
                                            ind_3 <- input$m.index_t3
                                            value_3 <- input$values_t3
                                            if(!(is.null(ind_3))){
                                              q_constraints <- setConstraints(
                                                values = list(value_1, value_2, value_3),
                                                modifyQueryConstraints = q,
                                                m.index = c(match(ind_1,q[[1]]$where$path), 
                                                            match(ind_2,q[[1]]$where$path), 
                                                            match(ind_3,q[[1]]$where$path))
                                              )
                                              ind_4 <- input$m.index_t4
                                              value_4 <- input$values_t4
                                              if(!(is.null(ind_4))){
                                                q_constraints <- setConstraints(
                                                  values = list(value_1, value_2, value_3, value_4),
                                                  modifyQueryConstraints = q,
                                                  m.index = c(match(ind_1,q[[1]]$where$path), 
                                                              match(ind_2,q[[1]]$where$path), 
                                                              match(ind_3,q[[1]]$where$path),
                                                              match(ind_4,q[[1]]$where$path))
                                                )
                                              }
                                            }
                                          }
                                          
                                          q_query <- setQuery(inheritQuery = q, where = q_constraints)
                                        }
                                        else{
                                          q_query <- q
                                        }
                                      }
  )
  
  observeEvent(c(input$t_choice,input$m.index_t1,input$m.index_t2,
                 input$m.index_t3,input$m.index_t4,
                 input$values_t1,input$values_t2,input$values_t3,
                 input$values_t4),{
                   output$template_constraint_summary <- renderUI({
                     tagList(
                       box(
                         width = 12, # fill the width of the page
                         tags$h2("Summary"),
                         if(is.list(q_query_reactive())){tags$p("No constraint set to template query.")}else{try(tags$p(summary(q_query_reactive())))},
                         div(style="display:inline-block; float:right",
                             actionButton("goResults","Go to Results",
                                          style="color: #fff; background-color: #3366ff; border-color: #3366ff"))
                       )
                     )
                   })
                   
                   
                   
                 })
  results_reactive <- eventReactive(c(input$t_choice,input$m.index_t1,input$m.index_t2,
                                      input$m.index_t3,input$m.index_t4,
                                      input$values_t1,input$values_t2,input$values_t3,
                                      input$values_t4,input$goResults),{
                                        q <- q_query_reactive()
                                        if(is.list(q)){
                                          res <- runQuery(im(), q)
                                        } else {
                                          res <- runQuery(im(), q)
                                        }
                                      })
  observeEvent(c(input$goBuilder, input$goResults),{
    output$resultstable <- renderDT({
      if(identical(modality(),NULL)){
        results <- datatable(as.data.frame(results_reactive()), fillContainer = TRUE, rownames = FALSE, options = list(
          pageLength = 25, autoWidth = TRUE))
        results
      }else{
        results <- datatable(as.data.frame(results_reactive_builder()), fillContainer = TRUE, rownames = FALSE, options = list(
          pageLength = 25, autoWidth = TRUE))
        results
      }
      
    })
    
  })
  
  
  observeEvent(input$options_button,{
    if(identical(modality(),NULL)){
      updateSelectInput(session, "id_nodes",
                        choices = names(results_reactive()))
    }else{
      updateSelectInput(session, "id_nodes",
                        choices = names(results_reactive_builder()))
    }
    
  })
  observeEvent(c(input$id_nodes,input$id_edges), {
    req(input$id_nodes)
    req(input$id_edges)
    if(input$id_nodes==input$id_edges) {
      showFeedbackDanger(
        inputId = "id_edges",
        text = 'Id and Source need to be different.'
      )
      showFeedbackDanger(
        inputId = "id_nodes",
        text = 'Id and Source need to be different.'
      )
    } else {
      hideFeedback(inputId = "id_edges")
      hideFeedback(inputId = "id_nodes")
    }
  })
  observeEvent(input$options_button,{
    if(identical(modality(),NULL)){
      updateSelectInput(session, "nodes_attributes",
                        choices = names(results_reactive()))
    }else{
      updateSelectInput(session, "nodes_attributes",
                        choices = names(results_reactive_builder()))
    }
    
  })
  observeEvent(input$options_button,{
    if(identical(modality(),NULL)){
      updateSelectInput(session, "id_edges",
                        choices = names(results_reactive()))
    }else{
      updateSelectInput(session, "id_edges",
                        choices = names(results_reactive_builder()))
    }
    
  })
  observeEvent(input$options_button,{
    if(identical(modality(),NULL)){
      updateSelectInput(session, "edges_attributes",
                        choices = names(results_reactive()))
    }else{
      updateSelectInput(session, "edges_attributes",
                        choices = names(results_reactive_builder()))
    }
    
  })
  
  interaction_reactive_func <- function(dataframe){
    df <- as.data.frame(dataframe)
    
    
    nodes <- data.frame(id = unique(c(df[,input$id_nodes], df[,input$id_edges])), 
                        stringsAsFactors = FALSE)
    
    edges <- df %>%
      dplyr::select(source = input$id_nodes,
                    target = input$id_edges) %>%
      dplyr::mutate(interaction = paste(source, '_', target)) 
    
    i_graph <- graph_from_data_frame(edges, directed = TRUE, nodes)
    
    for(element in input$edges_attributes){
      edge_attr(i_graph, element) <- df[,element]
    }
    for(element in input$nodes_attributes){
      df<- df[!duplicated(df[,input$id_nodes]),]
      vertex_attr(i_graph, element) <- df[,element]
    }
    
    g<-igraph.to.graphNEL(i_graph)
    
  }
  
  interaction_reactive <- eventReactive(c(input$t_choice,input$m.index_t1,input$m.index_t2,
                                          input$m.index_t3,input$m.index_t4,
                                          input$values_t1,input$values_t2,input$values_t3,
                                          input$values_t4,input$goResults,
                                          input$goInteraction,input$id_nodes,input$id_edges,input$nodes_attributes,input$edges_attributes),{
                                            
                                            interaction_reactive_func(results_reactive())
                                          })
  
  observeEvent(input$goInteraction,{
    if(identical(modality(),NULL)){
      updateSelectInput(session, "selectName",
                        choices = c("",nodes(interaction_reactive())))
    }else{
      updateSelectInput(session, "selectName",
                        choices = c("",nodes(interaction_reactive_builder())))
    }
  })
  
  observeEvent(input$goInteraction,{
    if(identical(modality(),NULL)){
      updateSelectInput(session, "delete_nodes",
                        choices = c(nodes(interaction_reactive())))
    }else{
      updateSelectInput(session, "delete_nodes",
                        choices = c(nodes(interaction_reactive_builder())))
    }
  })
  
  nodes_attr_reactive <- reactive({
    input$nodes_attributes
  })
  observeEvent(input$goInteraction, {
    if(identical(modality(),NULL)){
      updateSelectInput(session, "selectName_2",
                        choices = c("",nodes_attr_reactive()))
    }else{
      updateSelectInput(session, "selectName_2",
                        choices = c("",nodes_attr_reactive()))
    }
    
  })
  
  observeEvent(input$selectName_2, ignoreInit = TRUE, {
    if(identical(modality(),NULL)){
      df <- results_reactive()
      updateSelectInput(session, "selectName_2_attr",
                        choices = c("",df[,input$selectName_2]))
    }else{
      df <- results_reactive_builder()
      updateSelectInput(session, "selectName_2_attr",
                        choices = c("",df[,input$selectName_2]))
    }
    
  })
  
  observeEvent(input$goOverlaid1, {
    if(identical(modality(),NULL)){
      updateSelectInput(session, "selectName_3",
                        choices = c("",c(nodes_attr_reactive())))
    }else{
      updateSelectInput(session, "selectName_3",
                        choices = c("",c(nodes_attr_reactive())))
    }
    
  })
  
  observeEvent(input$selectName_3, ignoreInit = TRUE, {
    if(identical(modality(),NULL)){
      df <- new_df()
      updateSelectInput(session, "selectName_3_attr",
                        choices = c("",df[,input$selectName_3]))
    }else{
      df <- new_df_builder()
      updateSelectInput(session, "selectName_3_attr",
                        choices = c("",df[,input$selectName_3]))
    }
    
  })
  
  observeEvent(input$filters_button,{
    if(identical(modality(),NULL)){
      updateSelectInput(session, "setNodeAttributes",
                        choices = c("",nodes(interaction_reactive())))
    }else{
      updateSelectInput(session, "setNodeAttributes",
                        choices = c("",nodes(interaction_reactive())))
    }
    
  })
  
  observeEvent(input$selectName,  ignoreInit=TRUE,{
    printf("about to sendCustomMessage, selectNodes")
    session$sendCustomMessage(type="selectNodes", message=list(input$selectName))
  })
  
  observeEvent(input$selectName_2_attr,  ignoreInit=FALSE,{
    if(identical(modality(),NULL)){
      printf("about to sendCustomMessage, selectNodes")
      df <- results_reactive()
      
      node_i <- df[,input$selectName_2]==input$selectName_2_attr
      node <- df[node_i,input$id_nodes]
      
      for (element in node){
        session$sendCustomMessage(type="selectNodes", message=list(element))
      }
    }else{
      printf("about to sendCustomMessage, selectNodes")
      df <- results_reactive_builder()
      
      node_i <- df[,input$selectName_2]==input$selectName_2_attr
      node <- df[node_i,input$id_nodes]
      
      for (element in node){
        session$sendCustomMessage(type="selectNodes", message=list(element))
      }
    }
  })
  
  observeEvent(input$sfn,  ignoreInit=TRUE,{
    printf("about to sendCustomMessage, sfn")
    session$sendCustomMessage(type="sfn", message=list())
  })
  
  observeEvent(input$fit, ignoreInit=TRUE, {
    fit(session, 80)
  })
  
  observeEvent(input$fitSelected,  ignoreInit=TRUE,{
    printf("about to call R function fitSelected")
    fitSelected(session, 100)
  })
  
  observeEvent(input$getSelectedNodes, ignoreInit=TRUE, {
    output$selectedNodesDisplay <- renderText({" "})
    getSelectedNodes(session)
  })
  
  observeEvent(input$selectedNodes, {
    
    #  communicated here via assignement in cyjShiny.js
    #     Shiny.setInputValue("selectedNodes", value, {priority: "event"});
    newNodes <- input$selectedNodes;
    output$selectedNodesDisplay <- renderText({
      paste(newNodes)
    })
  })
  
  observeEvent(input$clearSelection,  ignoreInit=TRUE, {
    
    printf("about to sendCustomMessage, clearSelection")
    session$sendCustomMessage(type="clearSelection", message=list())
  })
  
  observeEvent(input$doLayout,  ignoreInit=TRUE,{
    
    strategy <- input$doLayout
    printf("about to sendCustomMessage, doLayout: %s", strategy)
    doLayout(session, strategy)
  })
  
  observeEvent(input$hideSelection, ignoreInit = TRUE, {
    hideSelection(session)
  })
  
  hidennodes <- reactiveVal()
  
  observeEvent(input$selectName,{
    if(identical(modality(),NULL)){
      hidennodes(c(hidennodes(),input$selectName))
    }else{
      hidennodes_builder(c(hidennodes_builder(),input$selectName))
    }
  })
  
  observeEvent(input$selectName_2_attr,{
    if(identical(modality(),NULL)){
      df <- results_reactive()
      
      node_i <- df[,input$selectName_2]==input$selectName_2_attr
      node <- df[node_i,input$id_nodes]
      
      for (element in node){
        hidennodes(c(hidennodes(),element))
      }
    }else{
      df <- results_reactive_builder()
      
      node_i <- df[,input$selectName_2]==input$selectName_2_attr
      node <- df[node_i,input$id_nodes]
      
      for (element in node){
        hidennodes_builder(c(hidennodes_builder(),element))
      }
    }
    
  })
  
  observeEvent(c(input$clearSelection, input$showAll),{
    if(identical(modality(),NULL)){
      hidennodes(NULL)
    }else{
      hidennodes_builder(NULL)
    }
  })
  
  new_df <- eventReactive(c(input$hideSelection, input$clearSelection, input$showAll, input$goOverlaid1),{
    #if(identical(modality(),NULL)){
    df <- results_reactive()
    for (element in hidennodes()) {
      df <- df[!df[,input$id_nodes]==element,]
    }#}else{
      #df <- results_reactive_builder()
      #for (element in hidennodes_builder()){
       # df <- df[!df[,input$id_nodes]==element,]
      #}
    #}
    df
  })
  
  observeEvent(input$invertSelection, ignoreInit = TRUE, {
    
    invertSelection(session)
  })
  
  observeEvent(input$showAll, ignoreInit = TRUE, {
    
    showAll(session)
  })
  
  output$cyjShiny <- renderCyjShiny({
    if(identical(modality(),NULL)){
      g3 <- interaction_reactive()
      graph <- graphToJSON(g3)
      cyjShiny(graph, layoutName="cola", height = 600)
    }else{
      g3 <- interaction_reactive_builder()
      graph <- graphToJSON(g3)
      cyjShiny(graph, layoutName="cola", height = 600)
    }
  })
  
  observeEvent(input$downloadviewer, ignoreInit=TRUE, {
    
    file.name <- tempfile(fileext=".png") #.png
    savePNGtoFile(session, file.name)
  })
  
  observeEvent(input$pngData, ignoreInit=TRUE, {
    
    printf("received pngData")
    png.parsed <- fromJSON(input$pngData)
    substr(png.parsed, 1, 30) # [1] "data:image/png;base64,iVBORw0K"
    nchar(png.parsed)  # [1] 768714
    png.parsed.headless <- substr(png.parsed, 23, nchar(png.parsed))  # chop off the uri header
    png.parsed.binary <- base64decode(png.parsed.headless)
    printf(paste0("writing png to ",input$filenameViewer,".png"))
    conn <- file(paste0(input$filenameViewer,".png"), "wb")
    writeBin(png.parsed.binary, conn)
    close(conn)
    
  })
  
  style_edges_reactive_func <- function(data_frame, id_nodes, id_edges){
    df <- data_frame
    
    nodes <- data.frame(id = unique(c(df[,id_nodes], df[,id_edges])), stringsAsFactors = FALSE)
    edges <- df %>%
      dplyr::select(source = all_of(id_nodes),
                    target = all_of(id_edges)) %>%
      dplyr::mutate(interaction = paste(source, '_', target))
  }
  
  style_edges_reactive <- eventReactive(c(input$t_choice,input$m.index_t1,input$m.index_t2,
                                          input$m.index_t3,input$m.index_t4,
                                          input$values_t1,input$values_t2,input$values_t3,
                                          input$values_t4,input$goResults,input$goOverlaid1, input$hideSelection, input$clearSelection, input$showAll, 
                                          input$goInteraction,input$id_nodes,input$id_edges,input$nodes_attributes,input$edges_attributes),{
                                            
                                            style_edges_reactive_func(new_df(),input$id_nodes, input$id_edges)
                                            
                                          })
  
  observeEvent(c(input$hideSelection, input$clearSelection, input$showAll, input$goOverlaid1),{
    if(identical(modality(),NULL)){
      plotInput <- cytoscape(nodes = style_custom_nodes_reactive(), edges = style_edges_reactive()) %>%
        #node_style(
        #  'background-fill' = 'radial-gradient',
        #  'background-gradient-stop-colors' = 'data(colors)', 
        #  'background-gradient-stop-positions' = '25 75 80') %>%
        layout('breadthfirst', directed = TRUE) %>%
        panzoom()
      saveWidget(plotInput, "temp.html", selfcontained = FALSE)
      output$network <- renderCytoscape({
        # draw the network
        cytoscape(nodes = style_custom_nodes_reactive(), edges = style_edges_reactive()) %>%
          #node_style(
          #  'background-fill' = 'radial-gradient',
          #  'background-gradient-stop-colors' = 'data(colors)', 
          #  'background-gradient-stop-positions' = '25 75 80') %>%
          layout('breadthfirst', directed = TRUE) %>%
          panzoom()
        
      })
    }else{
      plotInput <- cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>%
        layout('breadthfirst', directed = TRUE) %>%
        panzoom()
      saveWidget(plotInput, "temp.html", selfcontained = FALSE)
      output$network <- renderCytoscape({
        # draw the network
        cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>%
          layout('breadthfirst', directed = TRUE) %>%
          panzoom()
        
      })
    }
    
  })  
  
  observeEvent(c(input$hideSelection, input$clearSelection, input$showAll, input$goOverlaid1),{
    if(identical(modality(),NULL)){
      df <- new_df()
      options <- unique(c(df[,input$id_nodes], df[,input$id_edges]))
      
      updateSelectInput(session, "selectid",
                        choices = c("",options))
    }else{
      df <- new_df_builder()
      options <- unique(c(df[,input$id_nodes], df[,input$id_edges]))
      
      updateSelectInput(session, "selectid",
                        choices = c("",options))
    }
    
  })
  
  observeEvent(input$select_parameter,{
    
    if(input$select_parameter=="shape"){
      options <- c("ellipse",
                   "triangle",
                   "round-triangle",
                   "rectangle",
                   "round-rectangle",
                   "bottom-round-rectangle",
                   "cut-rectangle",
                   "barrel",
                   "rhomboid",
                   "diamond",
                   "round-diamond",
                   "pentagon",
                   "round-pentagon",
                   "hexagon",
                   "round-hexagon",
                   "concave-hexagon",
                   "heptagon",
                   "round-heptagon",
                   "octagon",
                   "round-octagon",
                   "star",
                   "tag",
                   "round-tag",
                   "vee")
    }else if(input$select_parameter=="size"){
      options <- c("10x","20px","50px","70px","90px","100px","150px")
    }else{
      options <- c("Orange"="#ff8c1a",
                   "Blue"="#99ccff",
                   "Dark Blue"="#0000cc",
                   "Forest Green"="#009900",
                   "Green"="#66ff66",
                   "Red"="#ff3300",
                   "Yellow"="#ffff4d",
                   "Purple"="#cc66ff",
                   "Pink"="#ff99cc",
                   "Grey"="#a6a6a6")
      
    }
    updateSelectInput(session, "select_parameter_option",
                      choices = c("", options))
  })
  
  rv <- NULL
  values <- reactiveValues(dfWorking = rv)
  
  observeEvent(input$deleteRows,{
    if(identical(modality(),NULL)){
      if (!is.null(input$table1_rows_selected)) {
        values$dfWorking <- values$dfWorking[-as.numeric(input$table1_rows_selected),]
      }
    }else{
      if (!is.null(input$table1_rows_selected)) {
        values_builder$dfWorking_builder <- values_builder$dfWorking_builder[-as.numeric(input$table1_rows_selected),]
      }
    }
    
  })

  
  observeEvent(c(input$button_set,input$layoutcytoscape, input$downloadstyle),{
    if(identical(modality(),NULL)){
      df <- new_df()
      node<-NULL
      node_i<-NULL
      try(node_i<-df[,input$selectName_3]==input$selectName_3_attr, silent = TRUE)
      try(node<-df[node_i,input$id_nodes], silent = TRUE)
      
      
      if(is.null(input$selectid)){
        if(is.null(input$selectName_3_attr)){
          strategy <- input$layoutcytoscape
        }else{
          for (element in data.frame(rbind(node,input$selectName_3_attr))){
            df_2 <- data.frame("Nodes"=element[1], "Attribute"=paste0(input$selectName_3," = ", element[2]), "Parameter"=input$select_parameter,"Selection"=input$select_parameter_option)
            values$dfWorking <- rbind(values$dfWorking, df_2)
            df_2 <- NULL
          }
        }
      }else{
        for (element in input$selectid){
          df_2 <- data.frame("Nodes"=element,"Attribute"="ID", "Parameter"=input$select_parameter,"Selection"=input$select_parameter_option)
          values$dfWorking <- rbind(values$dfWorking, df_2)        
          df_2 <- NULL
          
        }
      }
      
      isolate({
        options <- nodes(interaction_reactive())
        for (element in hidennodes()) {
          options <- options[options != element]
        }
        updateSelectInput(session, "selectid",
                          choices = c("",options))
      })
      
      isolate({
        updateSelectInput(session,"select_parameter",
                          choices = c("", "Background colour"="background-color",
                                      "Shape"="shape",
                                      "Size"="size"
                          ))
      })
      
      isolate({
        updateSelectInput(session, "selectName_3",
                          choices = c("",c(nodes_attr_reactive())))
        
      })
      
      if(is.null(values$dfWorking)){
        strategy <- input$layoutcytoscape
        printf("about to sendCustomMessage, layout: %s", strategy)
        
        if(strategy=="cola"){
          plotInput <- cytoscape(nodes = style_custom_nodes_reactive(), edges = style_edges_reactive()) %>% 
            cola_layout(avoidOverlap = TRUE) %>%
            panzoom()
          saveWidget(plotInput, "temp.html", selfcontained = FALSE)
          output$network <- renderCytoscape({
            #workflow_zip2(new_df(),values$dfWorking)
            cytoscape(nodes = style_custom_nodes_reactive(), edges = style_edges_reactive()) %>% 
              cola_layout(avoidOverlap = TRUE) %>%
              panzoom()
          })
        }else{
          plotInput <- cytoscape(nodes = style_custom_nodes_reactive(), edges = style_edges_reactive()) %>% 
            layout(strategy, avoidOverlap = TRUE) %>%
            panzoom()
          saveWidget(plotInput, "temp.html", selfcontained = FALSE)
          output$network <- renderCytoscape({
            #workflow_zip2(new_df(),values$dfWorking)
            cytoscape(nodes = style_custom_nodes_reactive(), edges = style_edges_reactive()) %>% 
              layout(strategy, avoidOverlap = TRUE) %>%
              panzoom()
          })
        }
      }else{
        strategy <- input$layoutcytoscape
        if(strategy == "cola"){
          #workflow_zip2(new_df(),values$dfWorking)
          plotInput <- cytoscape(nodes = style_custom_nodes_reactive(), edges = style_edges_reactive()) %>% 
            node_style('background-color' = 'data(node_color)') %>%
            node_style('shape' = 'data(node_shape)') %>%
            node_style('width' = 'data(node_width)') %>%
            node_style('height' = 'data(node_height)') %>%
            #node_style(
            #  'background-fill' = 'radial-gradient',
            #  'background-gradient-stop-colors' = 'data(colors)', 
            #  'background-gradient-stop-positions' = '25 75 80') %>%
            #node_style("background-fill" = "radial-gradient") %>%
            #node_style("background-gradient-stop-colors" = "data(colors)") %>%
            #node_style("background-gradient-stop-positions" = "25 400 8000") %>%
            cola_layout(avoidOverlap = TRUE) %>%
            panzoom()
        }else{
          #workflow_zip2(new_df(),values$dfWorking)
          plotInput <- cytoscape(nodes = style_custom_nodes_reactive(), edges = style_edges_reactive()) %>% 
            node_style('background-color' = 'data(node_color)') %>%
            node_style('shape' = 'data(node_shape)') %>%
            node_style('width' = 'data(node_width)') %>%
            node_style('height' = 'data(node_height)') %>%
            #node_style(
            #  'background-fill' = 'radial-gradient',
            #  'background-gradient-stop-colors' = 'data(colors)', 
            #  'background-gradient-stop-positions' = '25 75 80') %>%
            
            #node_style('background-fill' = 'radial-gradient') %>%
            #node_style('background-gradient-stop-colors' = 'data(colors)') %>%
            #node_style('background-gradient-stop-positions' = '25 75 80') %>%
            layout(strategy, avoidOverlap = TRUE) %>%
            panzoom()
        }
        saveWidget(plotInput, "temp.html", selfcontained = FALSE)
        output$network <- renderCytoscape({
          strategy <- input$layoutcytoscape
          printf("about to sendCustomMessage, layout: %s", strategy)
          if(strategy == "cola"){
            #workflow_zip2(new_df(),values$dfWorking)
            cytoscape(nodes = style_custom_nodes_reactive(), edges = style_edges_reactive()) %>% 
              node_style('background-color' = 'data(node_color)') %>%
              node_style('shape' = 'data(node_shape)') %>%
              node_style('width' = 'data(node_width)') %>%
              node_style('height' = 'data(node_height)') %>%
              #node_style(
              #  'background-fill' = 'radial-gradient',
              #  'background-gradient-stop-colors' = 'data(colors)', 
              #  'background-gradient-stop-positions' = '25 75 80') %>%
              #node_style("background-fill" = "radial-gradient") %>%
              #node_style("background-gradient-stop-colors" = "data(colors)") %>%
              #node_style("background-gradient-stop-positions" = "25 400 8000") %>%
              cola_layout(avoidOverlap = TRUE) %>%
              panzoom()
          }else{
            #workflow_zip2(new_df(),values$dfWorking)
            cytoscape(nodes = style_custom_nodes_reactive(), edges = style_edges_reactive()) %>% 
              node_style('background-color' = 'data(node_color)') %>%
              node_style('shape' = 'data(node_shape)') %>%
              node_style('width' = 'data(node_width)') %>%
              node_style('height' = 'data(node_height)') %>%
              #node_style(
              #  'background-fill' = 'radial-gradient',
              #  'background-gradient-stop-colors' = 'data(colors)', 
              #  'background-gradient-stop-positions' = '25 75 80') %>%
              
              #node_style('background-fill' = 'radial-gradient') %>%
              #node_style('background-gradient-stop-colors' = 'data(colors)') %>%
              #node_style('background-gradient-stop-positions' = '25 75 80') %>%
              layout(strategy, avoidOverlap = TRUE) %>%
              panzoom()
          }
        })
      }
      
      node <- NULL
      node_i <- NULL
    }else{
      df <- new_df_builder()
      node<-NULL
      node_i<-NULL
      try(node_i<-df[,input$selectName_3]==input$selectName_3_attr, silent = TRUE)
      try(node<-df[node_i,input$id_nodes], silent = TRUE)
      
      
      if(is.null(input$selectid)){
        if(is.null(input$selectName_3_attr)){
          strategy <- input$layoutcytoscape
        }else{
          for (element in data.frame(rbind(node,input$selectName_3_attr))){
            df_2 <- data.frame("Nodes"=element[1], "Attribute"=paste0(input$selectName_3," = ", element[2]), "Parameter"=input$select_parameter,"Selection"=input$select_parameter_option)
            values_builder$dfWorking_builder <- rbind(values_builder$dfWorking_builder, df_2)
            df_2 <- NULL
          }
        }
      }else{
        for (element in input$selectid){
          df_2 <- data.frame("Nodes"=element,"Attribute"="ID", "Parameter"=input$select_parameter,"Selection"=input$select_parameter_option)
          values_builder$dfWorking_builder <- rbind(values_builder$dfWorking_builder, df_2)        
          df_2 <- NULL
          
        }
      }
      
      isolate({
        options <- nodes(interaction_reactive_builder())
        for (element in hidennodes_builder()) {
          options <- options[options != element]
        }
        updateSelectInput(session, "selectid",
                          choices = c("",options))
      })
      
      isolate({
        updateSelectInput(session,"select_parameter",
                          choices = c("", "Background colour"="background-color",
                                      "Shape"="shape",
                                      "Size"="size"
                          ))
      })
      
      isolate({
        updateSelectInput(session, "selectName_3",
                          choices = c("",c(nodes_attr_reactive())))
        
      })
      
      if(is.null(values_builder$dfWorking_builder)){
        strategy <- input$layoutcytoscape
        printf("about to sendCustomMessage, layout: %s", strategy)
        plotInput <- cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
          cola_layout(avoidOverlap = TRUE) %>%
          panzoom()
        saveWidget(plotInput, "temp.html", selfcontained = FALSE)
        if(strategy=="cola"){
          output$network <- renderCytoscape({
            #workflow_zip2(new_df(),values$dfWorking)
            cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
              cola_layout(avoidOverlap = TRUE) %>%
              panzoom()
          })
        }else{
          plotInput <- cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
            layout(strategy, avoidOverlap = TRUE) %>%
            panzoom()
          saveWidget(plotInput, "temp.html", selfcontained = FALSE)
          output$network <- renderCytoscape({
            #workflow_zip2(new_df(),values$dfWorking)
            cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
              layout(strategy, avoidOverlap = TRUE) %>%
              panzoom()
          })
        }
      }else{
        strategy <- input$layoutcytoscape
        if(strategy == "cola"){
          #workflow_zip2(new_df(),values$dfWorking)
          plotInput <- cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
            node_style('background-color' = 'data(node_color)') %>%
            node_style('shape' = 'data(node_shape)') %>%
            node_style('width' = 'data(node_width)') %>%
            node_style('height' = 'data(node_height)') %>%
            cola_layout(avoidOverlap = TRUE) %>%
            panzoom()
        }else{
          #workflow_zip2(new_df(),values$dfWorking)
          plotInput <- cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
            node_style('background-color' = 'data(node_color)') %>%
            node_style('shape' = 'data(node_shape)') %>%
            node_style('width' = 'data(node_width)') %>%
            node_style('height' = 'data(node_height)') %>%
            layout(strategy, avoidOverlap = TRUE) %>%
            panzoom()
        }
        saveWidget(plotInput, "temp.html", selfcontained = FALSE)
        output$network <- renderCytoscape({
          strategy <- input$layoutcytoscape
          printf("about to sendCustomMessage, layout: %s", strategy)
          if(strategy == "cola"){
            #workflow_zip2(new_df(),values$dfWorking)
            cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
              node_style('background-color' = 'data(node_color)') %>%
              node_style('shape' = 'data(node_shape)') %>%
              node_style('width' = 'data(node_width)') %>%
              node_style('height' = 'data(node_height)') %>%
              cola_layout(avoidOverlap = TRUE) %>%
              panzoom()
          }else{
            #workflow_zip2(new_df(),values$dfWorking)
            cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
              node_style('background-color' = 'data(node_color)') %>%
              node_style('shape' = 'data(node_shape)') %>%
              node_style('width' = 'data(node_width)') %>%
              node_style('height' = 'data(node_height)') %>%
              layout(strategy, avoidOverlap = TRUE) %>%
              panzoom()
          }
        })
      }
      
      node <- NULL
      node_i <- NULL
    }
  })
  observe({
    output$ggsave_graph <- downloadHandler(
      filename = function() {ifelse(input$imagenameStyle=="", paste0("network",format(Sys.time(), "%m%d_%H%M"),".png"), #png
                                    paste0(input$imagenameStyle,".png"))},
      content = function(file) {
        webshot("temp.html", file = file , cliprect = "viewport")
        #as I am using this function I cannot save in vectorial format
      }
    )
  })
  #observeEvent(input$save_graph_eps,{
    #setEPS()
    
    #postscript("whatever.eps")
    #cytoscape(nodes = style_custom_nodes_reactive(), edges = style_edges_reactive())
    
    #dev.off()
  #})
  observe({
    if(identical(modality(),NULL)){
      output$downloadstyle <- downloadHandler(
        filename <- function() {
          ifelse(input$filenameStyle=="",paste0("workflow_final",format(Sys.time(), "%m%d_%H%M"),".zip"), # default name of the zip
                 paste0(input$filenameStyle,".zip"))
        },
        content <- function(file) {
          temp <- tempdir() # Set a temp dir
          setwd(tempdir())
          # Create the files
          if(is.null(values$dfWorking)){
            results_table_path <- paste0("results_",format(Sys.time(), "%m%d_%H%M"),".csv")
            customization_table_path <- paste0("customization_",format(Sys.time(), "%m%d_%H%M"),".txt")
            ids_path <- paste0("ids_",format(Sys.time(), "%m%d_%H%M"),".csv")
            json_path <- paste0("network_",format(Sys.time(), "%m%d_%H%M"),".json")
            
            utils::write.csv(new_df(), results_table_path, row.names = TRUE)
            write("", customization_table_path)
            utils::write.csv(data.frame(nodes=input$id_nodes, edges=input$id_edges), ids_path)
            write(dataFramesToJSON(style_edges_reactive(), style_custom_nodes_reactive()),json_path) 
            # Create a zip of the data
            zip::zipr(zipfile = file, files = c(results_table_path,
                                                customization_table_path, 
                                                ids_path, json_path))
          }else{
            results_table_path <- paste0("results_",format(Sys.time(), "%m%d_%H%M"),".csv")
            customization_table_path <- paste0("customization_",format(Sys.time(), "%m%d_%H%M"),".csv")
            ids_path <- paste0("ids_",format(Sys.time(), "%m%d_%H%M"),".csv")
            json_path <- paste0("network_",format(Sys.time(), "%m%d_%H%M"),".json")
            
            utils::write.csv(new_df(), results_table_path, row.names = TRUE)
            utils::write.csv(values$dfWorking, customization_table_path, row.names = TRUE)
            utils::write.csv(data.frame(nodes=input$id_nodes, edges=input$id_edges), ids_path)
            write(dataFramesToJSON(style_edges_reactive(), style_custom_nodes_reactive()),json_path) 
            # Create a zip of the data
            zip::zipr(zipfile = file, files = c(results_table_path, customization_table_path, ids_path, json_path))
          }
        },
        contentType = "application/zip")
    }else{
      output$downloadstyle <- downloadHandler(
        filename <- function() {
          ifelse(input$filenameStyle=="",paste0("workflow_final",format(Sys.time(), "%m%d_%H%M"),".zip"), # default name of the zip
                 paste0(input$filenameStyle,".zip"))
        },
        content <- function(file) {
          temp <- tempdir() # Set a temp dir
          setwd(tempdir())
          # Create the files
          if(is.null(values_builder$dfWorking_builder)){
            results_table_path <- paste0("results_",format(Sys.time(), "%m%d_%H%M"),".csv")
            customization_table_path <- paste0("customization_",format(Sys.time(), "%m%d_%H%M"),".txt")
            ids_path <- paste0("ids_",format(Sys.time(), "%m%d_%H%M"),".csv")
            json_path <- paste0("network_",format(Sys.time(), "%m%d_%H%M"),".json")
            
            utils::write.csv(new_df_builder(), results_table_path, row.names = TRUE)
            write("", customization_table_path)
            utils::write.csv(data.frame(nodes=input$id_nodes, edges=input$id_edges), ids_path)
            write(dataFramesToJSON(style_edges_reactive(), style_custom_nodes_reactive()),json_path)
            # Create a zip of the data
            zip::zipr(zipfile = file, files = c(results_table_path, 
                                                customization_table_path, 
                                                ids_path, json_path))
          }else{
            results_table_path <- paste0("results_",format(Sys.time(), "%m%d_%H%M"),".csv")
            customization_table_path <- paste0("customization_",format(Sys.time(), "%m%d_%H%M"),".csv")
            ids_path <- paste0("ids_",format(Sys.time(), "%m%d_%H%M"),".csv")
            json_path <- paste0("network_",format(Sys.time(), "%m%d_%H%M"),".json")
            
            utils::write.csv(new_df_builder(), results_table_path, row.names = TRUE)
            utils::write.csv(values_builder$dfWorking_builder, customization_table_path, row.names = TRUE)
            utils::write.csv(data.frame(nodes=input$id_nodes, edges=input$id_edges), ids_path)
            write(dataFramesToJSON(style_edges_reactive(), style_custom_nodes_reactive()),json_path)
            # Create a zip of the data
            zip::zipr(zipfile = file, files = c(results_table_path, customization_table_path, ids_path, json_path))
          }
        },
        contentType = "application/zip")
    }  
  })
  
  
  
  
  
  output$table1 <- renderDataTable({
    if(identical(modality(),NULL)){
      input$button_set
      values$dfWorking
    }else{
      input$button_set
      values_builder$dfWorking_builder
    }
  })
  
  output$table_constraints <- renderDataTable({
    input$setQ
    data.frame(values_select$dfSelect)
  })
  
  style_nodes_reactive <- function(data_frame,data_frame_working, id_nodes, id_edges){
    df <- data_frame
    custom_df <- data_frame_working
    if( is.null(custom_df)){
      nodes <- data.frame(id = unique(c(df[,id_nodes], df[,id_edges]))) %>%
        dplyr::mutate(node_color = "#595959") %>%
        dplyr::mutate(node_width = "10px") %>%
        dplyr::mutate(node_height = "10px") %>%
        dplyr::mutate(node_shape = "ellipse")
    }else{
      custom_background <- subset(custom_df,Parameter == "background-color", select = c("Nodes","Selection"))
      custom_shape <- subset(custom_df,Parameter == "shape", select = c("Nodes","Selection"))
      custom_size <- subset(custom_df, Parameter == "size", select = c("Nodes","Selection"))
      
      nodes <- data.frame(id = unique(c(df[,id_nodes], df[,id_edges]))) %>%
        #Define node colours
        dplyr::mutate(node_color = ifelse(id %in% c(custom_background$Nodes),
                                          c(custom_background$Selection),
                                          #subset(custom_background,Nodes==id)$Selection,
                                          "#595959")) %>%
        # Define node width
        dplyr::mutate(node_width = ifelse(id %in% c(custom_size$Nodes),
                                          c(custom_size$Selection),
                                          "10px")) %>%
        # Define node height
        dplyr::mutate(node_height = ifelse(id %in% c(custom_size$Nodes),
                                           c(custom_size$Selection),
                                           "10px")) %>%
        
        # Define node shapes
        dplyr::mutate(node_shape = ifelse(id %in% c(custom_shape$Nodes),
                                          c(custom_shape$Selection),
                                          "ellipse")) #%>%
      #dplyr::mutate(colors = "cyan magenta")
      # Need to add the nodes for the two groups, Selected and "Not Selected"
      #dplyr::bind_rows(dplyr::tibble(id = c("Selected", "Not Selected"), 
      #                     node_color="whitesmoke"))
      
    }
  }
  
  style_custom_nodes_reactive <- eventReactive(c(input$t_choice,input$m.index_t1,input$m.index_t2,
                                                 input$m.index_t3,input$m.index_t4,
                                                 input$values_t1,input$values_t2,input$values_t3,
                                                 input$values_t4,input$goResults,input$goOverlaid1,input$hideSelection, input$clearSelection, input$showAll, 
                                                 input$goInteraction,input$id_nodes,input$id_edges,input$nodes_attributes,input$button_set,
                                                 input$edges_attributes, input$deleteRows),{
                                                   
                                                   
                                                   style_nodes_reactive(new_df(),values$dfWorking, input$id_nodes, input$id_edges)
                                                   
                                                 })
  observeEvent(input$unzip,{
    filename_glob <- "*0"
    
    output_dir = tempdir() #dir where saves unzip data
    setwd(tempdir())
    
    
    # Unzip data in output_dir
    unzip <- utils::unzip(input$file$datapath, list = TRUE, overwrite = TRUE, exdir = output_dir)
    
    ls_content <- unzip$Name
    
    print(ls_content)
    
    output$resultstable_save <- renderDT({
      results <- datatable(as.data.frame(read.csv(ls_content[1])), fillContainer = TRUE, rownames = FALSE, options = list(
        pageLength = 25, autoWidth = TRUE))
      results
    })
    if(str_sub(ls_content[2],-1)=="t"){
        df <- NULL
      }else{
        df <- read.csv(ls_content[2])
      }
    output$network_saved <- renderCytoscape({
      cytoscape(nodes = style_nodes_reactive(read.csv(ls_content[1]),df, read.csv(ls_content[3])$nodes, read.csv(ls_content[3])$edges), 
                edges = style_edges_reactive_func(read.csv(ls_content[1]),read.csv(ls_content[3])$nodes, read.csv(ls_content[3])$edges)) %>% 
        node_style('background-color' = 'data(node_color)') %>%
        node_style('shape' = 'data(node_shape)') %>%
        node_style('width' = 'data(node_width)') %>%
        node_style('height' = 'data(node_height)') %>%
        cola_layout(avoidOverlap = TRUE) %>%
        panzoom()
    })
  })
  
  ###BUILDER
  output$builder_mine <- renderUI({
    tags$h4(paste0("You are querying in ", input$mine_template, "."))
  })
  
  output$builder_choice <- renderUI({
    model <- model_im()
    model_duplicated <- duplicated(model$type)
    model$logical <- model_duplicated
    options <- subset(model, logical == FALSE)$type
    
    selectInput(
      inputId = "b_choice",
      label = "Select a Data Class to Begin a Query:",
      choices = options
    )
    
  })
 
  
  get_select_view_1()
  get_select_view_2()
  get_select_view_3()
  get_select_view_4()
  get_select_view_5()
  
 
  get_select_constraint_2()
  get_select_constraint_3()
  get_select_constraint_4()
  get_select_constraint_5()
  
  get_order()
  
  get_constraint_val()
  observeEvent(input$help2,
               if (input$sideTabs == "builder") {
                 introjs(session, options = list(
                   steps = data.frame(element = c("#builder_choice", "#builder_select_view_1","#value_0",
                                                  "#constraint_button_1","#set_begin","#add_3","#setQ"),
                                      intro = c("Start by choosing a Data Class.",
                                                "Then, you must set which bioterms you want to see in the results. This also defines 
                                                the type of sorting which will be used to order the retrieved data.frame.",
                                                "Setting a constraint for the first Data Class is optional, but take into account
                                                that at some point you should define one.",
                                                "Press this button to set a constraint against a bioterm from the first Data Class. (Optional)",
                                                "Press this button to add a second level and overlay extra data.
                                                In the consecutive steps one checkbox tree is displayed for data type to be returned
                                                and another one for data types to set constraints. Move your mouse over the trees and cells to read more information.",
                                                "Once you have achieved the 3rd level, you can press this button to set
                                                two more levels of extra data into your query.",
                                                "Once you are happy with the definition of the query press this button and check that all the values to
                                                be returned are correct. You can delete values in this step."))
                 ))
               }
  )
  observeEvent(input$help3,
               if (input$sideTabs == "results") {
                 introjs(session, options = list(
                   steps = data.frame(element = c("#options_button"),
                                      intro = c("Press this button to select the Id and the Source for the Cytoscape
                   Network Visualization."))
                 ))
               }
  )
  
  observeEvent(input$help4,
               if (input$sideTabs == "visualization") {
                 introjs(session, options = list(
                   steps = data.frame(element = c(NA,"selectName",
                                                  "#hideSelection","#showAll"),
                                      intro = c("The following steps let you remove nodes from your network by attribute or ID.",
                                                "You can either select nodes by their ID, defined in the previous page, or by an attribute.",
                                                "Finally, once you are happy with the selection press this button.",
                                                "You can go back and display all the initial nodes pressing this button."))
                                      ))
               }
  )
  
  observeEvent(input$help5,
               if (input$sideTabs == "overlaid") {
                 introjs(session, options = list(
                   steps = data.frame(element = c("select_parameter",
                                                  "select_parameter_option","selectid",
                                                  "selectName_3",
                                                  "#button_set","#history"),
                                      intro = c("First select which parameter you want to customize.",
                                                "Second, define the value for the parameter.",
                                                "You can either set the node(s) to customize by their ID or by attribute. See next hint.",
                                                "You can select one of the attributes set in the Run Query Section to filter the customization. You must select the value for the attribute if you have choosen this way.",
                                                "Do not forget to press this button each time you describe a new feature for the network.",
                                                "Pressing this button you can see the changes you have done and delete some of them."))
                 ))
               }
  )
  
  options_reactive <- eventReactive(input$b_choice,{
    opt_select <- c()
    for(element in subset(model_im(), type == input$b_choice & child_type == "")$child_name){
      opt_select <- c(opt_select, element)
    }
    opt_select
  })
  observeEvent(c(input$b_select_1, input$sideTabs), {
    if (input$sideTabs == "builder") {
    if(is.null(input$b_select_1)) {
      showFeedbackDanger(
        inputId = "b_select_1",
        text = "Select at least a value."
      )
    } else {
      hideFeedback(inputId = "b_select_1")
    }
  }})
  observeEvent(c(input$selectName_2_attr, input$selectName_2), {
    req(input$selectName_2)
    if(nchar(input$selectName_2_attr)<1 & nchar(input$selectName_2)>1) {
      showFeedbackDanger(
        inputId = "selectName_2_attr",
        text = "Select a value."
      )
    } else {
      hideFeedback(inputId = "selectName_2_attr")
    }
    })
  observe({ 
    updateSelectInput(session, "b_select_1", 
                      choices = options_reactive())
  })
  
  observe({
    updateSelectInput(session, "b_constraint_1",
                      choices = options_reactive())
  })
  
  observeEvent(c(input$b_constraint_1,input$operator_1), {
    req(input$b_constraint_1)
    if(is.null(input$operator_1) & !(is.null(input$b_constraint_1))) {
      showFeedbackDanger(
        inputId = "operator_1",
        text = 'Select the operator.'
      )
    } else {
      hideFeedback(inputId = "operator_1")
    }
  })
  observeEvent(c(input$b_constraint_2,input$operator_2), {
    req(input$b_constraint_2)
    if(is.null(input$operator_2) & !(is.null(input$b_constraint_2))) {
      showFeedbackDanger(
        inputId = "operator_2",
        text = 'Select the operator.'
      )
    } else {
      hideFeedback(inputId = "operator_2")
    }
  })
  observeEvent(c(input$b_constraint_3,input$operator_3), {
    req(input$b_constraint_3)
    if(is.null(input$operator_3) & !(is.null(input$b_constraint_3))) {
      showFeedbackDanger(
        inputId = "operator_3",
        text = 'Select the operator.'
      )
    } else {
      hideFeedback(inputId = "operator_3")
    }
  })
  observeEvent(c(input$b_constraint_4,input$operator_4), {
    req(input$b_constraint_4)
    if(is.null(input$operator_4) & !(is.null(input$b_constraint_4))) {
      showFeedbackDanger(
        inputId = "operator_4",
        text = 'Select the operator.'
      )
    } else {
      hideFeedback(inputId = "operator_4")
    }
  })
  observeEvent(c(input$b_constraint_5,input$operator_5), {
    req(input$b_constraint_5)
    if(is.null(input$operator_5) & !(is.null(input$b_constraint_1))) {
      showFeedbackDanger(
        inputId = "operator_5",
        text = 'Select the operator.'
      )
    } else {
      hideFeedback(inputId = "operator_5")
    }
  })
  observeEvent(c(input$operator_0,input$value_0), {
    req(input$operator_0)
    if(nchar(input$value_0)<1) {
      showFeedbackDanger(
        inputId = "value_0",
        text = 'Select the value.'
      )
    } else {
      hideFeedback(inputId = "value_0")
    }
  })
  observeEvent(c(input$operator_1,input$value_1), {
    req(input$operator_1)
    if(nchar(input$value_1)<1) {
      showFeedbackDanger(
        inputId = "value_1",
        text = 'Select the value.'
      )
    } else {
      hideFeedback(inputId = "value_1")
    }
  })
  observeEvent(c(input$operator_2,input$value_2), {
    req(input$operator_2)
    if(nchar(input$value_2)<1) {
      showFeedbackDanger(
        inputId = "value_2",
        text = 'Select the value.'
      )
    } else {
      hideFeedback(inputId = "value_2")
    }
  })
  observeEvent(c(input$operator_3,input$value_3), {
    req(input$operator_3)
    if(nchar(input$value_3)<1) {
      showFeedbackDanger(
        inputId = "value_3",
        text = 'Select the value.'
      )
    } else {
      hideFeedback(inputId = "value_3")
    }
  })
  observeEvent(c(input$operator_4,input$value_4), {
    req(input$operator_4)
    if(nchar(input$value_4)<1) {
      showFeedbackDanger(
        inputId = "value_4",
        text = 'Select the value.'
      )
    } else {
      hideFeedback(inputId = "value_4")
    }
  })
  observeEvent(c(input$operator_5,input$value_5), {
    req(input$operator_5)
    if(nchar(input$value_5)<1) {
      showFeedbackDanger(
        inputId = "value_5",
        text = 'Select the value.'
      )
    } else {
      hideFeedback(inputId = "value_5")
    }
  })
  order_reactive <- eventReactive(c(input$b_choice, input$b_select_1),{
    options<-input$b_select_1
    options
  })
  observe({
    updateSelectInput(session, "b_order", 
                      choices = order_reactive())
  })
  
  options_2_reactive <- eventReactive(input$b_choice,{
    subset(model_im(), type == input$b_choice & !(child_type == ""))$child_type
  })
  
  nodes2 <- list()
  nodes3 <- list()
  nodes4 <- list()
  nodes_builder <- reactiveValues(nodes2_builder = nodes2,
                                  nodes3_builder = nodes3,
                                  nodes4_builder = nodes4)
  
  observeEvent(input$set_begin,{
    list2 <- list()
    options <- options_2_reactive()
    for (element in unique(options)) {
      list2_children <- list()
      for (variable in subset(model_im(), type == element & child_type == "")$child_name) {
        list2_children <- rlist::list.append(list2_children, list(label = variable,
                                                                  value = paste0(element,"_",variable)))
      }
      list2 <- rlist::list.append(list2,list(label = element, 
                                             value = element,
                                             children = list2_children))
    }
    
    nodes_builder$nodes2_builder <- list2
    get_2_choices(nodes_builder$nodes2_builder)
    
  })
  
  observeEvent(input$set_begin,{
    list2 <- list()
    options <- options_2_reactive()
    for (element in unique(options)) {
      list2_children <- list()
      for (variable in subset(model_im(), type == element & child_type == "")$child_name) {
        list2_children <- rlist::list.append(list2_children, list(label = variable,
                                                                  value = paste0(element,"_",variable)))
      }
      list2 <- rlist::list.append(list2,list(label = element, 
                                             value = element,
                                             children = list2_children))
    }
    
    nodes_builder$nodes2_builder <- list(list(label = input$b_choice, 
                                              value = paste0(input$b_choice,"_"),
                                              children = list2))
    get_2_choices(nodes_builder$nodes2_builder)
    
  })
  options_3_reactive <- eventReactive(input$b_select_2,{
    b_select_2 <- str_split(input[["b_select_2"]], "_")
    list <- list()
    element_split <- c()
    for (element in b_select_2) {
      element_split <- c(element_split,element[1])
    }
    for (element in unique(element_split)){
      elements <- unique(subset(model_im(), type == element & !(child_type == ""))$child_type)
      list <- rlist::list.append(list, elements)
    }
    names(list) <- unique(element_split)
    list
  })
  
  observeEvent(input$set_choice3,{
    list3 <- list()
    options <- options_3_reactive()
    for (i in seq_along(options)) {
      list_3_sub <- list()
      for (element in options[[i]]) {
        list3_children <- list()
        for (variable in subset(model_im(), type == element & child_type == "")$child_name) {
          list3_children <- rlist::list.append(list3_children, list(label = variable,
                                                                    value = paste0(element,"_",variable,"_",names(options)[i])))
        }
        list_3_sub <- rlist::list.append(list_3_sub,list(label = element, 
                                                         value = paste0(names(options)[i],"_",element),
                                                         children = list3_children))
      }
      
      list3 <- rlist::list.append(list3,list(label = names(options)[i], 
                                             value = names(options)[i],
                                             children = list_3_sub))
    }
    
    nodes_builder$nodes3_builder <- list3
    get_3_choices(nodes_builder$nodes3_builder)
    
  })
  
  options_4_reactive <- eventReactive(input$b_select_3,{
    b_select_3 <- str_split(input[["b_select_3"]], "_")
    list <- list()
    element_split <- c()
    for (element in b_select_3) {
      element_split <- c(element_split,paste0(element[1],"_",element[3]))
    }
    for (element in unique(element_split)){
      elements <- unique(subset(model_im(), type == str_split(element,"_")[[1]][1] & !(child_type == ""))$child_type)
      list <- rlist::list.append(list, elements)
    }
    names(list) <- unique(element_split)
    list
  })
  
  
  observeEvent(input$add_3,{
    list4 <- list()
    options <- options_4_reactive()
    for (i in seq_along(options)) {
      list_4_sub <- list()
      for (element in options[[i]]) {
        list4_children <- list()
        for (variable in subset(model_im(), type == element & child_type == "")$child_name) {
          list4_children <- rlist::list.append(list4_children, list(label = variable,
                                                                    value = paste0(element,"_",variable,"_",names(options)[i])))
        }
        list_4_sub <- rlist::list.append(list_4_sub,list(label = element, 
                                                         value = paste0(str_split(names(options)[i],"_")[[1]][1],"_",element),
                                                         children = list4_children))
      }
      list4 <- rlist::list.append(list4,list(label = str_split(names(options)[i],"_")[[1]][1], 
                                             value = paste0(input$b_choice,"_",names(options)[i]),
                                             children = list_4_sub))
    }
    
    nodes_builder$nodes4_builder <- list4
    get_4_choices(nodes_builder$nodes4_builder)
    
  })
  
  
  options_5_reactive <- eventReactive(input$b_select_4,{
    b_select_4 <- str_split(input[["b_select_4"]], "_")
    list <- list()
    element_split <- c()
    for (element in b_select_4) {
      element_split <- c(element_split,paste0(element[1],"_",element[3],"_",element[4])) 
    }
    for (element in unique(element_split)){
      elements <- unique(subset(model_im(), type == str_split(element,"_")[[1]][1] & !(child_type == ""))$child_type)
      list <- rlist::list.append(list, elements)
    }
    names(list) <- unique(element_split)
    list
  })
  
  observeEvent(input$set_choice5,{
    list5 <- list()
    options <- options_5_reactive()
    for (i in seq_along(options)) {
      list_5_sub <- list()
      for (element in options[[i]]) {
        list5_children <- list()
        for (variable in subset(model_im(), type == element & child_type == "")$child_name) {
          list5_children <- rlist::list.append(list5_children, list(label = variable,
                                                                    value = paste0(element,"_",variable,"_",names(options)[i])))
        }
        list_5_sub <- rlist::list.append(list_5_sub,list(label = element, 
                                                         value = paste0(str_split(names(options)[i],"_")[[1]][1],"_",element),
                                                         children = list5_children))
      }
      
      list5 <- rlist::list.append(list5,list(label = str_split(names(options)[i],"_")[[1]][1], 
                                             value = paste0(input$b_choice,"_",names(options)[i]),
                                             children = list_5_sub))
    }
    
    nodes_builder$nodes5_builder <- list5
    get_5_choices(nodes_builder$nodes5_builder)
  })
  
  
  
  q_query_reactive_builder_select <- eventReactive(c(input$b_choice,input$b_2_choice,input$b_3_choice,
                                                     input$b_4_choice,input$b_5_choice,
                                                     input$b_select_1,input$b_constraint_1,input$b_order,
                                                     input$desc_asc, input$b_select_2, input$b_constraint_2, 
                                                     input$b_select_3, input$b_constraint_3, input$b_select_4,
                                                     input$b_constraint_4, input$b_select_5, input$b_constraint_5,
                                                     input$operator_0, input$value_0, input$operator_1, input$value_1,
                                                     input$operator_2, input$value_2, input$operator_3, input$value_3,
                                                     input$operator_4, input$value_4, input$operator_5, input$value_5),{
                                                       model_mine <- model_im()
                                                     
                                                       selectitems <- c()
                                                       
                                                       if(!(is.null(input$b_select_1))){
                                                         try({
                                                           #print(input$b_select_1)
                                                           for (element in input$b_select_1){
                                                             element_str <- strsplit(element, " ")
                                                             #print(element_str)
                                                             element_str <- element_str[[1]]
                                                             try({
                                                               for (variable in element_str[2:length(element_str)]) {
                                                                 if(variable==toupper(variable) & variable!="SNP"){
                                                                   element_str <- replace(element_str, element_str==variable,capitalize(tolower(variable)))
                                                                 }
                                                               }
                                                             },silent = TRUE)
                                                             
                                                             if(element_str==toupper(element_str)){
                                                               element_str[1] <- tolower(element_str[1])
                                                             }else{
                                                               element_str[1] <- decapitalize(element_str[1])
                                                             }
                                                             element_str <- paste(element_str, collapse = "")
                                                             selectitems <- c(selectitems,paste0(input$b_choice,".",element_str))
                                                           }
                                                           
                                                           
                                                           
                                                         })
                                                       }
                                                       
                                                       if(!(is.null(input$b_select_2))){
                                                         try({
                                                           
                                                           for (element in input$b_select_2){
                                                             element1 <- str_split(element,"_")[[1]][2]
                                                             element_str <- strsplit(element1, " ")
                                                             element_str <- element_str[[1]]
                                                             try({
                                                               for (variable in element_str[2:length(element_str)]) {
                                                                 if(variable==toupper(variable) & variable!="SNP"){
                                                                   element_str <- replace(element_str, element_str==variable,capitalize(tolower(variable)))
                                                                 }
                                                               }
                                                             }, silent = TRUE)
                                                             
                                                             if(element_str==toupper(element_str)){
                                                               element_str[1] <- tolower(element_str[1])
                                                             }else{
                                                               element_str[1] <- decapitalize(element_str[1])
                                                             }
                                                             element_str <- paste(element_str, collapse = "")
                                                             
                                                             b_choice_2 <- strsplit(subset(model_mine, child_type == str_split(element,"_")[[1]][1] & type == input$b_choice)$child_name, " ")
                                                             for (b_choice_2_el in b_choice_2) {
                                                               b_choice_2_el <- b_choice_2_el[[1]]
                                                               try({
                                                                for (variable in b_choice_2_el[2:length(b_choice_2_el)]) {
                                                                   if(variable==toupper(variable) & variable!="SNP"){
                                                                     b_choice_el <- replace(b_choice_2_el, b_choice_2_el==variable,capitalize(tolower(variable)))
                                                                   }
                                                                 } 
                                                               }, silent = TRUE)
                                                               
                                                               if(b_choice_2_el==toupper(b_choice_2_el)){
                                                                 b_choice_2_el[1] <- tolower(b_choice_2_el[1])
                                                               }else{
                                                                 b_choice_2_el[1] <- decapitalize(b_choice_2_el[1])
                                                               }
                                                               b_choice_2_str<-paste(b_choice_2_el, collapse = "")
                                                               
                                                               selectitems <- c(selectitems,paste0(input$b_choice,".",
                                                                                                   b_choice_2_str,
                                                                                                   ".",element_str))
                                                             }
                                                             
                                                           }
                                                           
                                                         })
                                                       }
                                                       
                                                       if(!(is.null(input$b_select_3))){
                                                         try({
                                                           
                                                           for (element in input$b_select_3){
                                                             element1 <- str_split(element,"_")[[1]][2]
                                                             element_str <- strsplit(element1, " ")
                                                             element_str <- element_str[[1]]
                                                             try({
                                                               for (variable in element_str[2:length(element_str)]) {
                                                                 if(variable==toupper(variable) & variable!="SNP"){
                                                                   element_str <- replace(element_str, element_str==variable,capitalize(tolower(variable)))
                                                                 }
                                                               }
                                                             }, silent = TRUE)
                                                             
                                                             if(element_str==toupper(element_str)){
                                                               element_str[1] <- tolower(element_str[1])
                                                             }else{
                                                               element_str[1] <- decapitalize(element_str[1])
                                                             }
                                                             element_str <- paste(element_str, collapse = "")
                                                             
                                                             b_choice_2 <- strsplit(subset(model_mine, child_type == str_split(element,"_")[[1]][3] & type == input$b_choice)$child_name, " ")
                                                             for (b_choice_2_el in b_choice_2) {
                                                               b_choice_2_el <- b_choice_2_el[[1]]
                                                               try({
                                                                 for (variable in b_choice_2_el[2:length(b_choice_2_el)]) {
                                                                   if(variable==toupper(variable) & variable!="SNP"){
                                                                     b_choice_2_el <- replace(b_choice_2_el, b_choice_2_el==variable,capitalize(tolower(variable)))
                                                                   }
                                                                 }
                                                               }, silent = TRUE)
                                                               
                                                               if(b_choice_2_el==toupper(b_choice_2_el)){
                                                                 b_choice_2_el[1] <- tolower(b_choice_2_el[1])
                                                               }else{
                                                                 b_choice_2_el[1] <- decapitalize(b_choice_2_el[1])
                                                               }
                                                               b_choice_2_str<-paste(b_choice_2_el, collapse = "")
                                                               
                                                               b_choice_3 <- strsplit(subset(model_mine, child_type == str_split(element,"_")[[1]][1] & type == str_split(element,"_")[[1]][3])$child_name, " ")
                                                               for (b_choice_3_el in b_choice_3) {
                                                                 b_choice_3_el <- b_choice_3_el[[1]]
                                                                 try({
                                                                   for (variable in b_choice_3_el[2:length(b_choice_3_el)]) {
                                                                     if(variable==toupper(variable) & variable!="SNP"){
                                                                       b_choice_3_el <- replace(b_choice_3_el, b_choice_3_el==variable,capitalize(tolower(variable)))
                                                                     }
                                                                   }
                                                                 }, silent = TRUE)
                                                                 
                                                                 if(b_choice_3_el==toupper(b_choice_3_el)){
                                                                   b_choice_3_el[1] <- tolower(b_choice_3_el[1])
                                                                 }else{
                                                                   b_choice_3_el[1] <- decapitalize(b_choice_3_el[1])
                                                                 }
                                                                 b_choice_3_str<-paste(b_choice_3_el, collapse = "")
                                                                 
                                                                 selectitems <- c(selectitems,paste0(input$b_choice,".",
                                                                                                     b_choice_2_str,
                                                                                                     ".",b_choice_3_str,
                                                                                                     ".",element_str))
                                                               }
                                                             }
                                                             
                                                             
                                                           }
                                                           
                                                         })
                                                       }
                                                       
                                                       if(!(is.null(input$b_select_4))){
                                                         try({
                                                           for (element in input$b_select_4){
                                                             element1 <- str_split(element,"_")[[1]][2]
                                                             element_str <- strsplit(element1, " ")
                                                             element_str <- element_str[[1]]
                                                             try({
                                                               for (variable in element_str[2:length(element_str)]) {
                                                                 if(variable==toupper(variable) & variable!="SNP"){
                                                                   element_str <- replace(element_str, element_str==variable,capitalize(tolower(variable)))
                                                                 }
                                                               }
                                                             }, silent = TRUE)
                                                             
                                                             if(element_str==toupper(element_str)){
                                                               element_str[1] <- tolower(element_str[1])
                                                             }else{
                                                               element_str[1] <- decapitalize(element_str[1])
                                                             }
                                                             element_str <- paste(element_str, collapse = "")
                                                             
                                                             b_choice_2 <- strsplit(subset(model_mine, child_type == str_split(element,"_")[[1]][4] & type == input$b_choice)$child_name, " ")
                                                             for(b_choice_2_el in b_choice_2){
                                                               b_choice_2_el <- b_choice_2_el[[1]]
                                                               try({
                                                                 for (variable in b_choice_2_el[2:length(b_choice_2_el)]) {
                                                                   if(variable==toupper(variable) & variable!="SNP"){
                                                                     b_choice_2_el <- replace(b_choice_2_el, b_choice_2_el==variable,capitalize(tolower(variable)))
                                                                   }
                                                                 }
                                                               }, silent = TRUE)
                                                               
                                                               if(b_choice_2_el==toupper(b_choice_2_el)){
                                                                 b_choice_2_el[1] <- tolower(b_choice_2_el[1])
                                                               }else{
                                                                 b_choice_2_el[1] <- decapitalize(b_choice_2_el[1])
                                                               }
                                                               b_choice_2_str<-paste(b_choice_2_el, collapse = "")
                                                               
                                                               b_choice_3 <- strsplit(subset(model_mine, child_type == str_split(element,"_")[[1]][3] & type == str_split(element,"_")[[1]][4])$child_name, " ")
                                                               for(b_choice_3_el in b_choice_3){
                                                                 b_choice_3_el <- b_choice_3_el[[1]]
                                                                 try({
                                                                  for (variable in b_choice_3_el[2:length(b_choice_3_el)]) {
                                                                     if(variable==toupper(variable) & variable!="SNP"){
                                                                       b_choice_3_el <- replace(b_choice_3_el, b_choice_3_el==variable,capitalize(tolower(variable)))
                                                                     }
                                                                   } 
                                                                 }, silent = TRUE)
                                                                 
                                                                 if(b_choice_3_el==toupper(b_choice_3_el)){
                                                                   b_choice_3_el[1] <- tolower(b_choice_3_el[1])
                                                                 }else{
                                                                   b_choice_3_el[1] <- decapitalize(b_choice_3_el[1])
                                                                 }
                                                                 b_choice_3_str<-paste(b_choice_3_el, collapse = "")
                                                                 
                                                                 b_choice_4 <- strsplit(subset(model_mine, child_type == str_split(element,"_")[[1]][1] & type == str_split(element,"_")[[1]][3])$child_name, " ")
                                                                 for(b_choice_4_el in b_choice_4){
                                                                   b_choice_4_el <- b_choice_4_el[[1]]
                                                                   try({
                                                                     for (variable in b_choice_4_el[2:length(b_choice_4_el)]) {
                                                                       if(variable==toupper(variable) & variable!="SNP"){
                                                                         b_choice_el <- replace(b_choice_4_el, b_choice_4_el==variable,capitalize(tolower(variable)))
                                                                       }
                                                                     }
                                                                   }, silent = TRUE)
                                                                   
                                                                   if(b_choice_4_el==toupper(b_choice_4_el)){
                                                                     b_choice_4_el[1] <- tolower(b_choice_4_el[1])
                                                                   }else{
                                                                     b_choice_4_el[1] <- decapitalize(b_choice_4_el[1])
                                                                   }
                                                                   b_choice_4_str<-paste(b_choice_4_el, collapse = "")
                                                                   
                                                                   
                                                                   selectitems <- c(selectitems,paste0(input$b_choice,".",
                                                                                                       b_choice_2_str,
                                                                                                       ".",b_choice_3_str,
                                                                                                       ".",b_choice_4_str,
                                                                                                       ".",element_str))
                                                                 }
                                                                 
                                                               }
                                                               
                                                             }
                                                             
                                                           }
                                                           
                                                         })
                                                       }
                                                       
                                                       if(!(is.null(input$b_select_5))){
                                                         try({
                                                           print(input$b_select_5)
                                                           for (element in input$b_select_5){
                                                             element1 <- str_split(element,"_")[[1]][2]
                                                             
                                                             element_str <- strsplit(element1, " ")
                                                             element_str <- element_str[[1]]
                                                             try({
                                                               for (variable in element_str[2:length(element_str)]) {
                                                                 if(variable==toupper(variable) & variable!="SNP"){
                                                                   v<-tolower(variable)
                                                                   v<-capitalize(v)
                                                                   element_str <- replace(element_str, element_str == variable, v)
                                                                 }
                                                               }
                                                             }, silent = TRUE)
                                                             
                                                             if(element_str==toupper(element_str)){
                                                               element_str[1] <- tolower(element_str[1])
                                                             }else{
                                                               element_str[1] <- decapitalize(element_str[1])
                                                             }
                                                             element_str <- paste(element_str, collapse = "")
                                                             
                                                             b_choice_2 <- strsplit(subset(model_mine, child_type == str_split(element,"_")[[1]][5] & type == input$b_choice)$child_name, " ")
                                                             for(b_choice_2_el in b_choice_2){
                                                               b_choice_2_el <- b_choice_2_el[[1]]
                                                               try({
                                                                 for (variable in b_choice_2_el[2:length(b_choice_2_el)]) {
                                                                   if(variable==toupper(variable) & variable!="SNP"){
                                                                     b_choice_2_el <- replace(b_choice_2_el, b_choice_2_el==variable,capitalize(tolower(variable)))
                                                                   }
                                                                 }
                                                               }, silent = TRUE)
                                                               
                                                               if(b_choice_2==toupper(b_choice_2)){
                                                                 b_choice_2_el[1] <- tolower(b_choice_2_el[1])
                                                               }else{
                                                                 b_choice_2_el[1] <- decapitalize(b_choice_2_el[1])
                                                               }
                                                               b_choice_2_str<-paste(b_choice_2_el, collapse = "")
                                                               
                                                               b_choice_3 <- strsplit(subset(model_mine, child_type == str_split(element,"_")[[1]][4] & type == str_split(element,"_")[[1]][5])$child_name, " ")
                                                               for(b_choice_3_el in b_choice_3){
                                                                 b_choice_3_el <- b_choice_3_el[[1]]
                                                                 try({
                                                                   for (variable in b_choice_3_el[2:length(b_choice_3_el)]) {
                                                                     if(variable==toupper(variable) & variable!="SNP"){
                                                                       b_choice_3_el <- replace(b_choice_3_el, b_choice_3_el==variable,capitalize(tolower(variable)))
                                                                     }
                                                                   }
                                                                 }, silent = TRUE)
                                                                 
                                                                 if(b_choice_3_el==toupper(b_choice_3_el)){
                                                                   b_choice_3_el[1] <- tolower(b_choice_3_el[1])
                                                                 }else{
                                                                   b_choice_3_el[1] <- decapitalize(b_choice_3_el[1])
                                                                 }
                                                                 b_choice_3_str<-paste(b_choice_3_el, collapse = "")
                                                                 
                                                                 b_choice_4 <- strsplit(subset(model_mine, child_type == str_split(element,"_")[[1]][3] & type == str_split(element,"_")[[1]][4])$child_name, " ")
                                                                 for(b_choice_4_el in b_choice_4){
                                                                   b_choice_4_el <- b_choice_4_el[[1]]
                                                                   try({
                                                                     for (variable in b_choice_4_el[2:length(b_choice_4_el)]) {
                                                                       if(variable==toupper(variable) & variable!="SNP"){
                                                                         b_choice_4_el <- replace(b_choice_4_el, b_choice_4_el==variable,capitalize(tolower(variable)))
                                                                       }
                                                                     }
                                                                   }, silent = TRUE)
                                                                   
                                                                   if(b_choice_4_el==toupper(b_choice_4_el)){
                                                                     b_choice_4_el[1] <- tolower(b_choice_4_el[1])
                                                                   }else{
                                                                     b_choice_4_el[1] <- decapitalize(b_choice_4_el[1])
                                                                   }
                                                                   b_choice_4_str<-paste(b_choice_4_el, collapse = "")
                                                                   
                                                                   b_choice_5 <- strsplit(subset(model_mine, child_type == str_split(element,"_")[[1]][1] & type == str_split(element,"_")[[1]][3])$child_name, " ")
                                                                   b_choice_5 <- b_choice_5[[1]]
                                                                   try({
                                                                     for (variable in b_choice_5[2:length(b_choice_5)]) {
                                                                       if(variable==toupper(variable) & variable!="SNP"){
                                                                         b_choice_5 <- replace(b_choice_5, b_choice_5==variable,capitalize(tolower(variable)))
                                                                       }
                                                                     }
                                                                   }, silent = TRUE)
                                                                   
                                                                   for(b_choice_5_el in b_choice_5){
                                                                     if(b_choice_5_el==toupper(b_choice_5_el)){
                                                                       b_choice_5_el[1] <- tolower(b_choice_5_el[1])
                                                                     }else{
                                                                       b_choice_5_el[1] <- decapitalize(b_choice_5_el[1])
                                                                     }
                                                                     b_choice_5_str<-paste(b_choice_5_el, collapse = "")
                                                                     
                                                                     selectitems <- c(selectitems,paste0(input$b_choice,".",
                                                                                                         b_choice_2_str,
                                                                                                         ".",b_choice_3_str,
                                                                                                         ".",b_choice_4_str,
                                                                                                         ".",b_choice_5_str,
                                                                                                         ".",element_str))
                                                                   }
                                                                   
                                                                 }
                                                                 
                                                               }
                                                               
                                                             }
                                                             
                                                           }
                                                           
                                                           
                                                         })
                                                       }
                                                       
                                                       
                                                       selectitems
                                                     }
  )
  
  q_query_reactive_builder_order <- eventReactive(c(input$b_choice,input$b_order,input$desc_asc),{
                                                    
                                                      b_order_str <- strsplit(input$b_order, " ")
                                                      b_order_str[[1]][1] <- decapitalize(b_order_str[[1]][1])
                                                      b_order_str <- paste(b_order_str[[1]], collapse = "")
                                                      
                                                      order <- paste0(input$b_choice,".",b_order_str)
                                                      sort <- c(as.character(input$desc_asc))
                                                      names(sort) <- order
                                                      return(list(sort))
                                                     
                                                    }
  )
  
  q_query_reactive_builder_constraints <- eventReactive(c(input$b_choice,input$b_2_choice,input$b_3_choice,
                                                          input$b_4_choice,input$b_5_choice,
                                                          input$b_select_1,input$b_constraint_1,input$b_order,
                                                          input$desc_asc, input$b_select_2, input$b_constraint_2, 
                                                          input$b_select_3, input$b_constraint_3, input$b_select_4,
                                                          input$b_constraint_4, input$b_select_5, input$b_constraint_5,
                                                          input$operator_0, input$value_0, input$operator_1, input$value_1,
                                                          input$operator_2, input$value_2, input$operator_3, input$value_3,
                                                          input$operator_4, input$value_4, input$operator_5, input$value_5),{
                                                            model_mine <- model_im()
                                                            constraints_paths <- c()
                                                            constraints_operators <- c()
                                                            constraints_values <- c()
                                                            if(!(is.null(input$operator_0)) & !(is.null(input$value_0))){
                                                              try({
                                                                constraints_paths <- c(constraints_paths,input$b_choice)
                                                                constraints_operators <- c(constraints_operators,input$operator_0)
                                                                constraints_values <- rlist::list.append(constraints_values, str_split(input$value_0,","))
                                                              })
                                                            }
                                                            if(!(is.null(input$operator_1)) & !(is.null(input$value_1))){
                                                              try({
                                                                b_select_1 <- strsplit(input$b_constraint_1, " ")
                                                                if(b_select_1==toupper(b_select_1)){
                                                                  b_select_1[[1]][1] <- tolower(b_select_1[[1]][1])
                                                                }else{
                                                                  b_select_1[[1]][1] <- decapitalize(b_select_1[[1]][1])
                                                                }
                                                                b_select_1_str<-paste(b_select_1[[1]], collapse = "")
                                                                
                                                                constraints_paths <- c(constraints_paths,paste0(input$b_choice,".",b_select_1_str))
                                                                constraints_operators <- c(constraints_operators,input$operator_1)
                                                                constraints_values <- rlist::list.append(constraints_values, str_split(input$value_1,","))
                                                              })
                                                            }
                                                            if(!(is.null(input$operator_2)) & !(is.null(input$value_2))){
                                                              try({
                                                                for (i in seq_along(input$b_constraint_2)){
                                                                  element <- input$b_constraint_2[i]
                                                                  element1 <- str_split(element,"_")[[1]][2]
                                                                  element_str <- strsplit(element1, " ")
                                                                  if(element_str==toupper(element_str)){
                                                                    element_str[[1]][1] <- tolower(element_str[[1]][1])
                                                                  }else{
                                                                    element_str[[1]][1] <- decapitalize(element_str[[1]][1])
                                                                  }
                                                                  b_select_2_str <- paste(element_str[[1]], collapse = "")
                                                                  b_choice_2 <- strsplit(subset(model_mine, child_type == str_split(element,"_")[[1]][1] & type == input$b_choice)$child_name, " ")
                                                                  for (b_choice_2_el in b_choice_2) {
                                                                    if(b_choice_2_el==toupper(b_choice_2_el)){
                                                                      b_choice_2_el[[1]][1] <- tolower(b_choice_2_el[[1]][1])
                                                                    }else{
                                                                      b_choice_2_el[[1]][1] <- decapitalize(b_choice_2_el[[1]][1])
                                                                    }
                                                                    b_choice_2_str<-paste(b_choice_2_el[[1]], collapse = "")
                                                                    op <- input$operator_2[i]
                                                                    constraints_operators <- c(constraints_operators,op)
                                                                    values <- str_split(input$value_2,";")[[1]][i]
                                                                    constraints_values <- rlist::list.append(constraints_values, str_split(values,","))
                                                                    constraints_paths <- c(constraints_paths,paste0(input$b_choice,".",
                                                                                                                    b_choice_2_str ,".",
                                                                                                                    b_select_2_str))
                                                                    
                                                                  }
                                                                  
                                                                }
                                                                
                                                                
                                                              })
                                                            }
                                                            if(!(is.null(input$operator_3)) & !(is.null(input$value_3))){
                                                              try({
                                                                for (i in seq_along(input$b_constraint_3)){
                                                                  element <- input$b_constraint_3[i]
                                                                  element1 <- str_split(element,"_")[[1]][2]
                                                                  element_str <- strsplit(element1, " ")
                                                                  if(element_str==toupper(element_str)){
                                                                    element_str[[1]][1] <- tolower(element_str[[1]][1])
                                                                  }else{
                                                                    element_str[[1]][1] <- decapitalize(element_str[[1]][1])
                                                                  }
                                                                  
                                                                  b_select_3_str<-paste(element_str[[1]], collapse = "")
                                                                  b_choice_2 <- strsplit(subset(model_mine, child_type == str_split(element,"_")[[1]][3] & type == input$b_choice)$child_name, " ")
                                                                  for (b_choice_2_el in b_choice_2) {
                                                                    if(b_choice_2_el==toupper(b_choice_2_el)){
                                                                      b_choice_2_el[[1]][1] <- tolower(b_choice_2_el[[1]][1])
                                                                    }else{
                                                                      b_choice_2_el[[1]][1] <- decapitalize(b_choice_2_el[[1]][1])
                                                                    }
                                                                    b_choice_2_str<-paste(b_choice_2_el[[1]], collapse = "")
                                                                    
                                                                    b_choice_3 <- strsplit(subset(model_mine, child_type == str_split(element,"_")[[1]][1] & type == str_split(element,"_")[[1]][3])$child_name, " ")
                                                                    for (b_choice_3_el in b_choice_3) {
                                                                      if(b_choice_3_el==toupper(b_choice_3_el)){
                                                                        b_choice_3_el[[1]][1] <- tolower(b_choice_3_el[[1]][1])
                                                                      }else{
                                                                        b_choice_3_el[[1]][1] <- decapitalize(b_choice_3_el[[1]][1])
                                                                      }
                                                                      b_choice_3_str<-paste(b_choice_3_el[[1]], collapse = "")
                                                                      
                                                                      constraints_paths <- c(constraints_paths,paste0(input$b_choice,".",
                                                                                                                      b_choice_2_str,
                                                                                                                      ".",b_choice_3_str,
                                                                                                                      ".",b_select_3_str))
                                                                      op <- input$operator_3[i]
                                                                      constraints_operators <- c(constraints_operators,op)
                                                                      values <- str_split(input$value_3,";")[[1]][i]
                                                                      constraints_values <- rlist::list.append(constraints_values, str_split(values,","))
                                                                    }
                                                                  }
                                                                  
                                                                }
                                                              })
                                                            }
                                                            if(!(is.null(input$operator_4)) & !(is.null(input$value_4))){
                                                              try({
                                                                for (i in seq_along(input$b_constraint_4)){
                                                                  element <- input$b_constraint_4[i]
                                                                  element1 <- str_split(element,"_")[[1]][2]
                                                                  element_str <- strsplit(element1, " ")
                                                                  if(element_str==toupper(element_str)){
                                                                    element_str[[1]][1] <- tolower(element_str[[1]][1])
                                                                  }else{
                                                                    element_str[[1]][1] <- decapitalize(element_str[[1]][1])
                                                                  }
                                                                  b_select_4_str<-paste(element_str[[1]], collapse = "")
                                                                  
                                                                  b_choice_2 <- strsplit(subset(model_mine, child_type == str_split(element,"_")[[1]][4] & type == input$b_choice)$child_name, " ")
                                                                  for(b_choice_2_el in b_choice_2){
                                                                    if(b_choice_2_el==toupper(b_choice_2_el)){
                                                                      b_choice_2_el[[1]][1] <- tolower(b_choice_2_el[[1]][1])
                                                                    }else{
                                                                      b_choice_2_el[[1]][1] <- decapitalize(b_choice_2_el[[1]][1])
                                                                    }
                                                                    b_choice_2_str<-paste(b_choice_2_el[[1]], collapse = "")
                                                                    
                                                                    b_choice_3 <- strsplit(subset(model_mine, child_type == str_split(element,"_")[[1]][3] & type == str_split(element,"_")[[1]][4])$child_name, " ")
                                                                    for(b_choice_3_el in b_choice_3){
                                                                      if(b_choice_3_el==toupper(b_choice_3_el)){
                                                                        b_choice_3_el[[1]][1] <- tolower(b_choice_3_el[[1]][1])
                                                                      }else{
                                                                        b_choice_3_el[[1]][1] <- decapitalize(b_choice_3_el[[1]][1])
                                                                      }
                                                                      b_choice_3_str<-paste(b_choice_3_el[[1]], collapse = "")
                                                                      
                                                                      b_choice_4 <- strsplit(subset(model_mine, child_type == str_split(element,"_")[[1]][1] & type == str_split(element,"_")[[1]][3])$child_name, " ")
                                                                      for(b_choice_4_el in b_choice_4){
                                                                        if(b_choice_4_el==toupper(b_choice_4_el)){
                                                                          b_choice_4_el[[1]][1] <- tolower(b_choice_4_el[[1]][1])
                                                                        }else{
                                                                          b_choice_4_el[[1]][1] <- decapitalize(b_choice_4_el[[1]][1])
                                                                        }
                                                                        b_choice_4_str<-paste(b_choice_4_el[[1]], collapse = "")
                                                                        constraints_paths <- c(constraints_paths,paste0(input$b_choice,".",
                                                                                                                        b_choice_2_str,
                                                                                                                        ".",b_choice_3_str,
                                                                                                                        ".",b_choice_4_str,
                                                                                                                        ".",b_select_4_str))
                                                                        op <- input$operator_4[i]
                                                                        constraints_operators <- c(constraints_operators,op)
                                                                        values <- str_split(input$value_4,";")[[1]][i]
                                                                        constraints_values <- rlist::list.append(constraints_values, str_split(values,","))
                                                                        
                                                                      }
                                                                    }
                                                                  }
                                                                  
                                                                }
                                                                
                                                                
                                                              })
                                                            }
                                                            if(!(is.null(input$operator_5)) & !(is.null(input$value_5))){
                                                              try({
                                                                
                                                                for (i in seq_along(input$b_constraint_5)){
                                                                  element <- input$b_constraint_5[i]
                                                                  element1 <- str_split(element,"_")[[1]][2]
                                                                  element_str <- strsplit(element1, " ")
                                                                  if(element_str==toupper(element_str)){
                                                                    element_str[[1]][1] <- tolower(element_str[[1]][1])
                                                                  }else{
                                                                    element_str[[1]][1] <- decapitalize(element_str[[1]][1])
                                                                  }
                                                                  b_select_5_str<-paste(element_str[[1]], collapse = "")
                                                                  
                                                                  b_choice_2 <- strsplit(subset(model_mine, child_type == str_split(element,"_")[[1]][5] & type == input$b_choice)$child_name, " ")
                                                                  for(b_choice_2_el in b_choice_2){
                                                                    if(b_choice_2==toupper(b_choice_2)){
                                                                      b_choice_2_el[[1]][1] <- tolower(b_choice_2_el[[1]][1])
                                                                    }else{
                                                                      b_choice_2_el[[1]][1] <- decapitalize(b_choice_2_el[[1]][1])
                                                                    }
                                                                    b_choice_2_str<-paste(b_choice_2_el[[1]], collapse = "")
                                                                    
                                                                    b_choice_3 <- strsplit(subset(model_mine, child_type == str_split(element,"_")[[1]][4] & type == str_split(element,"_")[[1]][5])$child_name, " ")
                                                                    for(b_choice_3_el in b_choice_3){
                                                                      if(b_choice_3_el==toupper(b_choice_3_el)){
                                                                        b_choice_3_el[[1]][1] <- tolower(b_choice_3_el[[1]][1])
                                                                      }else{
                                                                        b_choice_3_el[[1]][1] <- decapitalize(b_choice_3_el[[1]][1])
                                                                      }
                                                                      b_choice_3_str<-paste(b_choice_3_el[[1]], collapse = "")
                                                                      
                                                                      b_choice_4 <- strsplit(subset(model_mine, child_type == str_split(element,"_")[[1]][3] & type == str_split(element,"_")[[1]][4])$child_name, " ")
                                                                      for(b_choice_4_el in b_choice_4){
                                                                        if(b_choice_4_el==toupper(b_choice_4_el)){
                                                                          b_choice_4_el[[1]][1] <- tolower(b_choice_4_el[[1]][1])
                                                                        }else{
                                                                          b_choice_4_el[[1]][1] <- decapitalize(b_choice_4_el[[1]][1])
                                                                        }
                                                                        b_choice_4_str<-paste(b_choice_4_el[[1]], collapse = "")
                                                                        
                                                                        b_choice_5 <- strsplit(subset(model_mine, child_type == str_split(element,"_")[[1]][1] & type == str_split(element,"_")[[1]][3])$child_name, " ")
                                                                        for(b_choice_5_el in b_choice_5){
                                                                          if(b_choice_5_el==toupper(b_choice_5_el)){
                                                                            b_choice_5_el[[1]][1] <- tolower(b_choice_5_el[[1]][1])
                                                                          }else{
                                                                            b_choice_5_el[[1]][1] <- decapitalize(b_choice_5_el[[1]][1])
                                                                          }
                                                                          b_choice_5_str<-paste(b_choice_5_el[[1]], collapse = "")
                                                                          constraints_paths <- c(constraints_paths,paste0(input$b_choice,".",
                                                                                                                          b_choice_2_str,
                                                                                                                          ".",b_choice_3_str,
                                                                                                                          ".",b_choice_4_str,
                                                                                                                          ".",b_choice_5_str,
                                                                                                                          ".",b_select_5_str))
                                                                          op <- input$operator_5[i]
                                                                          constraints_operators <- c(constraints_operators,op)
                                                                          values <- str_split(input$value_5,";")[[1]][i]
                                                                          constraints_values <- rlist::list.append(constraints_values, str_split(values,","))
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                                
                                                              })
                                                            }
                                                            
                                                            constraints_values_list <- list()
                                                            index<-1
                                                            for(i in constraints_values){
                                                              constraints_values_list[index]<-i
                                                              index<-index+1
                                                            }
                                                            
                                                            constraint_function <- setConstraints(
                                                              paths = constraints_paths,
                                                              operators = constraints_operators,
                                                              values = constraints_values_list
                                                            )
                                                            
                                                            constraint_function
                                                          }
  )
  
  q_query_reactive_builder <- eventReactive(c(input$setQ, input$delete_constraints),{
    values_select$dfSelect <- q_query_reactive_builder_select()
    if (!is.null(input$table_constraints_rows_selected)) {
      values_select$dfSelect <- data.frame(values_select$dfSelect)
      values_select$dfSelect <- values_select$dfSelect[-as.numeric(input$table_constraints_rows_selected),]
    }
    
    if(is.data.frame(values_select$dfSelect)){
      values_select <- values_select$dfSelect[,1]
    }else{
      values_select <- values_select$dfSelect
    }
    q_query_reactive_builder_func <- setQuery(
      select = values_select,
      orderBy = q_query_reactive_builder_order(),
      where = q_query_reactive_builder_constraints()
    )
    q_query_reactive_builder_func
  })
  
  observeEvent(c(input$setQ,input$delete_constraints),{
    output$builder_constraint_summary <- renderUI({
      tagList(
        box(
          width = 12, # fill the width of the page
          tags$h2("Summary"),
          if(is.list(q_query_reactive_builder())){tags$p("No constraint set to template query.")}else{
            if(length(summary(q_query_reactive_builder())) == 1){try(tags$p(summary(q_query_reactive_builder())),silent=TRUE
                )}else{tags$p("We can't show you the Summary.")}
            },
          div(style="display:inline-block; float:right",
              actionButton("goBuilder","Go to Results",
                           style="color: #fff; background-color: #3366ff; border-color: #3366ff"))
        )
      )
    })
    
    
    
  })
  
  results_reactive_builder <- eventReactive(c(input$goBuilder, input$delete_constraints),{
    
    q <- q_query_reactive_builder()
    if(is.list(q)){
      res <- runQuery(im(), q[[1]])
    } else {
      res <- runQuery(im(), q)
    }
  })
  
  
  interaction_reactive_builder <- eventReactive(c(input$goInteraction,input$id_nodes,input$id_edges,input$nodes_attributes,input$edges_attributes),{
    interaction_reactive_func(results_reactive_builder())
  })
  
  hidennodes_builder <- reactiveVal()
  
  
  new_df_builder <- eventReactive(c(input$hideSelection, input$clearSelection, input$showAll, input$goOverlaid1),{
    df <- results_reactive_builder()
    for (element in hidennodes_builder()) {
      df <- df[!df[,input$id_nodes]==element,]
    }
    View(df)
    df
    
  })
  
  style_edges_reactive_builder <- eventReactive(c(input$b_choice,input$b_2_choice,input$b_3_choice,
                                                  input$b_4_choice,input$b_5_choice,
                                                  input$b_select_1,input$b_constraint_1,input$b_order,
                                                  input$desc_asc, input$b_select_2, input$b_constraint_2, 
                                                  input$b_select_3, input$b_constraint_3, input$b_select_4,
                                                  input$b_constraint_4, input$b_select_5, input$b_constraint_5,
                                                  input$operator_0, input$value_0, input$operator_1, input$value_1,
                                                  input$operator_2, input$value_2, input$operator_3, input$value_3,
                                                  input$operator_4, input$value_4, input$operator_5, input$value_5,
                                                  input$goOverlaid1,input$hideSelection, input$clearSelection, input$showAll,input$goBuilder,
                                                  input$goInteraction,input$id_nodes,input$id_edges,input$nodes_attributes,input$edges_attributes),{
                                                    
                                                    style_edges_reactive_func(new_df_builder(),input$id_nodes,input$id_edges)
                                                    
                                                  })
  
  
  rv_builder <- NULL
  rv_select <- NULL
  values_builder <- reactiveValues(dfWorking_builder = rv_builder)
  values_select <- reactiveValues(dfSelect = rv_select)
  
  style_custom_nodes_reactive_builder <- eventReactive(c(input$b_choice,input$b_2_choice,input$b_3_choice,
                                                         input$b_4_choice,input$b_5_choice,
                                                         input$b_select_1,input$b_constraint_1,input$b_order,
                                                         input$desc_asc, input$b_select_2, input$b_constraint_2, 
                                                         input$b_select_3, input$b_constraint_3, input$b_select_4,
                                                         input$b_constraint_4, input$b_select_5, input$b_constraint_5,
                                                         input$operator_0, input$value_0, input$operator_1, input$value_1,
                                                         input$operator_2, input$value_2, input$operator_3, input$value_3,
                                                         input$operator_4, input$value_4, input$operator_5, input$value_5,input$deleteRows, input$button_set,
                                                         input$goOverlaid1,input$hideSelection, input$clearSelection, input$showAll,input$goBuilder,
                                                         input$goInteraction,input$id_nodes,input$id_edges,input$nodes_attributes,input$edges_attributes),{
                                                           
                                                           style_nodes_reactive(as.data.frame(results_reactive_builder()), values_builder$dfWorking_builder, input$id_nodes, input$id_edges)
                                                           
                                                         })
  
  
  observeEvent(input$colorSelection,  ignoreInit=TRUE,{
    output$network <- renderCytoscape({
      strategy <- input$layoutcytoscape
      printf("about to sendCustomMessage, layout: %s", strategy)
      if(strategy == "cola"){
        cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
          node_style('background-color' = 'data(node_color)') %>%
          cola_layout(avoidOverlap = TRUE) %>%
          panzoom()
      }else{
        cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
          node_style('background-color' = 'data(node_color)') %>%
          layout(strategy, avoidOverlap = TRUE) %>%
          panzoom()
      }
    })
  })
  icon_g <- icon("github")
  icon_g[["attribs"]][["class"]] <- "fa fa-github"
  output$dropdownmenu <- renderMenu({
    dropdownMenu(type = "messages",
                 # from for first line, message 2nd line smaller font
                 messageItem(
                   from = "Project in Github",
                   message = "Documentation, Source, Citation",
                   # github icon have compatibility problem https://github.com/rstudio/shiny/issues/2260, shiny dashboard fixed 5.0 problem with sidebar switch but not this one. shiny support font 5.0 but this is dashboard problem. We can just change the icon to v4 by changing class from fab to fa.
                   icon = icon_g,
                   href = "https://github.com/celions/InterMineR-Cytoscape"),
                 # we always print in console so no need for this.
                 # messageItem(
                 #   from = "Package Build Date",
                 #   message = PKG_BUILD_INFO$build_date,
                 #   icon = icon("calendar-o")),
                 messageItem(
                   from = "Issues",
                   message = "Report Issues",
                   icon = icon("exclamation-circle"),
                   href = "https://github.com/celions/InterMineR-Cytoscape/issues"),
                 badgeStatus = NULL,
                 icon = icon("info-circle fa-lg"),
                 headerText = "App Information"
    )
  })
}

# Call ui and server to create a new session in browser which display the app----
shinyApp(ui, server, options = list(launch.browser = TRUE))
