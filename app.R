## app.R ##

#Packages----
# load("Packages.R")

#Libraries----
library(shiny)
library(shinyWidgets)
library(plotly)
library(zip)
library(filesstrings)
library(shinydashboard)
library(shinycustomloader)
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
library(base64enc)
library(dplyr)
#library(rlist)
#library(InterMineR) #this line is giving an error, meanwhile I solve the problem you can load the functions saved in workspace_app.RData opening this file. 

#Load functions----
#load("workspace_app.RData")
#source("graphToJSON_function.R")
#source("InterMineR_functions.R")

# UI: View----
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
      menuItem("4. Overlay additional data", tabName = "overlaid", icon = icon("palette")),
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
        
        introjsUI(), #include rintrojs
        
        useShinyFeedback(), # include shinyFeedback
        
        tags$h3("Template queries"),
        tabPanel("Template queries", fluid = TRUE, 
                 
                 sidebarLayout(
                   sidebarPanel( #display with distinc background color and containing input controls
                     fluidRow(
                       column( # create a column
                         width = 12, # fill width
                         
                         uiOutput("template_mine"), #output text for the user saying which Mine is chosen
                         hr(),
                         uiOutput("template_choice"), #the select list to choose a template query, the values are defined once the Mine is selected
                         br(),
                         br(),
                         br(),
                         #include actionable button to display help information
                         actionButton("help1","", icon = icon("question"), style="color: #000; background-color: #bdff80; border-color: #bdff80")
                         
                       ))),
                   mainPanel( #ocupies approx 2/3 of the width, usually contains outputs  
                     fluidRow(
                       uiOutput("index_choice"),
                       hr(),
                       br(),
                       uiOutput("template_constraint_summary"), #control inputs to redefine constraints from the template query
                       bsTooltip("template_constraint_summary","Summarize the information about the constraints contained by an object of the class InterMineR.","top")
                     )
                   )
                 )
                 
                 
        )
        
      ),
      
      tabItem(
        tabName = "builder", # Add items inside tab plots
        
        introjsUI(), #include rintrojs
        
        useShinyFeedback(), # include shinyFeedback
        
        tags$h3("Query Builder"), #include titles
        br(),
        useShinyalert(), #initializes shinyalert 
        
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
              #SET button will actionate secondary layers of the query
              div(style="display:inline-block",actionButton("set_begin","SET"),actionButton("help2","", icon = icon("question"), style="color: #000; background-color: #bdff80; border-color: #bdff80")),
              br(),
              br(),
              uiOutput("builder_select_view_1"), #enables to define later the values to be displayed on a select list, or other input control
              selectInput( #unlike the previous line, the values for the input control can be defined here and will not change
                inputId = "operator_0",
                label = "Constraint operator:",
                choices = c("=", "!=", "LOOKUP", "ONE OF", "NONE OF", ">", "<", ">=", "<=", "LIKE"),
                multiple = TRUE
              ),
              textInput( #the user writes the input 
                inputId = "value_0",
                label = "Value(s) separated by commas for the constraint",
                value = ""
              ),
              #conditionl panel dependent on actionable button to uncover control inputs
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
            bsModal("add_data_type","Overlaying extra data","add_3",size = "large", #creates a modal/pop-up window
                    #the modal window contain the 4th and 5th layer of query
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
          br(), #separation of sections with 4 line breaks
          br(),
          br(),
          br(),
          column(
            width = 12,
            #using div() the input controls can be displayed in a concrete position 
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
          #this modal window is added to delete at the end layers/conditions that have been incorporated in order to go to deeper layers for the query
          bsModal("modal_queries", "SET QUERY", "setQ", size ="large",actionButton("delete_constraints", "Delete Rows"),tags$br(), tags$br(), dataTableOutput("table_constraints")),
          br(),
          hr(),
          #conditional panel is displayed when a correct query has been created
          conditionalPanel(
            "input.setQ",
            uiOutput("builder_constraint_summary"),
            bsTooltip("builder_constraint_summary","Summarize the information about the constraints contained by an object of the class InterMineR.","top")
          )
        )
      ),
      
      tabItem(
        tabName = "results",
        
        introjsUI(), #include rintrojs
        
        useShinyFeedback(), # include shinyFeedback
        
        tags$h3("Query Results"),
        
        tabPanel( # Create a new tab
          title = "Table of results:", value="table",
          uiOutput("result_table")
        )
      ),
      
      tabItem(
        tabName = "visualization",
        
        introjsUI(), #include rintrojs
        
        tags$h3("Cytoscape Network Viewer"),
        tabPanel("Cytoscape Network Viewer", fluid = TRUE, 
                 sidebarLayout(
                   sidebarPanel( #contains inputs
                     fluidRow(
                       column(
                         width = 12,
                         tags$legend("RCytoscape visualization options:"),
                         div(style="display:inline-block",
                             selectInput("doLayout", "Select Layout:", #for organizing the network visually according to algorithms
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
                         #the options are going to be defined by the inputs (nodes) defined in Results tab
                         selectInput("selectName", "Select Node by ID:", choices = c()), 
                         selectInput("selectName_2", "Select Node by attribute:", choices = c("")), #attr selected by the user in Results
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
                   mainPanel( #contains the outputs, in this case the Network 
                     fixedRow(
                       column(
                         width = 12,
                         box( #important to include the network(plot) inside a box to keep the margins of the figure 
                           border = 0, 
                           width = 12,
                           height = 600,
                           cyjShinyOutput('cyjShiny', height = '600'),
                           style = "max-height: 100%; border: 0px solid; outline: none; outline-width: 0;" #the drawing of the box is invisble
                         ),
                         br(),
                         box(
                           width = 12,
                           div(style="display:inline-block", #the following buttons are goin to be displayed in the same line
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
                         tags$legend("Options for Node's body:"), #the following are input buttons to costumize the network
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
                         selectInput("selectName_3", "Attribute:", choices = c()), #the ones selected in the Results table
                         bsTooltip("selectName_3", "To see more attributes return to Run your query Section and choose more node's attributes."),
                         selectInput("selectName_3_attr","Value", choices = c(), multiple = TRUE),
                         br(),
                         hr(),
                         tags$h4("Continuous Mapping"),
                         selectInput("gradient_id","Choose a node attribute:",selected = NULL, choices = c()),
                         bsTooltip("gradient_id", "Select a property and obtain a linear mapping of it to colours or sizes (colour and size gradient)."),
                         radioButtons("mapping_question", "Do you want to applay a gradient?",
                                      c("Yes, a colour gradient.",
                                        "Yes, a size gradient.",
                                        "No"),
                                      selected = "No"),
                         uiOutput("ui"),
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
                             actionButton("history","History of changes")), #this buttons open a pop-up window to make changes on the selection
                         #the pop-up displays a table with each parameter changed in rows, which can be deleted
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
                         cytoscapeOutput("network", height = "900px"),
                         br(),
                         br(),
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
                             #downloa button initiate a browser download when clicked
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
                         fileInput("file", "Upload Zip file", accept = ".zip"), #file upload control
                         bsTooltip("file", "Here, you can upload saved networks and see the results of the query in a table and the interactive Network with your last modifications."),
                         actionButton("unzip", "Unzip files", style="color: #fff; background-color: #3366ff; border-color: #3366ff"),
                         bsTooltip("unzip", "Press this button to display the results table and the network."),
                         br(), # add a line break
                         br(),
                         textOutput("zipError") # Output error
                       )
                     
                   ),
                   conditionalPanel("input.unzip",{ #only if the unzip is correct
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
  #Pop-up that displays a welcoming message 
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("intro_text.html"), #renders the file "intro_text.html"
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

  ###### Redirect buttons #####

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
  ###### Functions Query Builder######
  #the following function displays the options for the 2nd level of the builded query
  get_2_choices <- function(nodes_list){
    output$builder_2_select <- renderUI({
      tagList(
        tags$h4(paste0("2nd level: ", input$b_choice)), #displays the selected option in the 2st level
        tags$b("Type of data to be returned"), #the returned/shown data is selected apart from the data to constrain
        checkboxTreeInput("b_select_2", #creates a checkbox tree
                          nodes = nodes_list), #the argument of the function is the list of "children" from the 2st level option
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
          choices = c("=", "!=", "LOOKUP", "ONE OF", "NONE OF", ">", "<", ">=", "<=", "LIKE"), #operator to filter/contrain
          multiple = TRUE #this argument is set to TRUE because multiple constraints can be created
        ),
        bsTooltip("operator_2", "Select the operators in the same order as the data is displayed in the above tree.", placement = "top"),
        textInput(
          inputId = "value_2",
          label = "Value(s) separated by commas", #limitation, if the user do not follow the indications (commas) induces an error
          value = ""
        ),
        #tooltip used to remind the user the correct procedure
        bsTooltip("value_2", "Type the values of the constraints separeted by a semicolon (;) if they are for different data types of the tree and so different constraints or by a comma (,) if they are going to be included in the same constraint."),
        actionButton("set_choice3","SET")
      )
      
    })
  }
  
  get_3_choices <- function(nodes_list){ #same as previous but for 3rd level in the query builder
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
  
  get_4_choices <- function(nodes_list){ #same as the previous but for 4th level in the query builder
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
  
  get_5_choices <- function(nodes_list){ #same as the previous but for 5th level in the query builder
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
  
  get_select_view_1 <- function(){ #this function allows to select the data "class" shown in the 1st level
    output$builder_select_view_1 <- renderUI({
      selectInput(
        inputId = "b_select_1",
        label = "Type of data to be returned",
        choices = c(),
        multiple = TRUE
      )
      
    })
  }
  
  get_select_constraint_1 <- function(){ #this function allows to select the data for a second constraint in the 1st level
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
  
  
  get_order <- function(){ #this functions defines the parameter that sets the order
    output$builder_order <- renderUI({
      selectInput(
        inputId = "b_order",
        label = "Order by",
        choices = ""
      )
    })
  }
  
  #the following function is used in the template tab to modify constraints from the template
  get_index_val <- function(){
    output$index_choice <- renderUI({
      fluidPage(
        column(
          width = 6,
          tags$b("Constraint Path:"), 
          textOutput( "m.index_t1" #it returns the path of the constraint of the template (information) 
          ),
          tags$b("Constraint Operator:"),
          textOutput( "m.op_t1" #it returns the operator of the constraint of the template (information)
          ),
          textAreaInput(
            inputId = "values_t1",
            label = "Value by default:" #it returns the argument of the constraint of the template (can be edited)
          ),
          br(),
          tags$b("Constraint Path:"), #same as the previous if the template has a second constraint
          textOutput( "m.index_t2"
          ),
          tags$b("Constraint Operator:"),
          textOutput( "m.op_t2"
          ),
          textAreaInput(
            inputId = "values_t2",
            label = "Value by default:"
          )
        ),
        column(
          width = 6,
          tags$b("Constraint Path:"),
          textOutput( "m.index_t3"
          ),
          tags$b("Constraint Operator:"),
          textOutput( "m.op_t3"
          ),
          textAreaInput(
            inputId = "values_t3",
            label = "Value by default:"
          ),
          br(),
          tags$b("Constraint Path:"),
          textOutput( "m.index_t4"
          ),
          tags$b("Constraint Operator:"),
          textOutput( "m.op_t4"
          ),
          textAreaInput(
            inputId = "values_t4",
            label = "Value by default:"
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
  ###### Template vs. Query Builder: modality reactive value #####
  
  modality <- reactiveVal() #reactive value object, variable that when change notifies it to any dependend reactive
  observeEvent(input$sideTabs, {
    if (input$sideTabs == "builder") { #when the user goes to the builder query tab creates an alert
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
        callbackR = function(x) { modality(TRUE) }, #modality reactive value is set to true
        callbackJS = NULL,
        inputId = "shinyalert"
      )}
  })
  observeEvent(input$sideTabs, {
    if (input$sideTabs == "templates") {
      modality(NULL) #when the user goes to template queries tab the modality reactive value is set to null
    }
  })
  
  ###### Choosing the Mine ######
  im <- ""
  #functions from the InterMineR package are called
  im <- reactive({initInterMine(mine = listMines()[input$mine_template])}) #saves the mine
  model_im <- reactive({getModel(initInterMine(mine = listMines()[input$mine_template]))})
  
  output$template_mine <- renderUI({
    tags$h4(paste0("You are querying in ", input$mine_template, "."))
  })
  ###### Choosing the Template ######
  output$template_choice <- renderUI({
      selectInput(
        inputId = "t_choice",
        label = "Choose a template query:",
        choices = getTemplates(im())[2], #function from InterMineR package that returns a list of templates
        selected = ""
      )
    
  })
  
  observeEvent(input$help1,
               if (input$sideTabs == "templates") {
                 introjs(session, options = list(
                   steps = data.frame(element = c("#template_choice", "#m.index_t1","#values_t1"),
                                      intro = c("When you select an element from this list a template query is given.",
                                                "You can modify pre-defined queries with constraints.",
                                                "Here type the new value for the constraint."))
                 ))
               }
  )
  
  
  q_reactive <- eventReactive(input$t_choice,{
    #getting the query contained in a template using a function from the InterMineR package
    getTemplateQuery(im(), getTemplates(im())[1][[1]][match(input$t_choice,getTemplates(im())[2][[1]])])
  })
  ###### Constraints from Template ######
  #calling the function to see and edit the constraints from the template query
  get_index_val()
  observeEvent(input$t_choice,{
    #renderText is a reactive version of function base::cat()
    #for the first constraint of the selected template
    output$m.index_t1 <- renderText({q_reactive()[[1]]$where$path[[1]]}) #the path is obtained
    output$m.op_t1 <- renderText({q_reactive()[[1]]$where$op[[1]]}) #the operator is obtained
    updateTextAreaInput(session, "values_t1",
                        value = q_reactive()[[1]]$where$value[[1]]) #the value by default is obtained
    })
  
  observeEvent(input$t_choice, {
    showFeedbackDanger(
      inputId = "values_t1",
      text = 'You can type a new value for the constraint.'
    )
  })
  
  observeEvent(input$t_choice,{
      output$m.index_t2 <- renderText({
        if(length(q_reactive()[[1]]$where$path)==2) { #condition to search for the second constraint
          q_reactive()[[1]]$where$path[[2]]
          }else{
            NULL
          }})
      output$m.op_t2 <- renderText({if(length(q_reactive()[[1]]$where$path)==2) {
        q_reactive()[[1]]$where$op[[2]]
        }else{
          NULL
        }})
      updateTextAreaInput(session, "values_t2",
                        value = 
                          if(length(q_reactive()[[1]]$where$path)==2) {q_reactive()[[1]]$where$value[[2]]
                            }else{
                              ""
                            })
  
    })
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
    output$m.index_t3 <- renderText({
      if(length(q_reactive()[[1]]$where$path)==3) {#again a condition for the third constraint of the template
        q_reactive()[[1]]$where$path[[3]]
      }else{
        NULL
      }})
    output$m.op_t3 <- renderText({if(length(q_reactive()[[1]]$where$path)==3) {
      q_reactive()[[1]]$where$op[[3]]
    }else{
      NULL
    }})
    updateTextAreaInput(session, "values_t3",
                        value = 
                          if(length(q_reactive()[[1]]$where$path)==3) {q_reactive()[[1]]$where$value[[3]]
                          }else{
                            ""
                          })
    
  })
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
    output$m.index_t4 <- renderText({
      if(length(q_reactive()[[1]]$where$path)==4) { #condition for the fourth constraint of the template
        q_reactive()[[1]]$where$path[[4]]
      }else{
        NULL
      }})
    output$m.op_t4 <- renderText({if(length(q_reactive()[[1]]$where$path)==4) {
      q_reactive()[[1]]$where$op[[4]]
    }else{
      NULL
    }})
    updateTextAreaInput(session, "values_t4",
                        value = 
                          if(length(q_reactive()[[1]]$where$path)==4) {q_reactive()[[1]]$where$value[[4]]
                          }else{
                            ""
                          })
    
  })
  observeEvent(c(input$m.index_t4,input$values_t4), {
    req(input$m.index_t4)
    if(nchar(input$values_t4) < 1) { #this is a feedback/warning message for the user to type a value if constraint path and op are set 
      showFeedbackDanger(
        inputId = "values_t4",
        text = 'Type the value for the constraint.'
      )
    } else {
      hideFeedback(inputId = "values_t4")
    }
  })
  ###### Set the Query Template ######
  #the following return the results from the (non)costumized template query in response to the events
  q_query_reactive <- eventReactive(c(input$t_choice,input$m.index_t1,input$m.index_t2, #these are the events inside a vector
                                      input$m.index_t3,input$m.index_t4,
                                      input$values_t1,input$values_t2,input$values_t3,
                                      input$values_t4),{
                                        q <- q_reactive()
                                        ind_1 <- input$m.index_t1
                                        value_1 <- input$values_t1
                                        if(!(is.null(value_1))){
                                          q_constraints <- setConstraints( #InterMineR function to modify constraints
                                            values = list(c(input$values_t1)),
                                            modifyQueryConstraints = q,
                                            m.index = 1
                                          )
                                          ind_2 <- input$m.index_t2
                                          value_2 <- input$values_t2
                                          if(value_2!=""){
                                            q_constraints <- setConstraints(
                                              values = list(value_1, value_2),
                                              modifyQueryConstraints = q,
                                              m.index = c(1,2)
                                            )
                                            ind_3 <- input$m.index_t3
                                            value_3 <- input$values_t3
                                            if(value_3!=""){
                                              q_constraints <- setConstraints(
                                                values = list(value_1, value_2, value_3),
                                                modifyQueryConstraints = q,
                                                m.index = c(1,2,3)
                                              )
                                              ind_4 <- input$m.index_t4
                                              value_4 <- input$values_t4
                                              if(value_4!=""){
                                                q_constraints <- setConstraints(
                                                  values = list(value_1, value_2, value_3, value_4), #different constraints and so different values
                                                  modifyQueryConstraints = q, #the template
                                                  m.index = c(1,2,3,4) #the index of the paths from the template
                                                )
                                              }
                                            }
                                          }
                                          
                                          q_query <- setQuery(inheritQuery = q, where = q_constraints) #combining the query template with the new constraints
                                        }
                                        else{
                                          q_query <- q #or in case there are no modifications, the template itselves
                                        }
                                      }
  )
  #the following returns a summary of the (non)modified template query in response to the events
  observeEvent(c(input$t_choice,input$values_t1,input$m.index_t2,
                 input$m.index_t3,input$m.index_t4,
                 input$values_t1,input$values_t2,input$values_t3,
                 input$values_t4),{
                   output$template_constraint_summary <- renderUI({
                     tagList(
                       box(
                         width = 12, # fill the width of the page
                         tags$h2("Summary"),
                         #only showing query information with constraints
                         if(is.list(q_query_reactive())){tags$p("No constraint set to template query.")}else{try(renderDataTable({summary(q_query_reactive())}))},
                         br(),
                         div(style="display:inline-block; float:right",
                             actionButton("goResults","Go to Results",
                                          style="color: #fff; background-color: #3366ff; border-color: #3366ff"))
                       )
                     )
                   })
                 })
  
  ###### Run Template Query ######
  results_reactive <- eventReactive(c(input$t_choice,input$m.index_t1,input$m.index_t2,
                                      input$m.index_t3,input$m.index_t4,
                                      input$values_t1,input$values_t2,input$values_t3,
                                      input$values_t4,input$goResults),{ #the more advanced in the workflow the more events
                                        #reactivity is that when a parameter of the template query is changed the results are obtained again
                                        q <- q_query_reactive()
                                        if(is.list(q)){
                                          res <- runQuery(im(), q)
                                        } else {
                                          res <- runQuery(im(), q)
                                        }
                                      })
  
  ###### First input controls and indicators in Query Builder ######
  output$builder_mine <- renderUI({
    tags$h4(paste0("You are querying in ", input$mine_template, ".")) #provides information about the mine
  })
  
  output$builder_choice <- renderUI({
    model <- model_im() #reactive variable previously defined
    model_duplicated <- duplicated(model$type)
    model$logical <- model_duplicated
    options <- subset(model, logical == FALSE)$type #the classes of the model, first selection
    
    selectInput(
      inputId = "b_choice",
      label = "Select a Data Class to Begin a Query:",
      choices = options
    )
    
  })
  
  #calling the functions we are initializing/locating the select list input controls and checkbox trees
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
  ###### Choices of Query Builder ######
  options_reactive <- eventReactive(input$b_choice,{
    opt_select <- c()
    #the choices for the first shown and constraint data are obteined from the child_name column when child_type is null
    for(element in subset(model_im(), type == input$b_choice & child_type == "")$child_name){
      opt_select <- c(opt_select, element)
    }
    opt_select
  }) 
  observe({ 
    updateSelectInput(session, "b_select_1", #shown data first level
                      choices = options_reactive())
  })
  observe({
    updateSelectInput(session, "b_constraint_1", #constraint data first level
                      choices = options_reactive())
  })
  order_reactive <- eventReactive(c(input$b_choice, input$b_select_1),{ 
    options<-input$b_select_1 #the choices to order the results are limitted here
    options
  })
  observe({
    updateSelectInput(session, "b_order", 
                      choices = order_reactive())
  })
  
  options_2_reactive <- eventReactive(input$b_choice,{
    #defining the second data type:
    #the type column has to contain the first data type and the child type column has to be non-null
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
      #the path is defined from child_name
      #taking the child_type's that have been saved with the function options_2_reactive
      #imposing the value in type column, child_type column null
      for (variable in subset(model_im(), type == element & child_type == "")$child_name) {
        #the child_name's are saved in list2_children as the label and "child_type"_"child_name" as value
        list2_children <- rlist::list.append(list2_children, list(label = variable,
                                                                  value = paste0(element,"_",variable)))
      }
      list2 <- rlist::list.append(list2,list(label = element, 
                                             value = element,
                                             children = list2_children)) #list for checkbox tree 2
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
    b_select_2 <- str_split(input[["b_select_2"]], "_") #the data type 2
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
          list3_children <- rlist::list.append(list3_children, list(label = variable, #the child_name
                                                                    value = paste0(element,"_",variable,"_",names(options)[i])))
                                                                    #child_type+child_name+type(2)
        }                                     
        list_3_sub <- rlist::list.append(list_3_sub,list(label = element, 
                                                         value = paste0(names(options)[i],"_",element),
                                                         children = list3_children))
      }
      
      list3 <- rlist::list.append(list3,list(label = names(options)[i], 
                                             value = names(options)[i],
                                             children = list_3_sub)) #for the checkbox tree of level 3
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
  
  
  ###### Feedback Danger in Query Builder #######
  #the following are functions that send messages to the user to help proceed correctly
  observeEvent(c(input$b_select_1, input$sideTabs), {
    if (input$sideTabs == "builder") {
      if(is.null(input$b_select_1)) { #because no value has been selected
        showFeedbackDanger(
          inputId = "b_select_1",
          text = "Select at least a value."
        )
      } else {
        hideFeedback(inputId = "b_select_1")
      }
    }})
  observeEvent(c(input$selectName_2_attr, input$selectName_2), {
    req(input$selectName_2) #check for required value
    if(nchar(input$selectName_2_attr)<1 & nchar(input$selectName_2)>1) {
      showFeedbackDanger(
        inputId = "selectName_2_attr",
        text = "Select a value."
      )
    } else {
      hideFeedback(inputId = "selectName_2_attr")
    }
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
  ###### Setting the Query from the Builder Query ######
  #Shown data
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
                                                       selectitems <- c() #this is going to be the first argument of setQuery
                                                       #type of data to be returned in the first level
                                                       if(!(is.null(input$b_select_1))){
                                                         try({
                                                           for (element in input$b_select_1){
                                                             element_str <- strsplit(element, " ") #creating a list of words
                                                             element_str <- element_str[[1]]
                                                             try({
                                                               for (variable in element_str[2:length(element_str)]) {
                                                                 #trying to catch the words in capital letters that are not SNP
                                                                 #future improvement: catch more exceptions such as SNP... 
                                                                 #it has been observed while building a query that some classes are not recognized due to the writting
                                                                 if(variable==toupper(variable) & variable!="SNP"){
                                                                   #the modification: capitalize (only the first letter) the variables in capital letters
                                                                   element_str <- replace(element_str, element_str==variable,capitalize(tolower(variable)))
                                                                 }
                                                               }
                                                             },silent = TRUE)
                                                             
                                                             if(element_str==toupper(element_str)){
                                                               element_str[1] <- tolower(element_str[1]) #the first entire word to lower 
                                                             }else{
                                                               element_str[1] <- decapitalize(element_str[1]) #the first letter of the first word to lower
                                                             }
                                                             element_str <- paste(element_str, collapse = "") #paste again all the splitted words together
                                                             selectitems <- c(selectitems,paste0(input$b_choice,".",element_str)) #add path 
                                                           }
                                                         })
                                                       }
                                                       #type of data to be returned in the second level
                                                       if(!(is.null(input$b_select_2))){
                                                         try({
                                                           #splitting, modify upper and lower case
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
                                                             element_str <- paste(element_str, collapse = "") #paste the modify splitted words
                                                             #taking now the child_name from the 2nd level data type and proceeding as before
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
                                                               #paths
                                                               selectitems <- c(selectitems,paste0(input$b_choice,".", #paste 1st data type
                                                                                                   b_choice_2_str,     #with 2nd data type
                                                                                                   ".",element_str))   #with the attribute of the last one
                                                             }
                                                             
                                                           }
                                                           
                                                         })
                                                       }
                                                       #type of data to be returned in the third level
                                                       if(!(is.null(input$b_select_3))){ 
                                                         #same steps, see previous if's
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
                                                                                                     ".",b_choice_3_str, #child_name of the third data type level
                                                                                                     ".",element_str))
                                                               }
                                                             }
                                                           }
                                                         })
                                                       }
                                                       #type of data to be returne in the fourth level
                                                       if(!(is.null(input$b_select_4))){
                                                         #same steps, see previous if's
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
                                                                                                       ".",b_choice_4_str, #new level, child_name of 4th type data
                                                                                                       ".",element_str))
                                                                 }
                                                                 
                                                               }
                                                               
                                                             }
                                                             
                                                           }
                                                           
                                                         })
                                                       }
                                                       #type of data to be returned in the fifth level
                                                       if(!(is.null(input$b_select_5))){
                                                         #same steps, see previous if's
                                                         try({
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
                                                                                                         ".",b_choice_5_str, #5th level data type child_name
                                                                                                         ".",element_str))
                                                                   }
                                                                   
                                                                 }
                                                                 
                                                               }
                                                               
                                                             }
                                                             
                                                           }
                                                           
                                                           
                                                         })
                                                       }
                                                       
                                                       selectitems #return
                                                     }
  )
  #Order
  q_query_reactive_builder_order <- eventReactive(c(input$b_choice,input$b_order,input$desc_asc),{
    #first split, decapitalize, paste again
    b_order_str <- strsplit(input$b_order, " ")
    b_order_str[[1]][1] <- decapitalize(b_order_str[[1]][1])
    b_order_str <- paste(b_order_str[[1]], collapse = "")
    
    order <- paste0(input$b_choice,".",b_order_str)
    sort <- c(as.character(input$desc_asc))
    names(sort) <- order 
    return(list(sort)) #list, the name of the column and the type of sorting
    
  }
  )
  #Constraint data
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
                                                            
                                                            #consider that from the first data type level there are two input controls for constraints
                                                            if(!(is.null(input$operator_0)) & !(is.null(input$value_0))){
                                                              try({
                                                                constraints_paths <- c(constraints_paths,input$b_choice)
                                                                constraints_operators <- c(constraints_operators,input$operator_0)
                                                                #if more than one value corresponds to a single constraint, they create a list
                                                                constraints_values <- rlist::list.append(constraints_values, str_split(input$value_0,","))
                                                              })
                                                            }
                                                            #treatment as with q_query_reactive_builder_select to define the paths for the query 
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
                                                            
                                                            constraints_values_list <- list() #order list
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
  #setQuery
  q_query_reactive_builder <- eventReactive(c(input$setQ, input$delete_constraints),{
    values_select$dfSelect <- q_query_reactive_builder_select()
    if (!is.null(input$table_constraints_rows_selected)) { #double check of selected paths with the table (summmary)
      #the user could had deleted some paths 
      values_select$dfSelect <- data.frame(values_select$dfSelect)
      values_select$dfSelect <- values_select$dfSelect[-as.numeric(input$table_constraints_rows_selected),]
    }
    #when is dataframe the information is inside [,1]
    if(is.data.frame(values_select$dfSelect)){
      values_select <- values_select$dfSelect[,1]
    }else{
      values_select <- values_select$dfSelect
    }
    #using the function from intermineR package 
    q_query_reactive_builder_func <- setQuery(
      select = values_select,
      orderBy = q_query_reactive_builder_order(),
      where = q_query_reactive_builder_constraints()
    )
    q_query_reactive_builder_func #return
  }) 
  
  #Summary of the query
  observeEvent(c(input$setQ,input$delete_constraints),{
    output$builder_constraint_summary <- renderUI({
      tagList(
        box(
          width = 12, # fill the width of the page
          tags$h2("Summary"),
          if(is.list(q_query_reactive_builder())){tags$p("No constraint set to template query.")}else{
            if(length(summary(q_query_reactive_builder())) == 1){try(tags$p(summary(q_query_reactive_builder())),silent=TRUE
            )}else{try(datatable(summary(q_query_reactive_builder())),silent=TRUE
            )}
          },
          div(style="display:inline-block; float:right",
              actionButton("goBuilder","Go to Results",
                           style="color: #fff; background-color: #3366ff; border-color: #3366ff"))
        )
      )
    })
  })
  
  #RunQuery
  results_reactive_builder <- eventReactive(c(input$goBuilder, input$delete_constraints),{
    q <- q_query_reactive_builder()
    if(is.list(q)){
      res <- runQuery(im(), q[[1]])
    } else {
      res <- runQuery(im(), q)
    }
  })
  ###### Table of constraints/filters in the Query Builder ######
  #enables the user to delete levels or data
  output$table_constraints <- renderDataTable({
    input$setQ
    data.frame(values_select$dfSelect)
  })
  ###### Results table UI output #####
  
  output$result_table <- renderUI({ #UI output, result_table has been defined in the ui (placed inside a tab)
    fluidRow(
      box(
        width = 12,
        column(
          width = 12,
          #elements
          tags$legend("Returning results from a query against data held inside the mine."),
          withLoader(DTOutput("resultstable", height = "100%"), loader = "loader6"),
          #formating
          bsTooltip("resultstable","Table containing the data which were retrieved from the InterMine instance."),
          div(style="display:inline-block",actionButton("options_button", "Set Nodes and Edges",style="color: #fff; background-color: #ff9900; border-color: #ff9900"),
              actionButton("help3","", icon = icon("question"), style="color: #000; background-color: #bdff80; border-color: #bdff80"))
        )),
      conditionalPanel(
        "input.options_button", #when the user clicks the button 
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
              #clickable button to move to the next tab
              actionButton("goInteraction", "Create the Network",
                           style="color: #fff; background-color: #3366ff; border-color: #3366ff")
          ))
        
      )
    )
  })
  ###### Displaying results in a datatable ######
  observeEvent(c(input$goBuilder, input$goResults),{
    output$resultstable <- renderDT({ # unique container for a unique table
      if(identical(modality(),NULL)){ #checking wheter the data comes from template or query builder 
        results <- datatable(as.data.frame(results_reactive()), fillContainer = TRUE, rownames = FALSE, options = list(
          pageLength = 25, autoWidth = TRUE))
        results #the results from template query in a datatable format to be displayed
      }else{
        results <- datatable(as.data.frame(results_reactive_builder()), fillContainer = TRUE, rownames = FALSE, options = list(
          pageLength = 25, autoWidth = TRUE))
        results #the results from the built query in a datatable format ready to be displayed
      }
    })
  })
  
  ###### Selection of Id and Source ######
  observeEvent(input$options_button,{
    if(identical(modality(),NULL)){
      #the updateSelectInput function fill the "choices" argument with options
      #it is used because the variables are reactive and so they change
      updateSelectInput(session, "id_nodes",
                        choices = names(results_reactive())) #the id's are obtained
    }else{
      updateSelectInput(session, "id_nodes",
                        choices = names(results_reactive_builder()))
    }
    
  })
  observeEvent(c(input$id_nodes,input$id_edges), {
    req(input$id_nodes)
    req(input$id_edges)
    if(input$id_nodes==input$id_edges) {
      showFeedbackDanger( #warning message, the Id and the Source cannot be the same, nodes and edges
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
  observeEvent(input$options_button,{ #the attributes selected in this step are the parameters to play with the network visualization
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
  
  ###### Dataframe of Results converted to graphNEL class ######
  interaction_reactive_func <- function(dataframe){
    df <- as.data.frame(dataframe)
    
    
    nodes <- data.frame(id = unique(c(df[,input$id_nodes], df[,input$id_edges])), 
                        stringsAsFactors = FALSE)
    
    edges <- df %>%
      dplyr::select(source = input$id_nodes,
                    target = input$id_edges) %>%
      dplyr::mutate(interaction = paste(source, '_', target)) 
    
    i_graph <- graph_from_data_frame(edges, directed = TRUE, nodes) #conversion to igraph
    
    for(element in input$edges_attributes){
      edge_attr(i_graph, element) <- df[,element]
    }
    for(element in input$nodes_attributes){
      df<- df[!duplicated(df[,input$id_nodes]),] #duplicated id nodes are eliminated
      vertex_attr(i_graph, element) <- df[,element]
    }
    
    g<-igraph.to.graphNEL(i_graph) #conversion to graphNEL class
    
  }
  
  interaction_reactive <- eventReactive(c(input$t_choice,input$m.index_t1,input$m.index_t2,
                                          input$m.index_t3,input$m.index_t4,
                                          input$values_t1,input$values_t2,input$values_t3,
                                          input$values_t4,input$goResults,
                                          input$goInteraction,input$id_nodes,input$id_edges,input$nodes_attributes,input$edges_attributes),{
                                            
                                            interaction_reactive_func(results_reactive())
                                          })
  interaction_reactive_builder <- eventReactive(c(input$goInteraction,input$id_nodes,input$id_edges,
                                                  input$nodes_attributes,input$edges_attributes),{
                                                    
                                                    interaction_reactive_func(results_reactive_builder())
  })
  
  ###### Options of the Visualize your results tab ###### 
  observeEvent(input$goInteraction,{
    if(identical(modality(),NULL)){
      updateSelectInput(session, "selectName", #Select Node by ID
                        choices = c("",nodes(interaction_reactive()))) #retrieves the nodes ID of the graphNEL
    }else{
      updateSelectInput(session, "selectName",
                        choices = c("",nodes(interaction_reactive_builder())))
    }
  })
  
  observeEvent(input$goInteraction,{
    if(identical(modality(),NULL)){
      updateSelectInput(session, "delete_nodes", #same selection 
                        choices = c(nodes(interaction_reactive())))
    }else{
      updateSelectInput(session, "delete_nodes",
                        choices = c(nodes(interaction_reactive_builder())))
    }
  })
  
  nodes_attr_reactive <- reactive({ #to read as a reactive variable 
    input$nodes_attributes
  })
  observeEvent(input$goInteraction, {
    if(identical(modality(),NULL)){
      updateSelectInput(session, "selectName_2", #select node by attribute
                        choices = c("",nodes_attr_reactive()))
    }else{
      updateSelectInput(session, "selectName_2",
                        choices = c("",nodes_attr_reactive()))
    }
    
  })
  
  observeEvent(input$selectName_2, ignoreInit = TRUE, { #once the attribute is set, choose the value
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
  observeEvent(input$selectName,  ignoreInit=TRUE,{
    printf("about to sendCustomMessage, selectNodes")
    session$sendCustomMessage(type="selectNodes", message=list(input$selectName)) 
    #using selectNodes function from cyjShiny in the form of a custom message to the web page
    #to select nodes by ID in the network 
  })
  
  observeEvent(input$selectName_2_attr,  ignoreInit=FALSE,{
    if(identical(modality(),NULL)){
      printf("about to sendCustomMessage, selectNodes")
      df <- results_reactive()
      
      node_i <- df[,input$selectName_2]==input$selectName_2_attr #the nodes that have the attribute
      node <- df[node_i,input$id_nodes] #the node ID is got 
      
      for (element in node){ #iterate because the function selectNodes only accepts one node
        session$sendCustomMessage(type="selectNodes", message=list(element))
        #selecting nodes by attribute value
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
    #select the first neighbors
    session$sendCustomMessage(type="sfn", message=list())
  })
  
  observeEvent(input$fit, ignoreInit=TRUE, {
    fit(session, 80) #pixels
  })
  
  observeEvent(input$fitSelected,  ignoreInit=TRUE,{
    printf("about to call R function fitSelected")
    #the current selected nodes fill the display
    fitSelected(session, 100)
  })
  
  observeEvent(input$getSelectedNodes, ignoreInit=TRUE, {
    output$selectedNodesDisplay <- renderText({" "})
    getSelectedNodes(session)
    #print the ID's of the node selection 
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
    printf("about to sendCustomMessage, doLayout: %s", strategy) #layout using the specified strategy
    #the strategies that are able: cola, cose, circle, concentric, grid, breadthfirst, random, dagre, cose-bilkent 
    doLayout(session, strategy)
  })
  
  observeEvent(input$hideSelection, ignoreInit = TRUE, {
    #all selected nodes and their edges are hidden
    hideSelection(session)
  })
  
  #as it is desired that the nodes hiden in the visualize your results tab kept hiden in the next step
  #reactive expressions are defined
  hidennodes <- reactiveVal() #for invert selected and remove selected buttons
  hidennodes_builder <- reactiveVal() 
  
  observeEvent(c(input$clearSelection, input$showAll),{
    if(identical(modality(),NULL)){
      #after a selection, the user can decide to unselect everything and so the hidennodes reactive value is set to null
      hidennodes(NULL)
    }else{
      hidennodes_builder(NULL)
    }
  })
  
  #to keep the deletes in the next tab
  observeEvent(input$hideSelection, ignoreInit=TRUE, {
    getSelectedNodes(session) #when Remove Selected is pressed 
  })
  
  observeEvent(input$selectedNodes, {
    if(input$hideSelection != 0){ #if Remove Selected has been pressed
      newNodes_hide <- input$selectedNodes;
      if(identical(modality(),NULL)){ #templates
        if(is.null(hidennodes())){ #considering non previous deletes
          df <- results_reactive()
          for (node in newNodes_hide){
            if (node %in% df[,input$id_nodes]){ #nodes or edges separation
              df <- df[!df[,input$id_nodes]==node,] #keep those nodes non selected 
            } else if (node %in% df[,input$id_edges]) { #here edges
              df <- df[!df[,input$id_edges]==node,] 
            }
          }
        } else { #considering previous deletes 
          df <- as.data.frame(hidennodes())
          for (node in newNodes_hide){
            if (node %in% df[,input$id_nodes]){ #nodes or edges separation
              df <- df[!df[,input$id_nodes]==node,]
            } else if (node %in% df[,input$id_edges]) {
              df <- df[!df[,input$id_edges]==node,] 
            }
          }
        }
      } else { #query builder
        if(is.null(hidennodes_builder())){
          df <- results_reactive_builder()
          for (node in newNodes_hide){
            if (node %in% df[,input$id_nodes]){ #nodes or edges separation
              df <- df[!df[,input$id_nodes]==node,]
            } else if (node %in% df[,input$id_edges]) {
              df <- df[!df[,input$id_edges]==node,]
            }
          }
        } else {
          df <- as.data.frame(hidennodes_builder())
          for (node in newNodes_hide){
            if (node %in% df[,input$id_nodes]){ #nodes or edges separation
              df <- df[!df[,input$id_nodes]==node,]
            } else if (node %in% df[,input$id_edges]) {
              df <- df[!df[,input$id_edges]==node,]
            }
          }
        }
      }
      if (identical(modality(), NULL)){
        hidennodes(df) #defining and saving the new data frame in a reactive value
      } else {
        hidennodes_builder(df)
      }
      updateActionButton(session, "hideSelection", "Remove Selected") #update the action button 
    
    } else {
      return()
    }
  })
  
  #for the template query pathway
  new_df <- eventReactive(c(input$hideSelection, input$clearSelection, input$showAll, input$goOverlaid1),{
    if (is.null(hidennodes())){ 
      df <- results_reactive()
    } else { #invert selected
      df <- as.data.frame(hidennodes())
    }
    df
  })
  
  #for the built query pathway
  new_df_builder <- eventReactive(c(input$hideSelection, input$clearSelection, input$showAll, input$goOverlaid1),{
    if (is.null(hidennodes_builder())){
      df <- results_reactive_builder()
    } else {     
      df <- as.data.frame(hidennodes_builder())
    }
    df
  })
  
  observeEvent(input$invertSelection, ignoreInit = TRUE, {
    #selected nodes and edges are hidden
    invertSelection(session)
  })
  
  observeEvent(input$showAll, ignoreInit = TRUE, {
    #all unselected and shown
    showAll(session)
  })
  
  ###### Options of the Overlay additional data tab ###### 
  observeEvent(input$goOverlaid1,{
    output$ui <- renderUI({
      if (is.null(input$mapping_question))
        return()
      
      # Depending on input$mapping_question, we'll generate a different
      # UI component and send it to the client.
      switch(input$mapping_question,
             "Yes, a colour gradient." = {
               box(width = 12, 
                   colorSelectorInput("range_color1", "Choose the first colour:",
                                                    choices = c("yellow", 
                                                                "orange",
                                                                "red",
                                                                "magenta",
                                                                "blue",
                                                                "cyan",
                                                                "green")),
                   bsTooltip("range_color1", "This colour is the minimum."),
                   colorSelectorInput("range_color2", "Choose the second colour:",
                                      choices = rev(c("yellow", 
                                                      "orange",
                                                      "red",
                                                      "magenta",
                                                      "blue",
                                                      "cyan",
                                                      "green"))),
                   bsTooltip("range_color2", "This colour is the maximum."),
                  sliderInput("range_color_numeric", "Specify the range of values to map:",min = 1, max = 1000,
                             value = c(200,500)),
                  bsTooltip("range_color_numeric", "Between the minimum and maximum value of the attribute.")
                 )},
             "Yes, a size gradient." = {
               box(width = 12,
                 sliderInput("range_size", "Choose the extremes of the gradient:",
                             min = 10, max = 200, value = c(10,200), step = 5, round = TRUE),
                 bsTooltip("range_size", "These are the minimum and maximum sizes for the visual style."),
                 sliderInput("range_size_numeric", "Specify the range of values to map:",min = 1, max = 1000,
                             value = c(200,500)),
                 bsTooltip("range_size_numeric", "Between the minimum and maximum value of the attribute.")
                 )},
             "No" = {return()}
      )
    })
  })
  observeEvent(c(input$mapping_question, input$gradient_id), {
    if(nchar(input$gradient_id)>2){
      updateSelectInput(session, "attr_color_grad",
                      choices = c(nodes_attr_reactive()))
      updateSelectInput(session, "attr_size_grad", #select node by attribute
                        choices = c(nodes_attr_reactive())) 
    }
  })
  observeEvent(c(input$mapping_question, input$gradient_id), {
    if(nchar(input$gradient_id)>2){
      if(identical(modality(),NULL)){
        df <- new_df()
        ids = unique(c(df[,input$id_nodes], df[,input$id_edges]))
        data <- data.frame(id = character(),    # Create empty data frame
                           gradient_id = numeric(),
                           stringsAsFactors = FALSE)
        n=1
        for(i in ids){
          list <- df[df[, input$id_nodes] == i,]
          ii <- list[[input$gradient_id]]
          data[n,]<-list(i,as.numeric(ii[1]))
          n <- n+1
        }
        updateSliderInput(session,"range_color_numeric",
                          min = min(data[,2],na.rm = TRUE), max = max(data[,2], na.rm = TRUE),
                          value = c(min(data[,2],na.rm = TRUE),max(data[,2], na.rm = TRUE))
                          ) 
      }else{
        df <- as.data.frame(results_reactive_builder())
        ids = unique(c(df[,input$id_nodes], df[,input$id_edges]))
        data <- data.frame(id = character(),    # Create empty data frame
                           gradient_id = numeric(),
                           stringsAsFactors = FALSE)
        n=1
        for(i in ids){
          list <- df[df[, input$id_nodes] == i,]
          ii <- list[[input$gradient_id]]
          data[n,]<-list(i,as.numeric(ii[1]))
          n <- n+1
        }
        updateSliderInput(session,"range_color_numeric",
                          min = min(data[,2],na.rm = TRUE), max = max(data[,2], na.rm = TRUE) ,
                          value = c(min(data[,2],na.rm = TRUE),max(data[,2], na.rm = TRUE))
        )
      }
    }
  })
  observeEvent(c(input$mapping_question, input$gradient_id), {
    if(nchar(input$gradient_id)>2){
      if(identical(modality(),NULL)){
        df <- new_df()
        ids = unique(c(df[,input$id_nodes], df[,input$id_edges]))
        data <- data.frame(id = character(),    # Create empty data frame
                           gradient_id = numeric(),
                           stringsAsFactors = FALSE)
        n=1
        for(i in ids){
          list <- df[df[, input$id_nodes] == i,]
          ii <- list[[input$gradient_id]]
          data[n,]<-list(i,as.numeric(ii[1]))
          n <- n+1
        }
        updateSliderInput(session,"range_size_numeric",
                          min = min(data[,2], na.rm = TRUE), max = max(data[,2], na.rm = TRUE),
                          value = c(min(data[,2],na.rm = TRUE),max(data[,2], na.rm = TRUE))
                          )
      }else{
        df <- as.data.frame(results_reactive_builder())
        ids = unique(c(df[,input$id_nodes], df[,input$id_edges]))
        data <- data.frame(id = character(),    # Create empty data frame
                           gradient_id = numeric(),
                           stringsAsFactors = FALSE)
        n=1
        for(i in ids){
          list <- df[df[, input$id_nodes] == i,]
          ii <- list[[input$gradient_id]]
          data[n,]<-list(i,as.numeric(ii[1]))
          n <- n+1
        }
        updateSliderInput(session,"range_size_numeric",
                          min = min(data[,2],na.rm = TRUE), max = max(data[,2], na.rm = TRUE) ,
                          value = c(min(data[,2],na.rm = TRUE),max(data[,2], na.rm = TRUE))
                          )
      }
    }
  })
  observeEvent(input$goOverlaid1, {
    if(identical(modality(),NULL)){
      updateSelectInput(session, "selectName_3", #select node by attribute
                        choices = c("",c(nodes_attr_reactive()))) 
    }else{
      updateSelectInput(session, "selectName_3",
                        choices = c("",c(nodes_attr_reactive())))
    }
    
  })
  
  observeEvent(input$selectName_3, ignoreInit = TRUE, {
    if(identical(modality(),NULL)){
      df <- new_df()
      updateSelectInput(session, "selectName_3_attr", #select the attribute value
                        choices = c("",df[,input$selectName_3]))
    }else{
      df <- new_df_builder()
      updateSelectInput(session, "selectName_3_attr",
                        choices = c("",df[,input$selectName_3]))
    }
  })
  
  ###### Wrapping cytoscape.js html widget in Visualize your results tab ######
  
  output$cyjShiny <- renderCyjShiny({
    if(identical(modality(),NULL)){
      g3 <- interaction_reactive()
      graph <- graphToJSON(g3)
      cyjShiny(graph, layoutName="cola", height = 600) #by default the cola layout is set
    }else{
      g3 <- interaction_reactive_builder()
      graph <- graphToJSON(g3)
      cyjShiny(graph, layoutName="cola", height = 600)
    }
  })
  #the html widget can be downloaded in png format
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
    if(nchar(input$filenameViewer)<1){
      printf(paste0("writing png to ","network",format(Sys.time(), "%m%d_%H%M"),".png"))
      conn <- file(paste0("network",format(Sys.time(), "%m%d_%H%M"),".png"), "wb")
    }else{
      printf(paste0("writing png to ",input$filenameViewer,".png"))
      conn <- file(paste0(input$filenameViewer,".png"), "wb")
    }
    writeBin(png.parsed.binary, conn)
    close(conn)
  })
  #info message of the download
  observeEvent(input$downloadviewer, {
      shinyalert(
        title = "The download is complete.",
        text = paste("You can find the file in ",getwd()," directory."),
        type = "info",
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
        className = "", #modality reactive value is set to true
        callbackJS = NULL,
        inputId = "shinyalert"
      )
  })
  
  ###### Preparing nodes and edges dataframes for Overlay tab ######
  #the following function returns edges dataframe
  style_edges_reactive_func <- function(data_frame, id_nodes, id_edges){
    df <- data_frame
    nodes <- data.frame(id = unique(c(df[,id_nodes], df[,id_edges])), stringsAsFactors = FALSE) #only non-repited
    edges <- df %>%
      dplyr::select(source = all_of(id_nodes),
                    target = all_of(id_edges)) %>%
      dplyr::mutate(interaction = paste(source, '_', target))
  }
  
  style_edges_reactive <- eventReactive(c(input$t_choice,input$m.index_t1,input$m.index_t2,
                                          input$m.index_t3,input$m.index_t4,
                                          input$values_t1,input$values_t2,input$values_t3,
                                          input$values_t4,input$goResults,input$goOverlaid1, input$hideSelection, input$clearSelection, input$invertSelection, input$showAll, 
                                          input$goInteraction,input$id_nodes,input$id_edges,input$nodes_attributes,input$edges_attributes),{
                                            
                                            #applaying the previous function in the template query
                                            style_edges_reactive_func(new_df(),input$id_nodes, input$id_edges)
                                            
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
                                                  input$goOverlaid1,input$hideSelection, input$clearSelection, input$invertSelection, input$showAll,input$goBuilder,
                                                  input$goInteraction,input$id_nodes,input$id_edges,input$nodes_attributes,input$edges_attributes),{
                                                    
                                                    style_edges_reactive_func(new_df_builder(),input$id_nodes,input$id_edges)
                                                    
                                                  })
  ###### Cytoscape network chart ######
  observeEvent(c(input$hideSelection, input$clearSelection, input$showAll, input$goOverlaid1),{
    if(identical(modality(),NULL)){
      #cytoscape network chart
      #nodes and edges data frames are the arguments of the function
      plotInput <- cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>%
        layout('breadthfirst', directed = TRUE) %>%
        panzoom()
      saveWidget(plotInput, "temp.html", selfcontained = FALSE)
      output$network <- renderCytoscape({
        # draw the network
        cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>%
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
  
  ###### Overlaying options ######
  observeEvent(c(input$hideSelection, input$clearSelection, input$showAll, input$goOverlaid1),{
    if(identical(modality(),NULL)){
      df <- new_df()
      options <- unique(c(df[,input$id_nodes], df[,input$id_edges]))
      #select nodes by ID
      updateSelectInput(session, "selectid",
                        choices = c("",options))
      updateSelectInput(session,"gradient_id",
                        choices = c("",c(nodes_attr_reactive())))
    }else{
      df <- new_df_builder()
      options <- unique(c(df[,input$id_nodes], df[,input$id_edges]))
      #select nodes by ID
      updateSelectInput(session, "selectid",
                        choices = c("",options))
      updateSelectInput(session,"gradient_id",
                        choices = c("",c(nodes_attr_reactive())))
    }
    
  })
  
  observeEvent(input$select_parameter,{
    #options of the parameters that can be edited
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
      options <- c("10","20","50","70","90","100","150") #size in pixels (unit)
    }else{
      options <- c("Orange"="#ff8c1a", #the option visible for the user to select is the name and the code is the argument
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
  
  ###### Delete overlaying ######
  #for templates
  rv <- NULL
  values <- reactiveValues(dfWorking = rv) #a dataframe where the edited parameters are saved in rows, initialized here
  #for builder
  rv_builder <- NULL
  rv_select <- NULL
  values_builder <- reactiveValues(dfWorking_builder = rv_builder)
  values_select <- reactiveValues(dfSelect = rv_select)
  
  #in the pop-up window the edited parameters can be deleted, going to the original network
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

  ###### Aplaying the overlays ######
  observeEvent(c(input$button_set,input$layoutcytoscape, input$downloadstyle),{
    if(identical(modality(),NULL)){
      df <- new_df()
      node<-NULL
      node_i<-NULL
      try(node_i<-df[,input$selectName_3]==input$selectName_3_attr, silent = TRUE)
      try(node<-df[node_i,input$id_nodes], silent = TRUE) #getting the ID
      
      
      if(is.null(input$selectid)){
        if(is.null(input$selectName_3_attr)){ #if the user has not decided to edit any parameter
          strategy <- input$layoutcytoscape #the user only has changed the layout strategy
        }else{ #the user has decided to edit nodes by attribute
          for (element in data.frame(rbind(node,input$selectName_3_attr))){
            #the dfworking dataframe is constructed defining 4 columns: Nodes, Attribute, Parameter, Selection
            df_2 <- data.frame("Nodes"=element[1], "Attribute"=paste0(input$selectName_3," = ", element[2]), "Parameter"=input$select_parameter,"Selection"=input$select_parameter_option)
            values$dfWorking <- rbind(values$dfWorking, df_2)
            df_2 <- NULL
          }
        }
      }else{ #the user has decided to edit nodes by ID
        for (element in input$selectid){
          df_2 <- data.frame("Nodes"=element,"Attribute"="ID", "Parameter"=input$select_parameter,"Selection"=input$select_parameter_option)
          values$dfWorking <- rbind(values$dfWorking, df_2)        
          df_2 <- NULL
          
        }
      }
      #the following piece of code resets the IDs, the user can create a new overlay
      isolate({ #to read reactive values without establishing a relationship with the caller (non re-execution)
        options <- unique(c(df[,input$id_nodes], df[,input$id_edges]))
        updateSelectInput(session, "selectid",
                          choices = c("",options))
      })
      #the following piece of code resets the editable parameters, the user can create a new overlay
      isolate({
        updateSelectInput(session,"select_parameter",
                          choices = c("", "Background colour"="background-color",
                                      "Shape"="shape",
                                      "Size"="size"
                          ))
      })
      #the following piece of code resets the attributes, the user can create a new overlay
      isolate({
        updateSelectInput(session, "selectName_3",
                          choices = c("",c(nodes_attr_reactive())))
        
      })
      #code to call cytoscape function and update the network
      if(is.null(values$dfWorking)){ #if there are no overlays
        if(input$mapping_question == "No"  | nchar(input$gradient_id)<2){
          strategy <- input$layoutcytoscape
          printf("about to sendCustomMessage, layout: %s", strategy)
          
          if(strategy=="cola"){
            plotInput <- cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
              cola_layout(avoidOverlap = TRUE) %>%
              panzoom()
            saveWidget(plotInput, "temp.html", selfcontained = FALSE)
            output$network <- renderCytoscape({
              #workflow_zip2(new_df(),values$dfWorking)
              cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
                cola_layout(avoidOverlap = TRUE) %>%
                panzoom()
            })
          }else{ 
            plotInput <- cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
              layout(strategy, avoidOverlap = TRUE) %>%
              panzoom()
            saveWidget(plotInput, "temp.html", selfcontained = FALSE)
            output$network <- renderCytoscape({
              #workflow_zip2(new_df(),values$dfWorking)
              cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
                layout(strategy, avoidOverlap = TRUE) %>%
                panzoom()
            })
          }
        }else{
          if(input$mapping_question == "Yes, a colour gradient."){
            if(strategy == "cola"){
              #workflow_zip2(new_df(),values$dfWorking)
              plotInput <- cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
                #the first argument, previous = sign, of node_style is a recognised cytoscape node style name
                #the second argument is the label given in the dataframe
                node_style('background-color' = paste0('mapData(gradient,',
                                                       as.character(input$range_color_numeric[1]),', ',
                                                       as.character(input$range_color_numeric[2]),', ',
                                                       input$range_color1,', ',
                                                       input$range_color2,')')) %>% 
                
                cola_layout(avoidOverlap = TRUE) %>%
                panzoom()
            }else{
              #workflow_zip2(new_df(),values$dfWorking)
              plotInput <- cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
                node_style('background-color' = paste0('mapData(gradient,',
                                                       as.character(input$range_color_numeric[1]),',',
                                                       as.character(input$range_color_numeric[2]),',',
                                                       input$range_color1,',',
                                                       input$range_color2,')')) %>% 
                
                layout(strategy, avoidOverlap = TRUE) %>%
                panzoom()
            }
            saveWidget(plotInput, "temp.html", selfcontained = FALSE)
            output$network <- renderCytoscape({
              strategy <- input$layoutcytoscape
              printf("about to sendCustomMessage, layout: %s", strategy)
              if(strategy == "cola"){
                #workflow_zip2(new_df(),values$dfWorking)
                cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
                  node_style('background-color' = paste0('mapData(gradient,',
                                                         as.character(input$range_color_numeric[1]),',',
                                                         as.character(input$range_color_numeric[2]),',',
                                                         input$range_color1,',',
                                                         input$range_color2,')')) %>% 
                  
                  cola_layout(avoidOverlap = TRUE) %>%
                  panzoom()
              }else{
                #workflow_zip2(new_df(),values$dfWorking)
                cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
                  node_style('background-color' = paste0('mapData(gradient,',
                                                         as.character(input$range_color_numeric[1]),',',
                                                         as.character(input$range_color_numeric[2]),',',
                                                         input$range_color1,',',
                                                         input$range_color2,')')) %>% 
                  
                  layout(strategy, avoidOverlap = TRUE) %>%
                  panzoom()
              }
            })
          }else{
            if(strategy == "cola"){
              #workflow_zip2(new_df(),values$dfWorking)
              plotInput <- cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
                #the first argument, previous = sign, of node_style is a recognised cytoscape node style name
                #the second argument is the label given in the dataframe
                
                node_style('width' = paste0('mapData(gradient,',
                                            as.character(input$range_size_numeric[1]),',',
                                            as.character(input$range_size_numeric[2]),',',
                                            input$range_size[1],',',
                                            input$range_size[2],')')) %>%
                node_style('height' = paste0('mapData(gradient,',
                                             as.character(input$range_size_numeric[1]),',',
                                             as.character(input$range_size_numeric[2]),',',
                                             input$range_size[1],',',
                                             input$range_size[2],')')) %>%
       
                cola_layout(avoidOverlap = TRUE) %>%
                panzoom()
            }else{
              #workflow_zip2(new_df(),values$dfWorking)
              plotInput <- cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
                
                node_style('width' = paste0('mapData(gradient,',
                                            as.character(input$range_size_numeric[1]),',',
                                            as.character(input$range_size_numeric[2]),',',
                                            input$range_size[1],',',
                                            input$range_size[2],')')) %>%
                node_style('height' = paste0('mapData(gradient,',
                                             as.character(input$range_size_numeric[1]),',',
                                             as.character(input$range_size_numeric[2]),',',
                                             input$range_size[1],',',
                                             input$range_size[2],')')) %>%
                
                layout(strategy, avoidOverlap = TRUE) %>%
                panzoom()
            }
            saveWidget(plotInput, "temp.html", selfcontained = FALSE)
            output$network <- renderCytoscape({
              strategy <- input$layoutcytoscape
              printf("about to sendCustomMessage, layout: %s", strategy)
              if(strategy == "cola"){
                #workflow_zip2(new_df(),values$dfWorking)
                cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
              
                  node_style('width' = paste0('mapData(gradient,',
                                              as.character(input$range_size_numeric[1]),',',
                                              as.character(input$range_size_numeric[2]),',',
                                              input$range_size[1],',',
                                              input$range_size[2],')')) %>%
                  node_style('height' = paste0('mapData(gradient,',
                                               as.character(input$range_size_numeric[1]),',',
                                               as.character(input$range_size_numeric[2]),',',
                                               input$range_size[1],',',
                                               input$range_size[2],')')) %>%
                 
                  cola_layout(avoidOverlap = TRUE) %>%
                  panzoom()
              }else{
                #workflow_zip2(new_df(),values$dfWorking)
                cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
                  
                  node_style('width' = paste0('mapData(gradient,',
                                              as.character(input$range_size_numeric[1]),',',
                                              as.character(input$range_size_numeric[2]),',',
                                              input$range_size[1],',',
                                              input$range_size[2],')')) %>%
                  node_style('height' = paste0('mapData(gradient,',
                                               as.character(input$range_size_numeric[1]),',',
                                               as.character(input$range_size_numeric[2]),',',
                                               input$range_size[1],',',
                                               input$range_size[2],')')) %>%
                 
                  layout(strategy, avoidOverlap = TRUE) %>%
                  panzoom()
              }
            })
          }
        }
        
      }else{ #with overlays
        strategy <- input$layoutcytoscape
        if(input$mapping_question == "No"| nchar(input$gradient_id)<2){
          if(strategy == "cola"){
            #workflow_zip2(new_df(),values$dfWorking)
            plotInput <- cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
              #the first argument, previous = sign, of node_style is a recognised cytoscape node style name
              #the second argument is the label given in the dataframe
              node_style('background-color' = 'data(node_color)') %>% 
              node_style('shape' = 'data(node_shape)') %>%
              node_style('width' = 'data(node_width)') %>% #size defines width and heigth
              node_style('height' = 'data(node_height)') %>%
              cola_layout(avoidOverlap = TRUE) %>%
              panzoom()
          }else{
            #workflow_zip2(new_df(),values$dfWorking)
            plotInput <- cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
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
              cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
                node_style('background-color' = 'data(node_color)') %>%
                node_style('shape' = 'data(node_shape)') %>%
                node_style('width' = 'data(node_width)') %>%
                node_style('height' = 'data(node_height)') %>%
                cola_layout(avoidOverlap = TRUE) %>%
                panzoom()
            }else{
              #workflow_zip2(new_df(),values$dfWorking)
              cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
                node_style('background-color' = 'data(node_color)') %>%
                node_style('shape' = 'data(node_shape)') %>%
                node_style('width' = 'data(node_width)') %>%
                node_style('height' = 'data(node_height)') %>%
                layout(strategy, avoidOverlap = TRUE) %>%
                panzoom()
            }
          })
          
        }else{
          if(input$mapping_question == "Yes, a colour gradient."){
            if(strategy == "cola"){
              #workflow_zip2(new_df(),values$dfWorking)
              plotInput <- cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
                #the first argument, previous = sign, of node_style is a recognised cytoscape node style name
                #the second argument is the label given in the dataframe
                node_style('background-color' = paste0('mapData(gradient,',
                                                       as.character(input$range_color_numeric[1]),',',
                                                       as.character(input$range_color_numeric[2]),',',
                                                       input$range_color1,',',
                                                       input$range_color2,')')) %>% 
                node_style('shape' = 'data(node_shape)') %>%
                node_style('width' = 'data(node_width)') %>%
                node_style('height' = 'data(node_height)') %>%
                cola_layout(avoidOverlap = TRUE) %>%
                panzoom()
            }else{
              plotInput <- cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
                node_style('background-color' = paste0('mapData(gradient,',
                                                       as.character(input$range_color_numeric[1]),',',
                                                       as.character(input$range_color_numeric[2]),',',
                                                       input$range_color1,',',
                                                       input$range_color2,')')) %>% 
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
                cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
                  node_style('background-color' = paste0('mapData(gradient,',
                                                         as.character(input$range_color_numeric[1]),',',
                                                         as.character(input$range_color_numeric[2]),',',
                                                         input$range_color1,',',
                                                         input$range_color2,')')) %>% 
                  node_style('shape' = 'data(node_shape)') %>%
                  node_style('width' = 'data(node_width)') %>%
                  node_style('height' = 'data(node_height)') %>%
                  cola_layout(avoidOverlap = TRUE) %>%
                  panzoom()
              }else{
                #workflow_zip2(new_df(),values$dfWorking)
                cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
                  node_style('background-color' = paste0('mapData(gradient,',
                                                         as.character(input$range_color_numeric[1]),',',
                                                         as.character(input$range_color_numeric[2]),',',
                                                         input$range_color1,',',
                                                         input$range_color2,')')) %>% 
                  node_style('shape' = 'data(node_shape)') %>%
                  node_style('width' = 'data(node_width)') %>%
                  node_style('height' = 'data(node_height)') %>%
                  layout(strategy, avoidOverlap = TRUE) %>%
                  panzoom()
              }
            })
          }else{
            if(strategy == "cola"){
              #workflow_zip2(new_df(),values$dfWorking)
              plotInput <- cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
                #the first argument, previous = sign, of node_style is a recognised cytoscape node style name
                #the second argument is the label given in the dataframe
                node_style('background-color' = 'data(node_color)') %>% 
                node_style('shape' = 'data(node_shape)') %>%
                node_style('width' = paste0('mapData(gradient,',
                                            as.character(input$range_size_numeric[1]),',',
                                            as.character(input$range_size_numeric[2]),',',
                                            input$range_size[1],',',
                                            input$range_size[2],')')) %>%
                node_style('height' = paste0('mapData(gradient,',
                                             as.character(input$range_size_numeric[1]),',',
                                             as.character(input$range_size_numeric[2]),',',
                                             input$range_size[1],',',
                                             input$range_size[2],')')) %>%
                cola_layout(avoidOverlap = TRUE) %>%
                panzoom()
            }else{
              plotInput <- cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
                node_style('background-color' = 'data(node_color)') %>% 
                node_style('shape' = 'data(node_shape)') %>%
                node_style('width' = paste0('mapData(gradient,',
                                            as.character(input$range_size_numeric[1]),',',
                                            as.character(input$range_size_numeric[2]),',',
                                            input$range_size[1],',',
                                            input$range_size[2],')')) %>%
                node_style('height' = paste0('mapData(gradient,',
                                             as.character(input$range_size_numeric[1]),',',
                                             as.character(input$range_size_numeric[2]),',',
                                             input$range_size[1],',',
                                             input$range_size[2],')')) %>%
                layout(strategy, avoidOverlap = TRUE) %>%
                panzoom()
            }
            saveWidget(plotInput, "temp.html", selfcontained = FALSE)
            output$network <- renderCytoscape({
              strategy <- input$layoutcytoscape
              printf("about to sendCustomMessage, layout: %s", strategy)
              if(strategy == "cola"){
                cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
                  node_style('background-color' = 'data(node_color)') %>% 
                  node_style('shape' = 'data(node_shape)') %>%
                  node_style('width' = paste0('mapData(gradient,',
                                              as.character(input$range_size_numeric[1]),',',
                                              as.character(input$range_size_numeric[2]),',',
                                              input$range_size[1],',',
                                              input$range_size[2],')')) %>%
                  node_style('height' = paste0('mapData(gradient,',
                                               as.character(input$range_size_numeric[1]),',',
                                               as.character(input$range_size_numeric[2]),',',
                                               input$range_size[1],',',
                                               input$range_size[2],')')) %>%
                  cola_layout(avoidOverlap = TRUE) %>%
                  panzoom()
              }else{
                cytoscape(nodes = style_custom_nodes_reactive_gradient(), edges = style_edges_reactive()) %>% 
                  node_style('background-color' = 'data(node_color)') %>% 
                  node_style('shape' = 'data(node_shape)') %>%
                  node_style('width' = paste0('mapData(gradient,',
                                              as.character(input$range_size_numeric[1]),',',
                                              as.character(input$range_size_numeric[2]),',',
                                              input$range_size[1],',',
                                              input$range_size[2],')')) %>%
                  node_style('height' = paste0('mapData(gradient,',
                                               as.character(input$range_size_numeric[1]),',',
                                               as.character(input$range_size_numeric[2]),',',
                                               input$range_size[1],',',
                                               input$range_size[2],')')) %>%
                  layout(strategy, avoidOverlap = TRUE) %>%
                  panzoom()
              }
            })
          }
        }
      
      
      node <- NULL
      node_i <- NULL
    }}else{ #exactly the same but with the data from a built query
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
        options <- unique(c(df[,input$id_nodes], df[,input$id_edges]))
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
      
      if(is.null(values_builder$dfWorking_builder)){ #if there are no overlays
        if(input$mapping_question == "No"  | nchar(input$gradient_id)<2){
          strategy <- input$layoutcytoscape
          printf("about to sendCustomMessage, layout: %s", strategy)
          
          if(strategy=="cola"){
            plotInput <- cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
              cola_layout(avoidOverlap = TRUE) %>%
              panzoom()
            saveWidget(plotInput, "temp.html", selfcontained = FALSE)
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
          if(input$mapping_question == "Yes, a colour gradient."){
            if(strategy == "cola"){
              #workflow_zip2(new_df(),values$dfWorking)
              plotInput <- cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
                #the first argument, previous = sign, of node_style is a recognised cytoscape node style name
                #the second argument is the label given in the dataframe
                node_style('background-color' = paste0('mapData(gradient,',
                                                       as.character(input$range_color_numeric[1]),', ',
                                                       as.character(input$range_color_numeric[2]),', ',
                                                       input$range_color1,', ',
                                                       input$range_color2,')')) %>% 
                
                cola_layout(avoidOverlap = TRUE) %>%
                panzoom()
            }else{
              #workflow_zip2(new_df(),values$dfWorking)
              plotInput <- cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
                node_style('background-color' = paste0('mapData(gradient,',
                                                       as.character(input$range_color_numeric[1]),',',
                                                       as.character(input$range_color_numeric[2]),',',
                                                       input$range_color1,',',
                                                       input$range_color2,')')) %>% 
                
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
                  node_style('background-color' = paste0('mapData(gradient,',
                                                         as.character(input$range_color_numeric[1]),',',
                                                         as.character(input$range_color_numeric[2]),',',
                                                         input$range_color1,',',
                                                         input$range_color2,')')) %>% 
                  
                  cola_layout(avoidOverlap = TRUE) %>%
                  panzoom()
              }else{
                #workflow_zip2(new_df(),values$dfWorking)
                cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
                  node_style('background-color' = paste0('mapData(gradient,',
                                                         as.character(input$range_color_numeric[1]),',',
                                                         as.character(input$range_color_numeric[2]),',',
                                                         input$range_color1,',',
                                                         input$range_color2,')')) %>% 
                  
                  layout(strategy, avoidOverlap = TRUE) %>%
                  panzoom()
              }
            })
          }else{
            if(strategy == "cola"){
              #workflow_zip2(new_df(),values$dfWorking)
              plotInput <- cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
                #the first argument, previous = sign, of node_style is a recognised cytoscape node style name
                #the second argument is the label given in the dataframe
                
                node_style('width' = paste0('mapData(gradient,',
                                            as.character(input$range_size_numeric[1]),',',
                                            as.character(input$range_size_numeric[2]),',',
                                            input$range_size[1],',',
                                            input$range_size[2],')')) %>%
                node_style('height' = paste0('mapData(gradient,',
                                             as.character(input$range_size_numeric[1]),',',
                                             as.character(input$range_size_numeric[2]),',',
                                             input$range_size[1],',',
                                             input$range_size[2],')')) %>%
                
                cola_layout(avoidOverlap = TRUE) %>%
                panzoom()
            }else{
              #workflow_zip2(new_df(),values$dfWorking)
              plotInput <- cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
                
                node_style('width' = paste0('mapData(gradient,',
                                            as.character(input$range_size_numeric[1]),',',
                                            as.character(input$range_size_numeric[2]),',',
                                            input$range_size[1],',',
                                            input$range_size[2],')')) %>%
                node_style('height' = paste0('mapData(gradient,',
                                             as.character(input$range_size_numeric[1]),',',
                                             as.character(input$range_size_numeric[2]),',',
                                             input$range_size[1],',',
                                             input$range_size[2],')')) %>%
                
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
                  
                  node_style('width' = paste0('mapData(gradient,',
                                              as.character(input$range_size_numeric[1]),',',
                                              as.character(input$range_size_numeric[2]),',',
                                              input$range_size[1],',',
                                              input$range_size[2],')')) %>%
                  node_style('height' = paste0('mapData(gradient,',
                                               as.character(input$range_size_numeric[1]),',',
                                               as.character(input$range_size_numeric[2]),',',
                                               input$range_size[1],',',
                                               input$range_size[2],')')) %>%
                  
                  cola_layout(avoidOverlap = TRUE) %>%
                  panzoom()
              }else{
                #workflow_zip2(new_df(),values$dfWorking)
                cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
                  
                  node_style('width' = paste0('mapData(gradient,',
                                              as.character(input$range_size_numeric[1]),',',
                                              as.character(input$range_size_numeric[2]),',',
                                              input$range_size[1],',',
                                              input$range_size[2],')')) %>%
                  node_style('height' = paste0('mapData(gradient,',
                                               as.character(input$range_size_numeric[1]),',',
                                               as.character(input$range_size_numeric[2]),',',
                                               input$range_size[1],',',
                                               input$range_size[2],')')) %>%
                  
                  layout(strategy, avoidOverlap = TRUE) %>%
                  panzoom()
              }
            })
          }
        }
        
      }else{ #with overlays
        strategy <- input$layoutcytoscape
        if(input$mapping_question == "No"| nchar(input$gradient_id)<2){
          if(strategy == "cola"){
            #workflow_zip2(new_df(),values$dfWorking)
            plotInput <- cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
              #the first argument, previous = sign, of node_style is a recognised cytoscape node style name
              #the second argument is the label given in the dataframe
              node_style('background-color' = 'data(node_color)') %>% 
              node_style('shape' = 'data(node_shape)') %>%
              node_style('width' = 'data(node_width)') %>% #size defines width and heigth
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
          
        }else{
          if(input$mapping_question == "Yes, a colour gradient."){
            if(strategy == "cola"){
              #workflow_zip2(new_df(),values$dfWorking)
              plotInput <- cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
                #the first argument, previous = sign, of node_style is a recognised cytoscape node style name
                #the second argument is the label given in the dataframe
                node_style('background-color' = paste0('mapData(gradient,',
                                                       as.character(input$range_color_numeric[1]),',',
                                                       as.character(input$range_color_numeric[2]),',',
                                                       input$range_color1,',',
                                                       input$range_color2,')')) %>% 
                node_style('shape' = 'data(node_shape)') %>%
                node_style('width' = 'data(node_width)') %>%
                node_style('height' = 'data(node_height)') %>%
                cola_layout(avoidOverlap = TRUE) %>%
                panzoom()
            }else{
              plotInput <- cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
                node_style('background-color' = paste0('mapData(gradient,',
                                                       as.character(input$range_color_numeric[1]),',',
                                                       as.character(input$range_color_numeric[2]),',',
                                                       input$range_color1,',',
                                                       input$range_color2,')')) %>% 
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
                cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
                  node_style('background-color' = paste0('mapData(gradient,',
                                                         as.character(input$range_color_numeric[1]),',',
                                                         as.character(input$range_color_numeric[2]),',',
                                                         input$range_color1,',',
                                                         input$range_color2,')')) %>% 
                  node_style('shape' = 'data(node_shape)') %>%
                  node_style('width' = 'data(node_width)') %>%
                  node_style('height' = 'data(node_height)') %>%
                  cola_layout(avoidOverlap = TRUE) %>%
                  panzoom()
              }else{
                #workflow_zip2(new_df(),values$dfWorking)
                cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
                  node_style('background-color' = paste0('mapData(gradient,',
                                                         as.character(input$range_color_numeric[1]),',',
                                                         as.character(input$range_color_numeric[2]),',',
                                                         input$range_color1,',',
                                                         input$range_color2,')')) %>% 
                  node_style('shape' = 'data(node_shape)') %>%
                  node_style('width' = 'data(node_width)') %>%
                  node_style('height' = 'data(node_height)') %>%
                  layout(strategy, avoidOverlap = TRUE) %>%
                  panzoom()
              }
            })
          }else{
            if(strategy == "cola"){
              #workflow_zip2(new_df(),values$dfWorking)
              plotInput <- cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
                #the first argument, previous = sign, of node_style is a recognised cytoscape node style name
                #the second argument is the label given in the dataframe
                node_style('background-color' = 'data(node_color)') %>% 
                node_style('shape' = 'data(node_shape)') %>%
                node_style('width' = paste0('mapData(gradient,',
                                            as.character(input$range_size_numeric[1]),',',
                                            as.character(input$range_size_numeric[2]),',',
                                            input$range_size[1],',',
                                            input$range_size[2],')')) %>%
                node_style('height' = paste0('mapData(gradient,',
                                             as.character(input$range_size_numeric[1]),',',
                                             as.character(input$range_size_numeric[2]),',',
                                             input$range_size[1],',',
                                             input$range_size[2],')')) %>%
                cola_layout(avoidOverlap = TRUE) %>%
                panzoom()
            }else{
              plotInput <- cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
                node_style('background-color' = 'data(node_color)') %>% 
                node_style('shape' = 'data(node_shape)') %>%
                node_style('width' = paste0('mapData(gradient,',
                                            as.character(input$range_size_numeric[1]),',',
                                            as.character(input$range_size_numeric[2]),',',
                                            input$range_size[1],',',
                                            input$range_size[2],')')) %>%
                node_style('height' = paste0('mapData(gradient,',
                                             as.character(input$range_size_numeric[1]),',',
                                             as.character(input$range_size_numeric[2]),',',
                                             input$range_size[1],',',
                                             input$range_size[2],')')) %>%
                layout(strategy, avoidOverlap = TRUE) %>%
                panzoom()
            }
            saveWidget(plotInput, "temp.html", selfcontained = FALSE)
            output$network <- renderCytoscape({
              strategy <- input$layoutcytoscape
              printf("about to sendCustomMessage, layout: %s", strategy)
              if(strategy == "cola"){
                cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
                  node_style('background-color' = 'data(node_color)') %>% 
                  node_style('shape' = 'data(node_shape)') %>%
                  node_style('width' = paste0('mapData(gradient,',
                                              as.character(input$range_size_numeric[1]),',',
                                              as.character(input$range_size_numeric[2]),',',
                                              input$range_size[1],',',
                                              input$range_size[2],')')) %>%
                  node_style('height' = paste0('mapData(gradient,',
                                               as.character(input$range_size_numeric[1]),',',
                                               as.character(input$range_size_numeric[2]),',',
                                               input$range_size[1],',',
                                               input$range_size[2],')')) %>%
                  cola_layout(avoidOverlap = TRUE) %>%
                  panzoom()
              }else{
                cytoscape(nodes = style_custom_nodes_reactive_builder(), edges = style_edges_reactive_builder()) %>% 
                  node_style('background-color' = 'data(node_color)') %>% 
                  node_style('shape' = 'data(node_shape)') %>%
                  node_style('width' = paste0('mapData(gradient,',
                                              as.character(input$range_size_numeric[1]),',',
                                              as.character(input$range_size_numeric[2]),',',
                                              input$range_size[1],',',
                                              input$range_size[2],')')) %>%
                  node_style('height' = paste0('mapData(gradient,',
                                               as.character(input$range_size_numeric[1]),',',
                                               as.character(input$range_size_numeric[2]),',',
                                               input$range_size[1],',',
                                               input$range_size[2],')')) %>%
                  layout(strategy, avoidOverlap = TRUE) %>%
                  panzoom()
              }
            })
          }
        }
        
        node <- NULL
        node_i <- NULL
      }
      
      node <- NULL
      node_i <- NULL
    }
  })
  
  ###### Table for the History of Changes ######
  output$table1 <- renderDataTable({
    if(identical(modality(),NULL)){
      input$button_set
      unique(values$dfWorking)
    }else{
      input$button_set
      unique(values_builder$dfWorking_builder)
    }
  })
  ###### Saving the results from overlaying ######
  observe({ #saving an image in png format
    output$ggsave_graph <- downloadHandler(
      filename = function() {ifelse(input$imagenameStyle=="", paste0("network",format(Sys.time(), "%m%d_%H%M"),".png"), #png
                                    paste0(input$imagenameStyle,".png"))},
      content = function(file) {
        webshot("temp.html", file = file , cliprect = "viewport")
        #as I am using this function I cannot save in vectorial format (EPS)
      }
    )
  })
  
  #observeEvent(input$save_graph_eps,{
    #setEPS()
    #postscript("whatever.eps")
    #cytoscape(nodes = style_custom_nodes_reactive(), edges = style_edges_reactive())
    #dev.off()
  #}) this does not work
  
  observe({#saving a zip folder with the results (csv of the previous step), customization (csv), ids (csv), network (json)
    if(identical(modality(),NULL)){
      output$downloadstyle <- downloadHandler(
        filename <- function() { #filename of the zip and extension
          ifelse(input$filenameStyle=="",paste0("workflow_final",format(Sys.time(), "%m%d_%H%M"),".zip"), # default name of the zip
                 paste0(input$filenameStyle,".zip"))
        }, 
        content <- function(file) {
          temp <- tempdir() # Set a temp dir
          setwd(tempdir()) #the content is written to the temp dir
          # Create the files
          if(is.null(values$dfWorking)){
            results_table_path <- paste0("results_",format(Sys.time(), "%m%d_%H%M"),".csv") #month day _ hour minute
            customization_table_path <- paste0("customization_",format(Sys.time(), "%m%d_%H%M"),".txt")
            ids_path <- paste0("ids_",format(Sys.time(), "%m%d_%H%M"),".csv") 
            json_path <- paste0("network_",format(Sys.time(), "%m%d_%H%M"),".json")
            
            utils::write.csv(new_df(), results_table_path, row.names = TRUE)
            write("", customization_table_path)
            utils::write.csv(data.frame(nodes=input$id_nodes, edges=input$id_edges), ids_path)
            write(dataFramesToJSON(style_edges_reactive(), style_custom_nodes_reactive_gradient()),json_path) 
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
            write(dataFramesToJSON(style_edges_reactive(), style_custom_nodes_reactive_gradient()),json_path) 
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
            write(dataFramesToJSON(style_edges_reactive(), style_custom_nodes_reactive_gradient()),json_path)
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
            write(dataFramesToJSON(style_edges_reactive(), style_custom_nodes_reactive_gradient()),json_path)
            # Create a zip of the data
            zip::zipr(zipfile = file, files = c(results_table_path, customization_table_path, ids_path, json_path))
          }
        },
        contentType = "application/zip")
    }  
  })
  
  ###### Styling nodes ######
  #function that modifies the nodes dataframe with the overlays
  style_nodes_reactive <- function(data_frame,data_frame_working, id_nodes, id_edges){
    df <- data_frame #original
    custom_df <- data_frame_working #overlays
    if( is.null(custom_df)){
      nodes <- data.frame(id = unique(c(df[,id_nodes], df[,id_edges]))) %>%
        dplyr::mutate(node_color = "#595959") %>% #background color by default
        dplyr::mutate(node_width = "10") %>% #size by defaults
        dplyr::mutate(node_height = "10") %>%
        dplyr::mutate(node_shape = "ellipse") #shape by default
    }else{
      custom_background <- subset(custom_df,Parameter == "background-color", select = c("Nodes","Selection")) #dataframe subset with only background-color
      custom_shape <- subset(custom_df,Parameter == "shape", select = c("Nodes","Selection")) #dataframe subset with only shape
      custom_size <- subset(custom_df, Parameter == "size", select = c("Nodes","Selection")) #dataframe subset with only size
      
      nodes <- data.frame(id = unique(c(df[,id_nodes], df[,id_edges]))) %>%
        #Define node colours
        #mutate adds a new label to the dataframe nodes, node_color
        dplyr::mutate(node_color = ifelse(id %in% c(custom_background$Nodes), #taking the values from the subset dataframe
                                          c(custom_background$Selection),
                                          #subset(custom_background,Nodes==id)$Selection,
                                          "#595959")) %>% #the nodes that are not modified have the background color by default
        # Define node width
        #mutate adds a new label to the dataframe nodes, node_width
        dplyr::mutate(node_width = ifelse(id %in% c(custom_size$Nodes), #the first argument is the condition
                                          c(custom_size$Selection),
                                          "10")) %>%
        # Define node height
        #mutate adds a new label to the dataframe nodes, node_height
        dplyr::mutate(node_height = ifelse(id %in% c(custom_size$Nodes),
                                           c(custom_size$Selection),
                                           "10")) %>%
        
        # Define node shapes
        #mutate adds a new label to the dataframe nodes, node_shape
        dplyr::mutate(node_shape = ifelse(id %in% c(custom_shape$Nodes),
                                          c(custom_shape$Selection),
                                          "ellipse")) 
      
    }
  }
  style_nodes_reactive_mapping <- function(data_frame,data_frame_working, id_nodes, id_edges, attr_size_grad, attr_color_grad){
    if(input$mapping_question == "No"| nchar(input$gradient_id)<2){
      df <- data_frame #original
      custom_df <- data_frame_working #overlays
      if( is.null(custom_df)){
        nodes <- data.frame(id = unique(c(df[,id_nodes], df[,id_edges]))) %>%
          dplyr::mutate(node_color = "#595959") %>% #background color by default
          dplyr::mutate(node_width = "10") %>% #size by defaults
          dplyr::mutate(node_height = "10") %>%
          dplyr::mutate(node_shape = "ellipse") #shape by default
      }else{
        custom_background <- subset(custom_df,Parameter == "background-color", select = c("Nodes","Selection")) #dataframe subset with only background-color
        custom_shape <- subset(custom_df,Parameter == "shape", select = c("Nodes","Selection")) #dataframe subset with only shape
        custom_size <- subset(custom_df, Parameter == "size", select = c("Nodes","Selection")) #dataframe subset with only size
        
        nodes <- data.frame(id = unique(c(df[,id_nodes], df[,id_edges]))) %>%
          #Define node colours
          #mutate adds a new label to the dataframe nodes, node_color
          dplyr::mutate(node_color = ifelse(id %in% c(custom_background$Nodes), #taking the values from the subset dataframe
                                            c(custom_background$Selection),
                                            #subset(custom_background,Nodes==id)$Selection,
                                            "#595959")) %>% #the nodes that are not modified have the background color by default
          # Define node width
          #mutate adds a new label to the dataframe nodes, node_width
          dplyr::mutate(node_width = ifelse(id %in% c(custom_size$Nodes), #the first argument is the condition
                                            c(custom_size$Selection),
                                            "10")) %>%
          # Define node height
          #mutate adds a new label to the dataframe nodes, node_height
          dplyr::mutate(node_height = ifelse(id %in% c(custom_size$Nodes),
                                             c(custom_size$Selection),
                                             "10")) %>%
          
          # Define node shapes
          #mutate adds a new label to the dataframe nodes, node_shape
          dplyr::mutate(node_shape = ifelse(id %in% c(custom_shape$Nodes),
                                            c(custom_shape$Selection),
                                            "ellipse")) 
        
      }
    }else{
      df <- data_frame
      if(input$mapping_question == "Yes, a colour gradient."){
        ids = unique(c(df[,id_nodes], df[,id_edges]))
        data <- data.frame(id = character(),    # Create empty data frame
                           gradient_id = numeric(),
                           stringsAsFactors = FALSE)
        n=1
        for(i in ids){
          list <- df[df[, id_nodes] == i,]
          ii <- list[[attr_color_grad]]
          data[n,]<-list(i,as.numeric(ii[1]))
          n <- n+1
        }
      }else{
        ids = unique(c(df[,id_nodes], df[,id_edges]))
        data <- data.frame(id = character(),    # Create empty data frame
                           gradient_id = numeric(),
                           stringsAsFactors = FALSE)
        n=1
        for(i in ids){
          list <- df[df[, id_nodes] == i,]
          ii <- list[[attr_size_grad]]
          data[n,]<-list(i,as.numeric(ii[1]))
          n <- n+1
        }
      }
      custom_df <- data_frame_working #overlays
      if( is.null(custom_df)){
        nodes <- data.frame(id = unique(c(df[,id_nodes], df[,id_edges])), gradient = data[,2]) %>%
          dplyr::mutate(node_color = "#595959") %>% #background color by default
          dplyr::mutate(node_width = "10") %>% #size by defaults
          dplyr::mutate(node_height = "10") %>%
          dplyr::mutate(node_shape = "ellipse") #shape by default
      }else{
        custom_background <- subset(custom_df,Parameter == "background-color", select = c("Nodes","Selection")) #dataframe subset with only background-color
        custom_shape <- subset(custom_df,Parameter == "shape", select = c("Nodes","Selection")) #dataframe subset with only shape
        custom_size <- subset(custom_df, Parameter == "size", select = c("Nodes","Selection")) #dataframe subset with only size
        
        nodes <- data.frame(id = unique(c(df[,id_nodes], df[,id_edges])), gradient = data[,2]) %>%
          #Define node colours
          #mutate adds a new label to the dataframe nodes, node_color
          dplyr::mutate(node_color = ifelse(id %in% c(custom_background$Nodes), #taking the values from the subset dataframe
                                            c(custom_background$Selection),
                                            "#595959")) %>% #the nodes that are not modified have the background color by default
          # Define node width
          #mutate adds a new label to the dataframe nodes, node_width
          dplyr::mutate(node_width = ifelse(id %in% c(custom_size$Nodes), #the first argument is the condition
                                            c(custom_size$Selection),
                                            "10")) %>%
          # Define node height
          #mutate adds a new label to the dataframe nodes, node_height
          dplyr::mutate(node_height = ifelse(id %in% c(custom_size$Nodes),
                                             c(custom_size$Selection),
                                             "10")) %>%
          
          # Define node shapes
          #mutate adds a new label to the dataframe nodes, node_shape
          dplyr::mutate(node_shape = ifelse(id %in% c(custom_shape$Nodes),
                                            c(custom_shape$Selection),
                                            "ellipse")) 
        
      }
    }
  }
  
 
  style_custom_nodes_reactive_gradient <- eventReactive(c(input$t_choice,input$m.index_t1,input$m.index_t2,
                                                 input$m.index_t3,input$m.index_t4,
                                                 input$values_t1,input$values_t2,input$values_t3,
                                                 input$values_t4,input$goResults,input$goOverlaid1,input$hideSelection, input$invertSelection, input$clearSelection, input$showAll, 
                                                 input$goInteraction,input$id_nodes,input$id_edges,input$nodes_attributes,input$button_set,
                                                 input$edges_attributes, input$deleteRows, input$gradient_id),{
                                                  
                                                   style_nodes_reactive_mapping(new_df(),values$dfWorking, input$id_nodes, input$id_edges, input$gradient_id, input$gradient_id)
                                                 })
  
  style_custom_nodes_reactive_builder <- eventReactive(c(input$b_choice,input$b_2_choice,input$b_3_choice,
                                                         input$b_4_choice,input$b_5_choice,
                                                         input$b_select_1,input$b_constraint_1,input$b_order,
                                                         input$desc_asc, input$b_select_2, input$b_constraint_2, 
                                                         input$b_select_3, input$b_constraint_3, input$b_select_4,
                                                         input$b_constraint_4, input$b_select_5, input$b_constraint_5,
                                                         input$operator_0, input$value_0, input$operator_1, input$value_1,
                                                         input$operator_2, input$value_2, input$operator_3, input$value_3,
                                                         input$operator_4, input$value_4, input$operator_5, input$value_5,input$deleteRows, input$button_set,
                                                         input$goOverlaid1,input$hideSelection, input$invertSelection, input$clearSelection, input$showAll,input$goBuilder,
                                                         input$goInteraction,input$id_nodes,input$id_edges,input$nodes_attributes,input$edges_attributes, 
                                                         input$gradient_id),{
                                                           
                                                           style_nodes_reactive_mapping(new_df_builder(), values_builder$dfWorking_builder, input$id_nodes, input$id_edges, input$gradient_id, input$gradient_id)
                                                         })
  
  ###### Unzip and display saved Networks ######
  observeEvent(input$unzip,{
    filename_glob <- "*0"
    output_dir = tempdir() #dir where saves unzip data
    setwd(tempdir())
    # Unzip data in output_dir
    unzip <- utils::unzip(input$file$datapath, list = TRUE, overwrite = TRUE, exdir = output_dir)
    ls_content <- unzip$Name
    print(ls_content)
    # Displaying the results from saved queries in a data table
    output$resultstable_save <- renderDT({
      results <- datatable(as.data.frame(read.csv(ls_content[1])), fillContainer = TRUE, rownames = FALSE, options = list(
        pageLength = 25, autoWidth = TRUE))
      results
    })
    if(str_sub(ls_content[2],-1)=="t"){ #if the format from the second file is .txt (last chr is t) the network has not got overlays
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
  
  ###### Help buttons ######
  observeEvent(input$help2,
               if (input$sideTabs == "builder") {
                 introjs(session, options = list(
                   #element are the Shiny elements, input controls, outputs, actionbuttons...
                   steps = data.frame(element = c("#builder_choice", "#builder_select_view_1","#value_0",
                                                  "#constraint_button_1","#set_begin","#add_3","#setQ"),
                                      #the text that will be displayed
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
                                                "You can select one of the attributes set in the Run Query Section to filter the customization. You must select the value for the attribute if you have chosen this way.",
                                                "Do not forget to press this button each time you describe a new feature for the network or you have set a colour or size gradient.",
                                                "Pressing this button you can see the changes you have done and delete some of them."))
                 ))
               }
  )
  ###### Dynamis dropdown menu with Source code info ######
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
  #end of ui
}

# Call ui and server to create a new session in browser which display the app----
shinyApp(ui, server, options = list(launch.browser = TRUE))
