# ImmunoCompare Application
# A multi-omics comparison tool for target/biomarker discovery


library(RPostgres)
library(jsTreeR)
library(bslib)

library(shiny)
library(reactable)
library(htmltools)
library(shinyWidgets)
library(shinyBS)
library(shinybusy)
library(shinyalert)
library(rintrojs)
library(shinyjs)
library(jsonlite)
library(tidyr)
library(dplyr)
library(stringr)
library(data.table)
library(reshape2)

library(ggplot2)
library(RColorBrewer)
library(pheatmap)
library(rvest)
library(circlize)
library(gridExtra)
library(VennDiagram)

library(poolr)

library(DT)
library(reactable)
library(kableExtra)
library(shinylogs)
library(rstudioapi)
library(RSQLite)
#options(repos = c(BiocManager::repositories()))
library(WebGestaltR)

#==============================================================================================================
# Import functions used to generate table summaries, table outputs, expression plots, etc
#==============================================================================================================
source("./global.R")
source("./functions/database_query.R")
source("./functions/dataset_selection.R")
source("./functions/comparison_summary.R")
source("./functions/comparison_tables.R")
source("./functions/plot_functions.R")
source("./functions/enrichment.R")
#==============================================================================================================


#==============================================================================================================
# Create the connection to the Immunoverse DB
#==============================================================================================================
dbPath <- "./scCompare.db"
db <- dbConnect(RSQLite::SQLite(),dbPath)

#==============================================================================================================

#### JS START ####
registerInputHandler("x.child", function(x, ...) {
  fromJSON(toJSON(x, auto_unbox = TRUE, null = "null"), 
           simplifyDataFrame = FALSE)
}, force = TRUE)


#==============================================================================================================

#### Define the UI for the application ####
ui <- navbarPage(
  title = "scCompare",
  id = "navbar-tabset",
  includeCSS("www/style.css"),
  useShinyjs(),
  tags$head(
    tags$script(HTML("
  $(document).ready(function(){
    var header = $('.navbar > .container-fluid');
    header.css({
      'display':'flex',
      'flex-direction': 'row',
      'justify-content': 'space-between',
      'align-items': 'center'
    });
    
    var titleAndTabs = header.children().not('.navbar-text');
    titleAndTabs.css({
      'flex-grow': '1',
    });
    
    $('.navbar-nav').css({
      'margin-left': '-15px',
      'padding-left': '15px'
    });
    
    var devText = '<div style=\"color:white; margin-left: auto; padding-top:8px\"><h5>Developed by Immunology Computational Biology - GRC</h5></div>';
    header.append(devText);
  })                                                                                                           
      ")),
    tags$script('
      $(document).on("click", "#start-button", function() {
        Shiny.setInputValue("start_button", true);
      });
    '),
    tags$style(HTML("
        .container {
            max-width: 960px;
            margin: 0 auto;
            padding: 15px;
        }
        
        .btn-primary {
            border: 2px solid #26de5a;
            color: white;
            background-color: #26de5a;
        }
        
        .lead {
            font-size: 1.25rem;
            color: #333;
            text-align: justify;
            white-space: normal;
            overflow-wrap: break-word;
            word-wrap: break-word;
        }
        
        h3 {
            color: #333;
            text-transform: uppercase;
            text-align: center;
            letter-spacing: .1em;
            line-height: 1.2;
        }
        
        .dataset-image {
            max-width: 100%;
            height: auto;
            display: block;
            margin-left: auto;
            margin-right: auto;
        }
        
        .responsive-image {
            max-width: 100%;
            height: auto;
        }
        
        .text-center {
            text-align: center;
        }
        
        .text-justify {
            text-align: justify;
        }
        
        .small {
            font-size: 0.875rem;
            color: #666;
        }
        
        .text-muted {
            color: #6c757d;
        }
        
        .mt-5 {
            margin-top: 3rem;
        }
        
        .mb-4 {
            margin-bottom: 1.5rem;
        }
        
        .description-text {
            font-size:16px;
            line-height: 1.5;
            word-wrap: break-word;
            max-width: 100%;
            text-align: justify;
        }
        
        @media (max-width: 768px) {
          .description-text {
            font-size: 14px;
          }
        }
        
        .input-row {
          display: flex;
          justify-content: space-between;
          align-items: flex-start;
          flex-wrap: wrap;
          width: 100%
        }
        
        .input-row .left-items {
          display: flex;
          align-items: flex-start;
          flex-wrap: wrap;
        }
        
        .input-row > * {
          margin-right: 10px;
          flex: 1 1 auto;
        }
        
        .input-row .left-items > * {
          margin-right: 10px;
          flex: 1 1 auto;
        }
        
        .download-button-container {
          display: flex;
          align-items: center;
        }

    ")),
    
    tags$style(HTML("
        #gene_option .radio label{
          font-size: 24px;
        }
        
        #toggleSection {
          display: block;
          margin-bottom: 10px;
          font-size: 16px;
          color: blue;
          cursor: pointer;
        }
        #toggleSection i {
          margin-right: 5px;
        }
        
              /* Custom CSS to remove row striping */
        .custom-datatable thread th, .custom-datatable tbody td {
        padding: 8px;
        text-align: center;
        }
        .custom-datatable tbody tr:nth-child(even){
        background-color: inherit;
        }
        
        .custom-datatable tbody tr:nth-child(odd){
        background-color: inherit;
        }
        
        .datatable-container {
        margin-left: auto;
        margin-right: auto;
        width: 90%;
        }
        
        #toggleContainer{
        width: auto;
        display: inline-block;
        margin: 10px;
        vertical-align: top;
        }
        
        .bs-select-all {
        display: none !important;
        }
        
        .bs-deselect-all {
        display: none !important;
        }
        
        .selected {background-color:grey !important;}
      ")),
    tags$script(HTML('
        $(document).ready(function() {
          $(".section .header").click(function() {
            var section = $(this).parent();
            if (section.hasClass("expanded")) {
              section.removeClass("expanded");
              section.find(".content").slideUp();
              section.find(".expand-icon").removeClass("fa-chevron-down").addClass("fa-chevron-right");
            } else {
              $(".section.expanded").removeClass("expanded");
              $(".section.expanded .content").slideUp();
              $(".section.expanded .expand-icon").removeClass("fa-chevron-down").addClass("fa-chevron-right");
              section.addClass("expanded");
              section.find(".content").slideDown();
              section.find(".expand-icon").removeClass("fa-chevron-right").addClass("fa-chevron-down");
            }
          });
        });
    ')),
    
    tags$script(src = "popover.js"),
    tags$script(HTML("
        $(document).ready(function() {
          $('body').popover({
            selector: '[data-toggle=\"popover\"]',
            html: true,
            trigger: 'hover',
            container: 'body'
          });
        });
        $(document).ready(function(){
          $('[data-toggle=\"popover\"]').popover({html: true}); 
        });
        $(document).ready(function() {
          $('#toggle').click(function() {
            $('#collapse').collapse('toggle');
            var icon = $(this).find('i');
            icon.toggleClass('fa-plus fa-minus');
          });
        });
      ")),
    
    tags$link(rel= "stylesheet", href= "https://unpkg.com/tippy.js@6/dist/tippy.css"),
    tags$script(src = "https://unpkg.com/@popperjs/core@2"),
    tags$script(src = "https://unpkg.com/tippy.js@6"),
    tags$script(HTML('
        $(document).ready(function() {
         $(document).on("click", ".show-description", function() {
          var desc = $(this).data("description");
          $("#description-modal .modal-body").text(desc);
          $("#description-modal").modal("show");
         });
        })')),
    tags$div(id= "description-modal", class = "modal fade", tabindex = "-1", role= "dialog",
             tags$div(class = "modal-dialog", role= "document",
                      tags$div(class = "modal-content",
                               tags$div(class = "modal-header",
                                        tags$button(type = "button", class= "close", `data-dismiss`="modal", `aria-label`="Close",
                                                    tags$span(`aria-hidden`="true", HTML("&times;"))),
                                        tags$h4(class="modal-title", "Description")),
                               tags$div(class= "modal-body")))),
    
    
    tags$style(HTML("
      #gene_summary_tab .dt-buttons {
        /* margin-bottom: -7px; */
        position: absolute;
        bottom: 10;
        left: 0;
        right: 0;
        text-align: center;
      }
  
      .checkbox-inline { 
        margin-left: 0px;
        margin-right: 10px;
      }
     .checkbox-inline+.checkbox-inline {
       margin-left: 0px;
        margin-right: 10px;
      }")),
    
    tags$script(HTML('
            function headerCallback(thead, data, start, end, display) {
        var $ths = $(thead).find("th");
        $ths.css({"vertical-align": "bottom","text-align": "center", "white-space": "nowrap"});
            }
      
      $(document).on("change", "input[type=checkbox]", function(){
      var $this = $(this);
      var isChecked = $(this).prop("checked");
      var checkboxName = $this.attr("name");
      var counterpartName = checkboxName === "include" ? "exclude" : "include";
      var columnIndex = $this.closest("td").index();
      
      if (isChecked){
      $("td:nth-child(" + (columnIndex + 1) + ") input[name=\'" + counterpartName+ "\']").prop("checked",false);
      }
      }
      );
        
      $(document).on("change", "input[type=checkbox]", function() {
        var include = $("input[name=include]:checked").map(function() {
          return this.value;
        }).get();
        
        var exclude = $("input[name=exclude]:checked").map(function() {
          return this.value;
        }).get();
        
        Shiny.setInputValue("checkbox_values", {include: include, exclude: exclude}, {priority: "event"});
      });
      '))
  ),
  
  ### HOME TAB ###
  tabPanel("Home", selected="Home",
           column(10, offset = 1,
                  #wellPanel(style = "background-color: #fff; border-color: #2c3e50;",
                            #add_busy_spinner(spin = 'pixel', position = 'bottom-right'),
                            includeHTML("homepage.html")) #)
  ),
  
  ### DATASET SELECTION TAB ###
  tabPanel("Dataset Selection",
           column(3,
                  wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 1150px;",
                            h4("REQUIRED", style = "text-align: center;"),
                            div(
                              class = "section",
                              div(
                                style = "margin-bottom: 5px;",
                                h4("Select cell types of interest"),
                                jstreeOutput("tree1")
                              ),   
                            ),
                            br(),
                            
                            div(
                              style = "margin-bottom: 5px;",
                              selectInput(
                                inputId = "disease_input",
                                label = "Select diseases of interest",
                                choices = NULL,
                                multiple = TRUE
                              )
                            ),
                            br(),
                            
                            h4("OPTIONAL", style = "text-align: center;"),
                            br(),
                            
                            div(
                              style = "margin-bottom: 5px;",
                              selectInput(
                                inputId = "tissue_input",
                                label = "Select tissues of interest",
                                choices = NULL,
                                multiple = TRUE
                              )
                            ),
                            br(),
                            
                            div(
                              style = "margin-bottom: 5px;",
                              selectInput(
                                inputId = "treatment_input",
                                label = "Select treatment of interest",
                                choices = NULL,
                                multiple = TRUE
                              )
                            ),  
                            br(),
                            
                            div(class = "fixed-button",
                                actionButton(inputId = "confirm_selections",
                                             label = "confirm selections",
                                             width = "300px",
                                             style = "font-size: 125%; color: #fff; background-color: #2c3e50"))
                  )),
           
           column(9,
                  wellPanel(style = "background-color: #fff; border-color: #2c3e50; height: 1150px;",
                            add_busy_spinner(spin = 'pixel', position = 'bottom-right'),
                            column(12,
                                   uiOutput("dataset_table_header"),
                                   uiOutput("select_all_button"),
                                   br(),
                                   reactableOutput("main_table")))
           )
           
  ),
  
  
  ### RESULTS PANEL TAB ###
  tabPanel(title = "Results Panel",
           ### TABS FOR OUTPUT ###
           column(10, offset = 1,
                  tabsetPanel(
                    type = "tabs",
                    id = "main_body_tabset",
                    
                    ### GENE SUMMARY TAB ###        
                    tabPanel("DEG Selection",
                             value = "gene_summary_tab",
                             class = "results-tab",
                             #fluidRow(
                             column(3,
                                    sidebarPanel(
                                      uiOutput("tree_ui") ,
                                      width = 12
                                    )
                             ),
                             column(9,
                                    tabsetPanel(
                                      id= "summary_tabs",
                                      tabPanel("DEG Compare", value = "Gene_list_table",
                                               fluidRow(
                                                 column(9,div(id="gene_list_div",class="datatable-container",DTOutput('gene_names_output'))),
                                                 column(1,div(id="toggleContainer", actionButton("toggleTable","",icon = icon("angle-double-left"))))
                                               ),
                                               fluidRow(column(9,div(class="datatable-container",DTOutput("mytable")))),
                                               fluidRow(column(9,div(class="datatable-container",DTOutput("mytable2")))),
                                                 div(style="text-align: center;", actionButton("enrichmentButton","Enrichment Analysis"))
                                      ),
                                      tabPanel("Venn Diagram", value = "venn_diagram", card(
                                        layout_sidebar(
                                          sidebar = sidebar(
                                            width = 350,
                                            uiOutput("venn_ui"),
                                            uiOutput("vennLabels")
                                          ),
                                          imageOutput("Venn"),
                                          downloadButton('downloadImage', 'Download Venn Diagram')
                                        )
                                      )),
                                      tabPanel("Pathway Enrichment Analysis", 
                                               value = "pathway_enrichment_analysis",
                                               add_busy_spinner(spin = 'pixel', position = 'bottom-right'),
                                               uiOutput("gene_enrich"))
                                      #)
                                    )
                             )
                    ),
                    
                    # ),
                    
                    #)
                    #),
                    
                    ### COMPARISON TABLES TAB ### 
                    
                    tabPanel("Comparison Tables (Results)",
                             value = "Tables",
                             class = "results-tab",
                             add_busy_spinner(spin = 'pixel', position = 'bottom-right'),
                             wellPanel(style = "background-color: #fff; border-color: #2c3e50; padding: 25px;",
                                       uiOutput("summary_title"),
                                       uiOutput("query_summary"),
                                       br(),
                                       uiOutput("dataset_summary")),
                             wellPanel(
                               style = "background-color: #fff; border-color: #2c3e50; padding: 15px;",
                               uiOutput("comparison_table_title_and_options"),
                               br(), br(),
                               uiOutput("comparison_table_selection_options"),
                               br(), br(),
                               uiOutput("dynamic_tabs")
                             )
                    ),
                    
                    ## VISUALS TAB ##
                    tabPanel("Expression Visuals",
                             value ="Visualization",
                             class = "results-tab",
                             add_busy_spinner(spin = 'pixel', position = 'bottom-right'),
                             wellPanel(style = "background-color: #fff; border-color: #2c3e50; padding: 25px;",
                                       uiOutput("expression_plot_title"),
                                       br(),
                                       #uiOutput("plot_options"),
                                       #br(),
                                       uiOutput("plot_buttons"),
                                       br(),
                                       plotOutput("expression_plot_output")),
                             # wellPanel(style = "background-color: #fff; border-color: #2c3e50; padding: 25px;",
                             #           uiOutput("meta_analysis_header"),
                             #           br(),
                             #          reactableOutput("meta_analysis_table"))
                    ),
                    ### ENRICHMENT ANALYSIS TAB ###
                    tabPanel("Enrichment Analysis",
                             value = "enrichment-analysis",
                             add_busy_spinner(spin = 'pixel', position = 'bottom-right'),
                             #column(8, #offset = 2,
                             wellPanel(style = "background-color: #fff; border-color: #2c3e50;",
                                       h2("Enrichment Analysis",
                                          style = "text-align: center;"),
                                       br(),
                             ),
                             wellPanel(style = "background-color: #fff; border-color: #2c3e50;",
                                       uiOutput("enrichment_report"))
                             #)
                    )
                  )
           )
  ), selected = "Home",
  
  
  #tags$div(
  #  actionButton(inputId = "repErr",label="Report Error", icon = icon("exclamation-triangle"),
  #               style = "position: absolute; top: 5px; right: 500px; z-index:10000;")) #,
  
  #div(id= "hover-info", class= "hover-info")
  
)


# The active datasets table (defined globally)
active_datasets_table <- reactiveVal(NULL)

# Define the server functionality for the application
server <- function(input, output, session) {
  selections <- reactiveValues(selected=numeric(0))
  main_selections <- reactiveVal(NULL)
  child_selections <- reactiveVal(NULL)
  selected_nodes <- reactiveVal(NULL)
  selected_cell_types <- reactiveVal(NULL)
  initial_selections <- reactiveVal(NULL)
  
  #
  
  output[["tree1"]] <- renderJstree({
    jstree(nodes = nodes,
           # theme="default-dark",
           multiple = T,checkboxes  = T,search = T
    )
  })
  output[["checked1"]] <- renderPrint({
    input[["tree1_checked"]] |> Text()
  })
  
  observeEvent(input$start_button, {
    updateTabsetPanel(session, "navbar-tabset", selected = "Dataset Selection")
  })
  
  observeEvent(input$tree1_selected,{
    #
    jsonlite::toJSON(input[["tree1_checked"]], pretty = TRUE, auto_unbox = TRUE)
    my_dataframe <- data.frame(text = unlist(lapply(input[["tree1_checked"]], `[[`, "text")))
    selected_cell_types <-unique(as.character(my_dataframe$text))
    print(selected_cell_types)
    cellType_str <- paste0("LOWER(cell_type) LIKE LOWER('%", selected_cell_types, "%')", collapse = " OR ")
    query <- paste0("SELECT DISTINCT disease FROM dataset WHERE ",cellType_str)
    print(query)
    disease <- dbGetQuery(db,query)
    disease <- sort(unique(unlist(strsplit(as.character(disease$disease), ";"))))
    disease <- setdiff(disease, "Control")
    updateSelectInput(session, "disease_input", choices = disease)
  })
  
  observeEvent(input$disease_input, {
    #
    jsonlite::toJSON(input[["tree1_checked"]], pretty = TRUE, auto_unbox = TRUE)
    my_dataframe <- data.frame(text = unlist(lapply(input[["tree1_checked"]], `[[`, "text")))
    selected_cell_types <-unique(as.character(my_dataframe$text))
    print(selected_cell_types)
    cellType_str <- paste0("LOWER(cell_type) LIKE LOWER('%", selected_cell_types, "%')", collapse = " OR ")
    disease_str <- paste0("LOWER(disease) LIKE LOWER('%", input$disease_input, "%')", collapse = " OR ")
    query <- paste0("SELECT DISTINCT source FROM dataset WHERE ( ",cellType_str,") AND (", disease_str, ")")
    print(query)
    tissue <- dbGetQuery(db, query)
    tissue <- sort(unique(unlist(strsplit(as.character(tissue$source), ";"))))
    updateSelectInput(session, "tissue_input",choices = tissue)
  })
  
  observeEvent(input$tissue_input, {
    #
    jsonlite::toJSON(input[["tree1_checked"]], pretty = TRUE, auto_unbox = TRUE)
    my_dataframe <- data.frame(text = unlist(lapply(input[["tree1_checked"]], `[[`, "text")))
    selected_cell_types <-unique(as.character(my_dataframe$text))
    print(selected_cell_types)
    cellType_str <- paste0("LOWER(cell_type) LIKE LOWER('%", selected_cell_types, "%')", collapse = " OR ")
    disease_str <- paste0("LOWER(disease) LIKE LOWER('%", input$disease_input, "%')", collapse = " OR ")
    tissue_str <- paste0("LOWER(source) LIKE LOWER('%", input$tissue_input, "%')", collapse = " OR ")
    query <- paste0("SELECT DISTINCT treatment FROM dataset WHERE ( ", cellType_str, ") AND (", disease_str, ") AND (", tissue_str,")")
    print(query)
    treatment <- dbGetQuery(db, query)
    treatment <- sort(unique(unlist(strsplit(as.character(treatment$treatment), ";"))))
    treatment <- treatment[treatment != "NA"]
    updateSelectInput(session, "treatment_input", choices = treatment)
  })
  
  ### REACTIVE VALUES ###
  # Define a vector used for genes
  genes <- reactiveVal(NULL)
  # Define a vector used for pathways
  pathways <- reactiveVal(NULL)
  # Define a vector used for the exact diseases (necessary because some datasets are associated with multiple diseases)
  exact_diseases <- reactiveVal(NULL)
  # Generate UNSTYLED comparison tables
  comp_tables_unstyled_reactive <- reactiveVal(NULL)
  # Generate STYLED comparison tables
  comp_tables_styled_reactive <- reactiveVal(NULL)
  # Define the list used to store expression plots
  plots_reactive <- reactiveVal(NULL)
  comparison_checked <- reactiveVal(NULL)
  
  # Queries
  diseases <- reactiveVal(NULL)
  sources <- reactiveVal(NULL)
  cell_types <- reactiveVal(NULL)
  treatments <- reactiveVal(NULL)
  
  datasets <- reactiveVal(NULL)
  all_datasets <- reactiveVal(NULL)
  comparisons <- reactiveVal(NULL)
  all_comparisons <- reactiveVal(NULL)
  comparison_data <- reactiveVal(NULL)
  gene_table <- reactiveVal(NULL)
  merged_table <- reactiveVal(NULL)
  
  comparison_tables <- reactiveVal(NULL)
  selectedDetailsRows <- reactiveVal(data.frame())
  selected_comparisons <- reactiveValues()
  
  enrichment_gene_sets <- read.csv("enrichment_gene_sets.csv")
  gmt_filename_reactive <- reactiveVal(NULL)
  upper_limit <- reactiveVal(4)
  lower_limit <- reactiveVal(-4)
  comp_log_fc_threshold <- reactiveVal(1)
  comp_p_value_threshold <- reactiveVal(0.05)
 
  
  #==============================================================================================================
  ### Generate a table of datasets based on the user query when the user clicks the "Confirm Selections" button
  #==============================================================================================================
  
  observeEvent(input$confirm_selections, {
    #
    initial_selections  <- NULL
    selections$selected <- NULL
    if (!isTruthy(input$disease_input)) {    # !(isTruthy(input$tree1_selected)) | !(isTruthy(input$gene_list_input) | isTruthy(input$signature_list_input) | isTruthy(input$pathway_list_input)) | 
      shinyalert(#title = "Insufficient Input",
        text = "You need to specify at least one disease",
        type = "error",
        confirmButtonText = "OK",
        confirmButtonCol = '#2c3e50')
    } else {
      output$select_all_button <- renderUI({
        actionButton("select_all","Toggle Selection")
        
      })
      output$gene_input_selection <- renderUI({
        if (input$gene_option=="GENE-level"){
          textInput(inputId = "gene_list_input",
                    label = "Enter a comma-separated list of gene names (max 10 genes)")
        }
        else if (input$gene_option=="ALL-GENE-level") {
          div(style = "padding-bottom: 5px;",
              div(style = "margin-bottom: 10px;",     # display: flex; align-self: center; margin-right: 10px;)
                  radioGroupButtons(inputId = "all_pval_option",
                                    label = NULL,
                                    choices = c("p-value" = "p_value", "adjusted p-value" = "p_value_adj"),
                                    selected = "p_value_adj",
                                    checkIcon = list(
                                      yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
                                      no = tags$i(class = "fa fa-square-o", style = "color: steelblue")))),
              
              div(style= "display: flex; align-items: center; margin-top:10px;",
                  div(style = "flex: 1; min-width: 100px; padding-right: 10px;", #
                      numericInput(inputId = "all_pval_threshold",
                                   label = "Set a threshold for the p-value",
                                   value = 0.05,
                                   min = 0,
                                   max = 1,
                                   step = 0.01)),
                  div(style = "flex: 1; min-width: 100px; padding-right: 10px;", #display: inline-block; padding-left: 35px;
                      numericInput(inputId = "all_logfc_threshold",
                                   label = "Set a threshold for the logFC",
                                   value = 0.25,
                                   min = 0,
                                   max = 5,
                                   step = 0.1)),
                  div(style = "flex: 1; min-width: 100px;", #display: inline-block; padding-left: 35px;
                      radioGroupButtons(inputId = "all_logfc_option",
                                        label = "",
                                        choices = c("up", "down"),
                                        selected = "up",
                                        width = "250px",
                                        direction = "horizontal",
                                        checkIcon = list(
                                          yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
                                          no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))))
              ))
        } else if(input$gene_option=="CUSTOM-level") {
          div(style= "display: flex; justify-content: space-between; align-items: flex-start; width: 100%;",
              div(style = "width: 70%; margin-right: 5%", #padding-bottom: 5px;     # flex: 1
                  div(
                    style= "margin-bottom: 10px;",
                    strong("Please enter your gene list (comprising at least 10 genes) and select comparisons to perform Enrichment Analysis on the genes")
                  ),
                  textAreaInput(inputId = "custom_gene_list_input",
                                label = NULL,
                                width = '100%',
                                height = '375px')),
              div(style= "display: flex; flex-direction: column; width:100%;",
                  div(style = "display: flex; flex-direction: column; margin-bottom: 20px;", #display: flex; justify-content: space-between;flex: 1;"
                      h4("Geneset Threshold Selection"),
                      div(style = "margin-right: 10px; padding-bottom: 5px;",  #width: 45%; flex: 1; 
                          radioGroupButtons(inputId = "pval_option",
                                            label = NULL,
                                            choices = c("p-value" = "p_value", "adjusted p-value" = "p_value_adj"),
                                            selected = "p_value_adj",
                                            width = "300px",
                                            direction = "horizontal",
                                            checkIcon = list(
                                              yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
                                              no = tags$i(class = "fa fa-square-o", style = "color: steelblue")))
                      ),
                      
                      div(style= "display: flex; align-items: center; margin-top:10px;", #display: flex; width: 76%;
                          div(style = "flex: 1; min-width: 100px; padding-right: 10px;",   #display: inline-block;"  display: flex; align-self: center;
                              numericInput(inputId = "pval_threshold",
                                           label = "Set a threshold for the p-value",
                                           value = 0.05,
                                           min = 0,
                                           max = 1,
                                           step = 0.01,
                                           width = "100%")),
                          div(style= "flex: 1; min-width: 100px; padding-right: 10px;", # display: flex; align-self: center;
                              numericInput(inputId = "logfc_threshold",
                                           label = "Set a threshold for the logFC",
                                           value = 0.25,
                                           min = 0,
                                           max = 5,
                                           step = 0.1,
                                           width = "100%"),
                          ),
                          div(style = "flex: 1; min-width: 100px; padding-top: 50px;",     # display: flex; align-self: center; margin-right: 10px;
                              radioGroupButtons(inputId = "logfc_option",
                                                label = NULL,
                                                choices = c("up", "down"),
                                                selected = "up",
                                                width = "250px",
                                                direction = "horizontal",
                                                checkIcon = list(
                                                  yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
                                                  no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))))
                      )
                  ),
                  div(style = "flex: 1;",
                      #h4("Pathway Threshold Selection"),
                      radioGroupButtons(inputId = "pathway",
                                        label = "Enrichment Significance Level",
                                        choices = c("FDR", "TOP"),
                                        selected = "FDR",
                                        width = "250px",
                                        direction = "horizontal",
                                        checkIcon = list(
                                          yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
                                          no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))
                      ),
                      numericInput("num",label= NULL,value = 0.05, width = "28%")
                  )
              )
          )
        }
      })
      
      diseases <- unlist(input$disease_input)
      diseases(diseases)
      sources <- unlist(input$tissue_input)
      sources(sources)
      treatments <- unlist(input$treatment_input)
      treatments(treatments)
      
      jsonlite::toJSON(input[["tree1_checked"]], pretty = TRUE, auto_unbox = TRUE)
      my_dataframe <- data.frame(text = unlist(lapply(input[["tree1_checked"]], `[[`, "text")))
      my_dataframe$text<-tools::toTitleCase(my_dataframe$text)
      cell_types <-selected_cells <-unique(as.character(my_dataframe$text))
      print(paste0("Selected Cell:",selected_cells))
      
      cell_types(selected_cells)
      
      #
      # Pull the dataset descriptions
      datasets <- get_datasets(diseases, 
                               sources, 
                               treatments, 
                               cell_types,
                               db, 
                               FALSE)
      
      # Pull the comparison descriptions and comparison data
      comparisons <- get_comparisons(datasets,
                                     diseases,
                                     sources, 
                                     treatments,
                                     cell_types, 
                                     db)
      # Update the reactive values
      comparisons(comparisons)
      all_comparisons(comparisons)
      
      # Format the diseases column in the datasets table and filter for datasets in the comparisons table
      #cell_types_pattern <- paste(cell_types, collapse = "|")
      
      datasets <- datasets %>%
        filter(dataset_acc %in% comparisons$dataset_acc)
      
      # Update the reactive values
      datasets(datasets)
      all_datasets(datasets)
      
      # Display the guidelines for interacting with the dataset selection table and display the Run ImmunoCompare button
      output$dataset_table_header <- renderUI({
        div(
          div(style = "display: flex; justify-content: center;",
              actionButton(inputId = "run_immunocompare",
                           label = "Compare Datasets",
                           width = "300px",
                           style = "font-size: 125%; color: #fff; background-color: #2c3e50")),
          br(), br(),
          p("The table below allows you to explicitly select datasets that you would like to compare.",
            "By default, all datasets are selected. If you are only interested in a few datasets, you can click the",
            span(style = "font-weight: bold;", "deselect all"), "button and then make your selections. Click the", 
            span(style = "font-weight: bold;", "Compare Datasets"), "button to display results across selected data.",
            "Results will be displayed in the Results Panel."),
          br(),
          radioGroupButtons(
            inputId = "gene_option",
            label = NULL,
            size = "sm",
            direction = "horizontal",
            choices = c("geneQuery" = "GENE-level","DEGEnricher"="CUSTOM-level", "DEGCompare"="ALL-GENE-level"),
            selected = "GENE-level",
            checkIcon = list(yes = tags$i(class = "fa fa-check-square",
                                          style = "color: steelblue"),
                             no = tags$i(class = "fa fa-square-o",
                                         style = "color: steelblue"))),
          div(
            class = "section",
            uiOutput("gene_input_selection")))
      })
      
      #
      # Pull the dataset descriptions for the dataset selection table and update the reactive value
      dataset_table <- get_dataset_table_data(datasets, db)
      active_datasets_table(dataset_table)
      
      selected_columns <- datasets[,c(1,9)]
      temp_table <- merge(dataset_table,selected_columns, by= "dataset_acc")
      temp_table$raw_dataset_acc <- temp_table$dataset_acc
      temp_table$dataset_acc <- mapply(function(id, desc) {
        as.character(tags$a(href= "#",class = "show-description", `data-description` = desc, id))
      }, temp_table$dataset_acc, temp_table$summary
      )
      merged_table(temp_table)
      
      
      #lets create display
      output$main_table <- renderReactable({
        req(merged_table())
        reactable(merged_table()[,c("dataset_acc","title","disease","source","treatment")],
                  #details = function(index){
                  #  htmltools::div(style = "padding: 16px",
                  #                 reactableOutput(paste0("tbl-",index)))
                    #subtable <- comparisons[comparisons$dataset_acc == merged_table$dataset_acc[index],]
                    #reactable(subtable, selection = 'multiple', defaultSelected = seq_len(nrow(subtable)))
                  #},
                  selection = 'multiple',
                  defaultSelected = NULL,  #selections$selected[selections$selected <=nrow(merged_table)],
                  columns = list(
                    dataset_acc = colDef(html = TRUE),
                    .selection = colDef(
                      headerStyle = list(pointerEvents = "none")
                    )
                  ),
                  theme = reactableTheme(
                    headerStyle = list("& input[type='checkbox']" = list(display = "none"))
                  )
        )
      })
      
      
      observeEvent(input$confirm_selections, {
        new_selection <- sample(1:nrow(merged_table()),1)
        updateReactable("main_table", selected = new_selection)
        selections$selected <- new_selection
        print(paste0("new selected rows:", paste(new_selection, collapse = ", ")))
      })
      
      #max_child_rows <- max(sapply(seq_len(nrow(merged_table())), function(index){
      #  subtable <- comparisons[comparisons$dataset_acc == merged_table()$raw_dataset_acc[index],]
      #  nrow(subtable)
      #}))
      
      
      
      #lapply(seq_len(nrow(merged_table())), function(index){
      #  output[[paste0("tbl-",index)]] <- renderReactable({
      #    subtable <- comparisons[comparisons$dataset_acc == merged_table()$raw_dataset_acc[index],]
          #default_selected <- seq_len(nrow(subtable))
          #if(nrow(subtable) < max_child_rows) {
          #  default_selected <- c(default_selected,rep(NA,max_child_rows - nrow(subtable)))
          #}
          
      #    reactable(subtable,selection = 'multiple', #defaultSelected = default_selected[!is.na(default_selected)],
      #              onClick = "select",outlined = TRUE)
      #  })
      #  observe({
      #    tbl_id <- paste0("tbl-", index)
       #   selected_rows <- getReactableState(tbl_id,"selected")
      #    child_selections(paste0(index,selected_rows))
          #cat("Expanded table", index, " Selected rows:", selected_rows, "\n")
          
       # })
      #})
      
    }
  })
  
  observeEvent(getReactableState("main_table", "selected"), {
    selected_rows <- getReactableState("main_table", "selected")
    selections$selected <- selected_rows
    main_selections(as.numeric(gsub(pattern = " ",replacement = "", selections$selected)))
    #print(paste0("Printing selections:",selections$selected))
    #cat("Main table rows:",selections$selected, "\n")
  })  
  
  observeEvent(input$select_all,{
    #
    print(paste("Before select all:", length(selections$selected)))
    if (length(selections$selected) < nrow(merged_table())){
      #select all rows
      selections$selected <- seq_len(nrow(merged_table()))
      print("selecting all rows")
    } else{
      #deselect
      selections$selected <- numeric(0)
      print("Deselecting all rows")
    }
    print(paste("After select all:", length(selections$selected)))
    updateReactable("main_table", selected = selections$selected)
  })
  
  observeEvent(input$pathway,{
    new_value <- ifelse(input$pathway == "FDR", 0.05,10)
    updateNumericInput(session,"num",value = new_value)
  })
  
  
  observeEvent(input$logfc_option,{
    new_threshold <- ifelse(input$logfc_option == "up", 0.25,-0.25)
    updateNumericInput(session,"logfc_threshold",value = new_threshold)
  })
  
  observeEvent(input$logfc_threshold,{
    if(!is.null(input$logfc_threshold) && !is.na(input$logfc_threshold)){
      if(input$logfc_option == "down" && input$logfc_threshold > 0){
        updateNumericInput(session,"logfc_threshold",value = -input$logfc_threshold )
      }
    }
  })
  
  
  observeEvent(input$all_logfc_option,{
    new_threshold <- ifelse(input$all_logfc_option == "up", 0.25,-0.25)
    updateNumericInput(session,"all_logfc_threshold",value = new_threshold)
  })
  
  observeEvent(input$all_logfc_threshold,{
    if(!is.null(input$all_logfc_threshold) && !is.na(input$all_logfc_threshold)){
      if(input$all_logfc_option == "down" && input$all_logfc_threshold > 0){
        updateNumericInput(session,"all_logfc_threshold",value = -input$all_logfc_threshold )
      }
    }
  })
  
  observeEvent(input$tree1_selected,{
    
    #clear the table when user is making new selection
    updateReactable("main_table", data = data.frame())
    
    #Reset selected state
    selections$selected <- NULL
    initial_selections(NULL)
  })
  
  
  
  #==============================================================================================================
  ### Run ImmunoCompare and produce output when the user clicks the "Run ImmunoCompare" button
  #==============================================================================================================
  observeEvent(input$run_immunocompare, {
    #
    output$tree_ui <- renderUI({print("Updating tree_ui")
      NULL})
    # Check if main_selection is empty
    if(is.null(main_selections()) || length(main_selections()) == 0) {
      # Show error message if no selection is made
      shinyalert(
        #title = "Select datasets",
        text = "Please select at least one dataset before clicking ",
        type =  "error"
      )
    } else {
      # Pull selected dataset IDs from data selection
      datasets <- all_datasets()[main_selections(),]
      print(datasets$dataset_acc)
      
      comparisons <- filter(all_comparisons(), dataset_acc %in% datasets$dataset_acc)
      comparisons(comparisons)
      checked_comparisons <- comparisons$id
      cat("checked comparisons: ", checked_comparisons)
      
      # Generate a vector of exact diseases (necessary because some datasets are associated with multiple diseases)
      exact_diseases <- get_exact_diseases(datasets)
      # Update the reactive value
      exact_diseases(exact_diseases)
      cat("Exact Diseases:", exact_diseases)
      
      #update the tabs
      updateNavbarPage(session, "navbar-tabset", selected = "Results Panel")
      hideTab("main_body_tabset", "gene_summary_tab")
      hideTab("main_body_tabset","Tables")
      hideTab("main_body_tabset","Visualization")
      hideTab("main_body_tabset","enrichment-analysis")
      
      if (input$gene_option == "GENE-level") {
        
        # Convert the user specified genes to a list and convert all genes to uppercase
        genes <- toupper(str_trim(unlist(str_split(input$gene_list_input, ","))))
        # Pull the comparison data and update the reactive value
        comparison_data <- get_comparison_data(comparisons, genes, db)
        comparison_data(comparison_data)
        
      } else if (input$gene_option == "ALL-GENE-level") {
        #
        gene_lists <- get_gene_list(comparisons, input$all_pval_option, input$all_logfc_option, input$all_logfc_threshold, input$all_pval_threshold, db)
        gene_sources <- get_gene_source(comparisons, input$all_pval_option, input$all_logfc_option, input$all_logfc_threshold, input$all_pval_threshold, db)
        print(paste("Printing gene source:", gene_sources$source))
        
        #update the tabs
        #update the tabs
        showTab("main_body_tabset", "gene_summary_tab")
        updateTabsetPanel(session, "main_body_tabset",selected= "gene_summary_tab")
      
        output$tree_ui <- renderUI({
          treeInput(
            inputId = "data_tree",
            label = "Select lists:",
            choices = create_tree(gene_sources),
            selected =  gene_sources$gene_list[1:7], #selected_lists,
            returnValue = "text",
            closeDepth = 0
          )
        })
        
        #
        print(paste("Printing input data_tree:", input$data_tree))
        
        observeEvent(input$data_tree,{
          #
          input_groups <- input$data_tree
          cat("Printing input_groups:\n", input_groups)
          input_groups <- input_groups[which(input_groups %in% gene_sources$gene_list)]
          if (length(input_groups)>7){
            showNotification("You can select upto 7 lists only",type="warning",duration=5)
            gene_lists_subset <- gene_lists[which(names(gene_lists) %in% input_groups)][1:7]
            print("Printing gene list subset:", gene_lists_subset)
            updateTreeInput(
              inputId ="data_tree",
              selected = names(gene_lists_subset),
            )
          } else {
            gene_lists_subset <- gene_lists[which(names(gene_lists) %in% input_groups)]
            #print("Printing gene list subset:", gene_lists_subset)
          }
          
          all_genes <- unique(unlist(gene_lists_subset))
          
          # Initialize a data frame with unique genes as rows
          gene_table <- data.frame(Gene = all_genes, stringsAsFactors = FALSE)
          print("Printing gene table content:\n")
          print(head(gene_table))
          
          # Function to check presence of genes in a list
          check_presence <- function(gene_list, gene_names) {
            return(ifelse(gene_names %in% gene_list, "yes", "no"))
          }
          
          # Add columns to the data frame for each list
          #
          for (list_name in names(gene_lists_subset)) {
            gene_table[[list_name]] <- check_presence(gene_lists_subset[[list_name]], gene_table$Gene)
          }
          
          gene_table(gene_table)
          print("column names of gene_table:\n")
          print(colnames(gene_table))
          
          # Create checkboxes with labels dynamically
          #
          checkbox_row <- data.frame(Option = c("Include", "Exclude"), stringsAsFactors = FALSE)
          for (list_name in names(gene_lists_subset)) {
            print(paste0("list name:", list_name))
            
            checkbox_row[[list_name]] <- c(
              sprintf('<input type="checkbox" name="include" value="%s" id="include_%s"/><label for="include_%s"></label>', list_name, list_name, list_name),
              sprintf('<input type="checkbox" name="exclude" value="%s" id="exclude_%s"/><label for="exclude_%s"></label>', list_name, list_name, list_name)
            )}
          
          colnames(checkbox_row) <- c("Options",sapply(1:(length(checkbox_row)-1),function(i){
            paste0("list",i)
          }))
          
          # Render the initial data table
          max_gene_length <<- 80 #max(nchar(gene_table$Gene))*20
          
          print(paste0("Printing max gene length", max_gene_length))
          
          # Render the initial data table
          output$mytable <- renderDT({
            datatable(
              checkbox_row,
              escape = FALSE,
              selection = 'none',
              rownames = FALSE,
              class = "custom-datatable",
              options = list(headerCallback = JS('headerCallback'),
                             dom = 't',paging=T, ordering = FALSE,
                             scrollX=TRUE,
                             columnDefs=list(
                               list(width=paste0(max_gene_length,"px"),targets=0)
                             ),
                             autowidth = TRUE)
            ) %>%
              formatStyle(c(1), `text-align` = 'left') %>%
              formatStyle(columns = c(1), width="80px") %>%
              formatStyle(c(2:ncol(checkbox_row)), `text-align`= 'center') %>%
              formatStyle(columns=c(2:ncol(checkbox_row)),width="20px")
          })
          
          observeEvent(input$data_tree,{
            #
            function_output <- create_data_table(gene_table,max_gene_length)
            gene_table_new <- function_output$output_table
            list_names <- function_output$list_names
            GeneLists <- colnames(gene_table_new$x$data)[2:ncol(gene_table_new$x$data)]
            gene_list_names <- data.frame(GeneLists=GeneLists,Name=list_names)
            output$gene_names_output <- renderDT({datatable(gene_list_names,
                                                            escape = FALSE,
                                                            selection = 'none',
                                                            rownames = FALSE,
                                                            class = "custom-datatable",
                                                            options = list(headerCallback = JS('headerCallback'),
                                                                           dom = 't',paging=T, ordering = FALSE,
                                                                           scrollX=TRUE)
            ) %>%
                formatStyle(columns = c(1), `text-align`='left') })
            output$mytable2 <- renderDT(server=FALSE,{gene_table_new %>%
                formatStyle(columns = c(1), `text-align`='left')
            })
            enrichment_genes <- gene_table_new$Gene
          })
          
        })
        
        
        # State to keep track of selected columns
        selected_columns <- reactiveValues(include = character(), exclude = character())
        
        observeEvent(input$checkbox_values, {
          #
          # Get the include and exclude lists
          print(selected_columns$include)
          include <- input$checkbox_values$include
          exclude <- input$checkbox_values$exclude
          
          # Update the reactive values for include columns
          selected_columns$include <- unique(c(include, setdiff(include, exclude)))
          selected_columns$include <- setdiff(include, exclude)
          
          # Update the reactive values for exclude columns
          selected_columns$exclude <- unique(c(exclude, setdiff(exclude, include)))
          selected_columns$exclude <- setdiff(exclude, include)
          
          # Debugging: print the selected columns to ensure they're being updated correctly
          
          # Filter genes based on the include columns
          if (length(selected_columns$include) > 0) {
            print("selected cols from gene table:")
            #print(selected_columns$include)
            print(colnames(gene_table))
            # Ensure genes are present in all selected include lists
            include_genes <- Reduce(intersect, lapply(selected_columns$include, function(col) gene_table()$Gene[gene_table()[[col]] == "yes"]))
          } else {
            include_genes <- gene_table()$Gene
          }
          
          # Filter genes based on the exclude columns
          if (length(selected_columns$exclude) > 0) {
            # Ensure genes are excluded from all selected exclude lists
            exclude_genes <- Reduce(union, lapply(selected_columns$exclude, function(col) gene_table()$Gene[gene_table()[[col]] == "yes"]))
          } else {
            exclude_genes <- c()
          }
          
          # Apply both include and exclude filters
          filtered_genes <- setdiff(include_genes, exclude_genes)
          filtered_gene_table <- gene_table()[gene_table()$Gene %in% filtered_genes, ]
          updated_table <- filtered_gene_table
          
          # Update the data table with the filtered genes
          function_output_subset <- create_data_table(updated_table,max_gene_length)
          gene_table_subset <- function_output_subset$output_table
          list_names <- function_output_subset$list_names
          GeneLists <- colnames(gene_table_subset$x$data)[2:ncol(gene_table_subset$x$data)]
          gene_list_names <- data.frame(GeneLists=GeneLists,Name=list_names)
          output$mytable2 <- renderDT(server=FALSE,{gene_table_subset %>%
              formatStyle(columns = c(1), `text-align`='left')})
          enrichment_genes <- gene_table_subset$Gene
          
        })
        
        observeEvent(input$enrichmentButton,{
          #nav_select("tabs",selected= "Enrichment Analysis")
          updateTabsetPanel(session, "summary_tabs",selected= "pathway_enrichment_analysis")
          showNotification("Running Enrichment Analysis. You will be redirected to Enrichment Analysis Tab once done.",duration=5)
          
        })
        
        observeEvent(input$enrichmentButton,{
          #
          folder_name <- perform_ora("pathway_Reactome",NULL,gene_table()$Gene,"FDR",0.05)
          #html_file <- list.files(paste0("./enrichment_results/Project_",folder_name),pattern = ".html")
          #b64 <- base64enc::dataURI(file = paste0("./enrichment_results/Project_",folder_name,"/",html_file), mime = "text/html")
          file.copy(paste0("./enrichment_results/Project_", folder_name), "./www/", recursive = TRUE)
          
          output$gene_enrich<-renderUI({
            htmltools::tags$iframe(seamless = "seamless",
                                   src=  paste0("Project_", folder_name, "/Report_", folder_name, ".html"),
                                   #src= b64,
                                   #height=600,
                                   width="100%",
                                   style ="height: 100vh;")
          })
          
        })
        
        observeEvent(input$data_tree,{
          #
          selected_groups <- input$data_tree
          selected_groups <- intersect(selected_groups, gene_sources$gene_list)
          print("Selected Groups from data tree:")
          print(selected_groups)
          #filtered_data <- gene_sources %>% filter(gene_lists %in% selected_groups)
          output$venn_ui <- renderUI({
            pickerInput(
              inputId = "vennChoices", 
              label = "Select groups for venn diagram",
              choices = selected_groups,
              selected = selected_groups[1:4],
              options = pickerOptions(
                actionsBox = TRUE,
                size = 10,
                selectedTextFormat = "count > 3"
              ),
              multiple = TRUE)
          })
          # updatePickerInput(session = session, inputId = "vennChoices",
          #                    choices = selected_groups,
          #                    selected = selected_groups[1:4])
        })
        
        
        
        observeEvent(input$vennChoices,{
          output$vennLabels <- renderUI({
            selected_vennChoices <- input$vennChoices
            print("Selected Venn Choices from dropdown:")
            print(selected_vennChoices)
            
            if(length(selected_vennChoices)==0){
              return(NULL)
            }
            
            list_labels <- c("L1","L2","L3","L4")
            cat("Printing list_labels:\n", list_labels)
            
            label_inputs <- lapply(seq_along(selected_vennChoices),function(i){
              textInput(inputId = paste0("label_",i),
                        label = paste("Label for", selected_vennChoices[i]),
                        value = list_labels[i]) 
            })
            do.call(tagList,label_inputs)
          })
          
          
          
          #
          output$Venn <- renderPlot({
            #
            selected_vennChoices <- input$vennChoices
            if (length(selected_vennChoices)>=4){
              showNotification("You can select upto 4 lists only",type="warning",duration=5)
              venn_lists <- gene_lists[selected_vennChoices][1:4]
            }else{
              venn_lists <- gene_lists[selected_vennChoices]
            }
            
            custom_labels <- sapply(1:length(venn_lists),function(i){
              input[[paste0("label_",i)]] %||% paste0("L",i)
            })
            
            myCol <- brewer.pal(length(venn_lists), "Accent")[c(1:length(venn_lists))]
            
            cat("myCol:\n",myCol)
            cat("custom_labels:\n", custom_labels)
            
            display_venn <- function(x, labels,...){
              library(VennDiagram)
              grid.newpage()
              futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
              venn_object <- venn.diagram(x, category.names = labels, cat.cex=1.5,cex=1.5,filename = NULL,  ...)
              legend_labels <- mapply(function(n, l) paste(n,l,sep=":"),names(x),labels)
              #print(legend_labels)
              lg <- legendGrob(labels=legend_labels, pch=rep(19,length(c(labels))),
                               gp=gpar(col=myCol, fill="gray"),
                               byrow=TRUE)
              g <- gTree(children = gList(venn_object))
              gridExtra::grid.arrange(g, lg, nrow = 2, heights = c(3,1))
              
              #grid.draw(venn_object)
            }  
            
            display_venn(venn_lists, labels=custom_labels,fill=myCol)
            
          })
          
        })
        
        
        
        output$downloadImage <- downloadHandler(
          filename = function(){
            paste("plots/venn_diagram",Sys.Date(),".png",sep="")
          },
          content = function(file) {
            selected_vennChoices <- input$vennChoices
            if (length(selected_vennChoices)>=4){
              venn_lists <- gene_lists[selected_vennChoices][1:4]
            }else{
              venn_lists <- gene_lists[selected_vennChoices]
            }
            
            custom_labels <- sapply(seq_along(venn_lists), function(i){
              input[[paste0("label_",i)]] %||% paste0("L",i)
            })
            
            
            myCol <- brewer.pal(length(venn_lists), "Accent")[length(venn_lists)]
            
            display_venn2 <- function(x,labels,filename, ...){
              library(VennDiagram)
              futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
              venn_object <- venn.diagram(x,category.names = labels,cat.cex=1.5,cex=1.5,filename = NULL, ...)
              legend_labels <- mapply(function(n, l) paste(n,l,sep=":"),names(x),labels)
              lg <- legendGrob(labels=legend_labels, pch=rep(19,length(c(labels))),
                               gp=gpar(col=myCol, fill="gray"),
                               byrow=TRUE)
              g <- gTree(children = gList(venn_object))
              png(filename=filename)
              gridExtra::grid.arrange(g, lg, nrow = 2, heights = c(3,1))
              dev.off()
            }  
            display_venn2(venn_lists,filename = file, labels=custom_labels, fill=myCol)
          })  
        
        observeEvent(input$toggleTable, {
          toggle("gene_list_div")
        })
        
      } else if (input$gene_option == "CUSTOM-level") {
        
        # Convert the user specified genes to a list and convert all genes to uppercase
        genes <- toupper(str_trim(unlist(str_split(input$custom_gene_list_input, "\n"))))
        comparison_data <- get_enrichment_comparison_data(checked_comparisons, genes, db)
        comparison_data(comparison_data)
      }
      
      
      #==============================================================================================================
      ### RESULTS (COMPARISON TABLES/ Enrichment Results)
      #==============================================================================================================
      
      # Generate the comparison tables
      if (input$gene_option == "GENE-level") {
        #
        if(nrow(comparison_data)==0){
          showModal(
            modalDialog(
              h4("Selected datasets do not contain these specific genes. Please select a different set of genes for your analysis."),
              easyClose = TRUE,
              footer = actionButton("ok", "okay")
            )
          )
        } else {
          
          #display summary tab
          # Generate the title of the summary section
          output$summary_title <- renderUI({
            h2("Summary of Query and Matching Datasets", align = "center")
          })
          
          # Summarize the query
          output$query_summary <- renderUI({
            div(
              p("The following output contains comparisons that meet the user specified critieria displayed below."),
              tags$ul(
                tags$li(paste0("genes - ", paste0(genes, collapse = ", "))),
                tags$li(paste0("diseases - ", paste0(diseases(), collapse = ", "))),
                tags$li(paste0("sources - ", paste0(c(sources(), cell_types()), collapse = ", "))),
                tags$li(paste0("treatments - ", paste0(treatments(), collapse = ", "))),
              ),
              p("The following table shows the number of datasets that match above criteria and contains the queried gene.")
            )
          })
          
          summary_df <- data.frame(
            Gene = genes,
            stringsAsFactors = FALSE
          )
          
          for(current_disease in diseases()) {
            results_table <- generate_dataset_summary_table(genes, current_disease, 
                                                            datasets, comparisons, 
                                                            comparison_data)
            
            #check if results_table is empty
            if(nrow(results_table) == 0 || !any(genes %in% colnames(results_table))) {
              warning(paste("No data for disease:", current_disease))
              next
            }
            
            # Calculate the total number of datasets found for each gene
            present_counts <- sapply(results_table[,genes, drop = FALSE], function(x) sum(x != "NA", na.rm = TRUE))
            
            summary_df[[current_disease]] <- present_counts
          }
          
          #
          summary_df <- style_dataset_summary_table(summary_df)
          
          output$dataset_summary <- renderText({
            summary_df
          })
          
          #
          table_list <- generate_comparison_tables(genes, diseases(), exact_diseases, 
                                                   datasets, comparisons, 
                                                   comparison_data, "p_value_adj", 0.05,1, FALSE)
          
          # Update the REACTIVE VALUE for the comparison tables
          comparison_tables(table_list)
          
          # Style the comparison tables
          #
          table_list <- style_comparison_tables(diseases(), table_list)
          
          #update the tab
          showTab("main_body_tabset", "Tables")
          showTab("main_body_tabset", "Visualization")
          updateTabsetPanel(session, "main_body_tabset", selected = "Tables")
          
          #dynamically create tabs
          
          output$dynamic_tabs <- renderUI({
            #disease_with_results <- results_and_disease()$disease
            #if(length(disease_with_results) ==0){
            # return(NULL)
            #}
            tablist <- lapply(diseases(), function(d){
              if(!is.null(table_list[[d]])){
                tabPanel(
                  title = d,
                  fluidRow(
                    column(12,
                           uiOutput(d)
                    )
                  )
                )
              }
            })
            do.call(tabsetPanel, tablist)
          })
          
          # Generate report for each tab
          for(m in diseases()){
            local({
              #
              output_name <- paste0(m)
              disease_name <- m
              output[[output_name]] <- renderText({
                result_tables <- table_list[[disease_name]]
              })
            })
          }
        }
        
      } else if (input$gene_option == "CUSTOM-level"){
        #
        gmt_filename <- create_gmt(comparison_data, input$pval_option, input$pval_threshold ,input$logfc_option, input$logfc_threshold)
        #gmt_filename <- create_gmt(comparison_data, input$pval_option, 0.5 ,input$logfc_option, 1)  #input$p_value_threshold, input$log_fc_threshold, 
        gmt_filename_reactive(gmt_filename)
        showNotification("GMT file created", type = "default")
        
        # Generate the enrichment results and return the directory with the html report
        project_name <- perform_ora("others",gmt_filename_reactive(),genes, input$pathway, input$num)
        
        if (is.null(project_name)) {
          # Display a message stating no genes met the thresholds
          showModal(
            modalDialog(
              h4("No genes meet the specified cutoffs. Please make adjustments if you would still like to run ORA."),
              easyClose = TRUE,
              footer = actionButton("okay", "okay")
            )
          )
        } else {
          # update the tab
          showTab("main_body_tabset","enrichment-analysis")
          updateTabsetPanel(session, "main_body_tabset", selected = "enrichment-analysis")
          
          # Modify the html report so that it is ready for viewing
          # modify_enrichment_report(project_name)
          # Move the associated files to the www directory so they can be accessed
          file.copy(paste0("./enrichment_results/Project_", project_name), "./www/", recursive = TRUE)
          
          # Display the html report
          output$enrichment_report <- renderUI({
            tags$iframe(seamless = "seamless",
                        src = paste0("Project_", project_name, "/Report_", project_name, ".html"),
                        width = "100%",
                        style ="height: 100vh;")
          })
        }
        
      }
      
      observeEvent(input$ok,{
        removeModal()
        updateTabsetPanel(session, "navbar-tabset", selected = "Dataset Selection")
      })
      
      observeEvent(input$okay,{
        removeModal()
        updateTabsetPanel(session, "navbar-tabset", selected = "Dataset Selection")
      })
      
      
      #==============================================================================================================
      ### COMPARISON TABLE BUTTONS
      #==============================================================================================================
      
      # Display options used for modifying the comparison tables
      if (input$gene_option == "GENE-level") {
        output$comparison_table_title_and_options <- renderUI({
          div(style = "display: flex; justify-content: space-between; align-items: center;",
              div(
                div(style = "display: inline-block; vertical-align: middle;",
                    h4("Table Options")),
                div(style = "display: inline-block; vertical-align: middle;",
                    dropdownButton(h4("table options"),
                                   radioGroupButtons(inputId = "comp_pval_option",
                                                     label = NULL,
                                                     choices = c("p-value" = "p_value", "adjusted p-value" = "p_value_adj"),
                                                     selected = "p_value_adj",
                                                     checkIcon = list(
                                                       yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
                                                       no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))),
                                   numericInput(inputId = "comp_pval_threshold",
                                                label = "Set a threshold for the p-value",
                                                value = 0.05,
                                                min = 0,
                                                max = 1,
                                                step = 0.01),
                                   numericInput(inputId = "comp_logfc_threshold",
                                                label = HTML(paste0("Set a threshold for the logFC")),
                                                value = 1,
                                                min = 0,
                                                step = 0.1),
                                   materialSwitch(inputId = "comp_remove_ns",
                                                  label = "Remove NS comparisons",
                                                  value = FALSE,
                                                  status = "primary"),
                                   circle = TRUE, 
                                   status = "primary",
                                   size = "sm",
                                   icon = icon("cog"), 
                                   width = "400px",
                                   right = FALSE))
              ),
              h2("Results", style = "margin: 0; margin-left: -10px;"),
              p("")
          )
        })
        
        # Display options used for filtering the comparison tables and generating expression box plots
        output$comparison_table_selection_options <- renderUI({
          div(style = "display: flex; justify-content: space-between;",
              div(
                actionButton("visuals_button", 
                             label = "Visualize Comparisons",
                             style = "color: black; background-color: #EDEDED; border-color: #36454f;")
              )
          )
        })
      } 
    }
  
  })
  
  
  #==============================================================================================================
  ### ObserveEvents for modifying comparison tables (p-value threshold, logFC threshold, remove NS)
  #==============================================================================================================
  
  #### P-VALUE OPTION, P-VALUE THRESHOLD, LOGFC THRESHOLD, ES OPTION, ES THRESHOLD, REMOVE NS COMPARISONS ####
  observeEvent(c(input$comp_pval_option, 
                 input$comp_pval_threshold, 
                 input$comp_logfc_threshold,
                 input$comp_remove_ns), {
                   #
                   print(paste(input$comp_pval_option,
                               input$comp_pval_threshold, input$comp_logfc_threshold,
                               input$comp_remove_ns))
                   
                   if(!is.null(input$comp_pval_threshold) && !is.na(input$comp_pval_threshold) && input$comp_pval_threshold != ""){
                     comp_p_value_threshold(input$comp_pval_threshold)
                   }
                   
                   if(!is.null(input$comp_logfc_threshold) && !is.na(input$comp_logfc_threshold) && input$comp_logfc_threshold != ""){
                     comp_log_fc_threshold(input$comp_logfc_threshold)
                   }
                   
                   # Generate the comparison tables
                   if (input$gene_option == "GENE-level") {
                     genes <- toupper(str_trim(unlist(str_split(input$gene_list_input, ','))))
                     table_list <- isolate(
                       generate_comparison_tables(genes, diseases(), exact_diseases(),
                                                  datasets(), comparisons(),
                                                  comparison_data(), input$comp_pval_option,
                                                  comp_p_value_threshold(), comp_log_fc_threshold(),
                                                  input$comp_remove_ns)
                     )
                     
                     # Update the REACTIVE VALUE for the comparison tables
                     comparison_tables(table_list)
                     
                     # Style the comparison tables
                     table_list <- style_comparison_tables(diseases(), table_list)
                     
                     #update the tab
                     showTab("main_body_tabset","Tables")
                     updateTabsetPanel(session, "main_body_tabset", selected = "Tables")
                     
                     # Generate report for each tab
                     
                     for(c in diseases()){
                       local({
                         #
                         output_name <- paste0(c)
                         disease_name <- c
                         print(disease_name)
                         output[[output_name]] <- renderText({
                           result_tables <- table_list[[disease_name]]
                           #cat("Debug:",c,"Results:", result_tables, "\n")
                           #paste(result_tables, collapse = " ")
                         })
                       })
                     }
                   } 
                 })
  
  
  ######
  
  
  
  #==============================================================================================================
  ### Generate boxplots given selected comparisons when the "Generate Visuals" button is pressed
  #==============================================================================================================
  
  observeEvent(input$visuals_button, {
    #
    
    # Display the button used to download the plots
    output$plot_buttons <- renderUI({
      div(class = "input-row",
          div(class = "left-items",
            radioGroupButtons("plot_type", 
                              label = "Select plot type:",
                              choices = c("logFC"= "log_fc","signed(-log10(p-value))"="p_val"),
                              direction = "horizontal",
                              selected = "log_fc",
                              checkIcon = list(
                                yes = tags$i(class = "fa fa-check-square", style = "color: steelblue"),
                                no = tags$i(class = "fa fa-square-o", style = "color: steelblue"))),
            numericInput(inputId = "plot_ceiling",
                         label = "Select heatmap ceiling/floor:",
                         value = 4,
                         min = 1)),
          div(class = "download-button-container",
              downloadButton('download_plots',
                             label = 'Download Plots',
                             style = 'color: black; background-color: #ededed; border-color: #26de5a;')
              )
            
          )
      #)
    })
    
    genes <- toupper(str_trim(unlist(str_split(input$gene_list_input, ','))))
    
    # Determine the ID's associated with the plot selection checkboxes
    checkbox_ids <- grep("_[0-9]+$", names(input), value = TRUE)
    # Determine which checkboxes are checked
    checked_values <- sapply(checkbox_ids, function(x)input[[x]])
    # Get unique comparison ids
    comparison_ids <- unique(sub("^NA_[^_]+_", "", checkbox_ids))
    # Consolidate selections 
    selected_comparison_ids <- sapply(comparison_ids, function(id) {
      related_ids <- grep(paste0("_", id, "$"), checkbox_ids, value = TRUE)
      any(checked_values[related_ids])
    })
    checked_comparisons <- comparison_ids[selected_comparison_ids]
      
    # Generate the title for the expression boxplot box
    output$expression_plot_title <- renderUI({
      h4("Heatmap for Selected Groups", align = "center")
    })
    
    # If no comparisons were selected, display a message in the Expression Visuals tab
    # Tell the user to select at least one comparison from the Comparison Tables tab
    if(length(checked_comparisons) == 0) {
      showModal(
        modalDialog(
          h4("No comparisons were selected. Select at least one comparison from the Plot Selection column in the Comparison Tables tab."),
          easyClose = TRUE,
          footer = NULL
        )
      )
      plots_reactive(NULL)
    } else {
      
      #
      removeModal()
      #update the tab
      updateTabsetPanel(session, "main_body_tabset", selected = "Visualization")
      
      heat_plot <- plot_heatmap(genes,checked_comparisons, input$comp_pval_option, "log_fc", db, upper_limit(), lower_limit())
      
      output$expression_plot_output <- renderPlot({
        heat_plot
      })
      
      output$download_plots <- downloadHandler(
        # Filename: specify the filename of the zipped plots
        filename =  function() {
          paste0(input$plot_type,"_heatmap_",Sys.time(),".png")
        },
        # Content: save the plots as a tar file
        content = function(file) {
          ggsave(file,
                 plot = heat_plot, #plots_reactive(),
                 width = 8,
                 height = 6,
                 units = "in",
                 dpi = 300)
        } 
      )
    }
  })
  
  observeEvent(input$plot_type, {
     genes <- toupper(str_trim(unlist(str_split(input$gene_list_input, ','))))
    
    # Determine the ID's associated with the plot selection checkboxes
    checkbox_ids <- grep("_[0-9]+$", names(input), value = TRUE)
    # Determine which checkboxes are checked
    checked_values <- sapply(checkbox_ids, function(x)input[[x]])
    # Get unique comparison ids
    comparison_ids <- unique(sub("^NA_[^_]+_", "", checkbox_ids))
    # Consolidate selections 
    selected_comparison_ids <- sapply(comparison_ids, function(id) {
      related_ids <- grep(paste0("_", id, "$"), checkbox_ids, value = TRUE)
      any(checked_values[related_ids])
    })
    checked_comparisons <- comparison_ids[selected_comparison_ids]
      
    heat_plot <- plot_heatmap(genes,checked_comparisons, input$comp_pval_option, input$plot_type, db, upper_limit(), lower_limit())
    
    # Generate the title for the expression boxplot box
    output$expression_plot_title <- renderUI({
      h4("Heatmap for Selected Groups", align = "center")
    })
    
    # If no comparisons were selected, display a message in the Expression Visuals tab
    if (length(checked_comparisons) == 0) {
      output$expression_plot_output <- renderUI({
        h4("No comparisons were selected. Select at least one comparison from the Plot Selection column",
           "in the Comparison Tables tab.")
      })
      plots_reactive(NULL)
    } else {
      
      #update the tab
      updateTabsetPanel(session, "main_body_tabset", selected = "Visualization")
      
      output$expression_plot_output <- renderPlot({
        heat_plot
      })
    }
  })
  
  observeEvent(input$plot_ceiling, {
    
    genes <- toupper(str_trim(unlist(str_split(input$gene_list_input, ','))))
    # Determine the ID's associated with the plot selection checkboxes
    checkbox_ids <- grep("_[0-9]+$", names(input), value = TRUE)
    # Determine which checkboxes are checked
    checked_values <- sapply(checkbox_ids, function(x)input[[x]])
    # Get unique comparison ids
    comparison_ids <- unique(sub("^NA_[^_]+_", "", checkbox_ids))
    # Consolidate selections 
    selected_comparison_ids <- sapply(comparison_ids, function(id) {
      related_ids <- grep(paste0("_", id, "$"), checkbox_ids, value = TRUE)
      any(checked_values[related_ids])
    })
    checked_comparisons <- comparison_ids[selected_comparison_ids]
    
    if(!is.null(input$plot_ceiling) && !is.na(input$plot_ceiling) && input$plot_ceiling != ""){
      #
      upper_limit(abs(input$plot_ceiling))
      lower_limit(-abs(input$plot_ceiling))
    }
    
    heat_plot <- plot_heatmap(genes,checked_comparisons, input$comp_pval_option, input$plot_type, db, upper_limit(), lower_limit())
    
    # Generate the title for the expression boxplot box
    output$expression_plot_title <- renderUI({
      h4("Heatmap for Selected Groups", align = "center")
    })
    
    # If no comparisons were selected, display a message in the Expression Visuals tab
    if (length(checked_comparisons) == 0) {
      output$expression_plot_output <- renderUI({
        h4("No comparisons were selected. Select at least one comparison from the Plot Selection column",
           "in the Comparison Tables tab.")
      })
      plots_reactive(NULL)
    } else {
      
      #update the tab
      updateTabsetPanel(session, "main_body_tabset", selected = "Visualization")
      
      output$expression_plot_output <- renderPlot({
        heat_plot
      })
    }
  })
  
  
  observeEvent(input$gene_option,{
    hideTab( "main_body_tabset","gene_summary_tab")
    hideTab("main_body_tabset","Tables")
    hideTab("main_body_tabset","Visualization")
    hideTab("main_body_tabset","enrichment-analysis")
    if(input$gene_option == "GENE-level"){
      showTab("main_body_tabset","Tables")
      showTab("main_body_tabset","Visualization")
    } else if(input$gene_option == "ALL-GENE-level"){
      showTab("main_body_tabset", "gene_summary_tab")
      updateTabsetPanel(session, "main_body_tabset",selected= "gene_summary_tab")
    }
  })
  
} 

# Close the connection to the DB
onStop(function() {
  dbDisconnect(db)
})

# Run the application 
shinyApp(ui = ui, server = server)