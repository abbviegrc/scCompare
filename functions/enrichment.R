
#==============================================================================================================
# Function to create gmt file that will be used for enrichment analysis
# Returns .gmt file in the enrichment folder
#==============================================================================================================

create_gmt <- function(comparison_data,pval_option, p_value_threshold, logfc_option, log_fc_threshold){
  #Filter the comparison data that meets user selected criteria
  if (logfc_option == "up") {
    filtered_comparison_data <- comparison_data %>% 
      filter(log_fc >= log_fc_threshold,
             comparison_data[[pval_option]] <= p_value_threshold)
  } else {
    filtered_comparison_data <- comparison_data %>% 
      filter(log_fc <= log_fc_threshold,
             comparison_data[[pval_option]] <= p_value_threshold)
  }
  
  #if (length(genes) == 0) {
  #  return(NULL)
  #}
  
  filtered_comparison_data$comparison_name <- paste0(filtered_comparison_data$dataset_acc,"_",filtered_comparison_data$case_ann,"_vs_",filtered_comparison_data$control_ann)
  filtered_comparison_data$comparison_name <- gsub(";","_", filtered_comparison_data$comparison_name)
  filtered_comparison_data <- filtered_comparison_data %>% select(comparison_name, gene)
  gmt_data <- filtered_comparison_data %>%
    group_by(comparison_name) %>%
    summarise(gene = paste0(gene, collapse = "\t")) %>%
    mutate(description= comparison_name, .after = comparison_name)
  timestampe <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  gmt_filename <- paste0("enrichment_results/custom_geneset_db_",timestampe,".gmt")
  write.table(gmt_data,gmt_filename, sep = "\t", row.names = F, col.names = F, quote = F)
  return(gmt_filename)
}



#==============================================================================================================
# Function used to perform ORA using fora
# Returns a dataframe with pathways
#==============================================================================================================
perform_ora <- function(enrich_Database,gmt_file, genes, significance_method, threshold) {
  sigMethod <- c()
  fdrThr = 0.05
  topThr = 10
  
  if(significance_method == "FDR"){
    sigMethod <- c("fdr")
    fdrThr <- threshold
  } else if(significance_method == "TOP"){
    sigMethod <- c("top")
    topThr <- threshold
  }
  

  # Define the output directory for the results
  project_name <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
  
  # Perform ORA
  options(bitmapType='cairo')
  enrichment_results <- WebGestaltR(enrichMethod = "ORA", 
                                    organism = "hsapiens", 
                                    enrichDatabase = enrich_Database,
                                    enrichDatabaseFile =  gmt_file,
                                    enrichDatabaseType = "genesymbol",
                                    interestGene = genes,
                                    interestGeneType = "genesymbol",
                                    referenceSet = "genome_protein-coding",
                                    #referenceGeneType = "genesymbol",
                                    collapseMethod = "mean",
                                    minNum = 5,
                                    maxNum = 20000,
                                    sigMethod = sigMethod,
                                    fdrMethod = "BH",
                                    fdrThr = ifelse(!is.null(fdrThr), fdrThr, NULL),
                                    topThr = ifelse(!is.null(topThr), topThr, NULL),
                                    isOutput = TRUE,
                                    outputDirectory = "enrichment_results",
                                    projectName = project_name) #,hostName="https://www.webgestalt.org/"
  
  if(is.null(enrichment_results) || nrow(enrichment_results) == 0){
    return(NULL)
  }
  return(project_name)
}
#==============================================================================================================

























#==============================================================================================================
# Function used to perform GSEA using Webgestalt
# Returns the project name and downloads an html report
#==============================================================================================================
perform_gsea <- function(selected_comparison, organism, database, dataset_acc, comparisons, comparison_data) {
  
  # Pull the comparison ID and pull the ranked gene list associated with the given comparison
  selected_comparison_id <- comparisons %>%
    filter(comparison == selected_comparison) %>%
    pull(id)
  ranked_genes <- comparison_data %>%
    filter(comparison_id == selected_comparison_id) %>%
    select(c(gene, log_fc))
  
  # Define the output directory for the results
  project_name <- paste(dataset_acc, selected_comparison_id, database, sep = "_")
  project_name <- gsub("-", "_", project_name)
  
  # Run Webgestalt
  enrichment_results <- WebGestaltR(enrichMethod = "GSEA", 
                                    organism = organism, 
                                    enrichDatabase = database,
                                    interestGene = ranked_genes,
                                    interestGeneType = "genesymbol",
                                    collapseMethod = "mean",
                                    minNum = 5,
                                    maxNum = 2000,
                                    isOutput = TRUE,
                                    outputDirectory = "enrichment_results",
                                    projectName = project_name,hostName="https://www.webgestalt.org/")
  
  return(project_name)
}
#==============================================================================================================


#==============================================================================================================
# Function used to modify the enrichment report produced by Webgestalt
# Writes the new report to the appropriate location
#==============================================================================================================
modify_enrichment_report <- function(project_name) {
  # Import the enrichment report and cast the rownames as numeric
  enrichment_report <- read.table(paste0("./enrichment_results/Project_", project_name, "/Report_", project_name, ".html"), 
                                  sep = "\n", quote = "")
  rownames(enrichment_report) <- as.numeric(rownames(enrichment_report))
  
  # Modify rows of the enrichment report
  row_index <- which(enrichment_report$V1 == "<hr><main>")
  enrichment_report$V1[row_index] <- gsub("<hr>", "", enrichment_report$V1[row_index])
  row_index <- which(grepl('<a href="$', enrichment_report$V1)) 
  enrichment_report$V1[row_index] <- '<a href="#" class="card-header-icon">'
  
  ### Remove the header and footer of the report ###
  title_indices <- which(startsWith(enrichment_report$V1, "<title>"))
  title_indices <- c(title_indices, title_indices + 1)
  #==========
  header_start <- which(startsWith(enrichment_report$V1, "<header>"))
  header_end <- which(startsWith(enrichment_report$V1, "</header>"))
  header_indices <- header_start:header_end
  #==========
  footer_start <- which(startsWith(enrichment_report$V1, "<footer"))
  footer_end <- which(startsWith(enrichment_report$V1, "</footer>"))
  footer_indices <- footer_start:(footer_end - 1)
  #==========
  enrichment_report$V1[footer_end] <- gsub("</footer>", "", enrichment_report$V1[footer_end])
  #==========
  rows_to_remove <- c(title_indices, header_indices, footer_indices)
  enrichment_report <- filter(enrichment_report, !(rownames(enrichment_report) %in% rows_to_remove))
  ##########
  
  # Escape double quotes and single quotes
  enrichment_report$V1 <- gsub('"', '\"', enrichment_report$V1)
  enrichment_report$V1 <- gsub("'", "\'", enrichment_report$V1)
  
  write.table(enrichment_report, file = paste0("./enrichment_results/Project_", project_name, "/Report_", project_name, ".html"), 
              row.names = FALSE, col.names = FALSE, quote = FALSE)
}
#==============================================================================================================


#==============================================================================================================
# Function used to upload the enrichment results to the DB
# Writes results to the enrichment_run, enrichment_results, and enrichment_leading_edge tables
#==============================================================================================================
upload_enrichment_results <- function(selected_comparison, organism, selected_database, dataset_acc, comparisons, comparison_data, db) {
  
  # Pull the comparison ID and pull the ranked gene list associated with the given comparison
  selected_comparison_id <- comparisons %>%
    filter(comparison == selected_comparison) %>%
    pull(id)
  ranked_genes <- comparison_data %>%
    filter(comparison_id == selected_comparison_id) %>%
    mutate(rank = -1*log10(p_value_adj) * sign(log_fc)) %>%
    select(c(gene, rank))
  
  # Skip the upload if the enrichment analysis is already run for the given dataset, comparison, and enrichment database
  enrichment_results <- dbGetQuery(db,
                                   stringr::str_interp(
                                     paste("SELECT *",
                                           "FROM enrichment_run",
                                           "WHERE (dataset_acc = ${dataset_acc}",
                                           "AND comparison_id = ${selected_comparison_id}",
                                           "AND database = ${selected_database})")))
  
  if (nrow(enrichment_results) > 0) {
    # Run Webgestalt
    enrichment_results <- WebGestaltR(enrichMethod = "GSEA", 
                                      organism = organism, 
                                      enrichDatabase = selected_database,
                                      interestGene = ranked_genes,
                                      interestGeneType = "genesymbol",
                                      collapseMethod = "mean",
                                      minNum = 5,
                                      maxNum = 2000,
                                      isOutput = FALSE,
                                      fdrThr = 1)
    
    ### ENRICHMENT RUN UPLOAD ###
    enrichment_run <- data.frame("dataset_acc" = dataset_acc,
                                 "comparison_id" = selected_comparison_id,
                                 "database" = selected_database)
    dbAppendTable(db, "enrichment_run", enrichment_run)
    ##########
    
    ### ENRICHMENT RESULTS UPLOAD ###
    # Store the leading edge info
    leading_edge_data <- select(enrichment_results, c(geneSet, userId))
    # Select columns and modify column names
    enrichment_results <- enrichment_results %>%
      select(-c(link, plotPath, leadingEdgeId, userId)) %>%
      rename(geneset = geneSet,
             leading_edge_number = leadingEdgeNum,
             es = enrichmentScore,
             nes = normalizedEnrichmentScore,
             p_value = pValue,
             p_value_adj = FDR)
    # Pull the primary key ID from the enrichment run table
    dataset_id <- paste0("'", dataset_acc, "'")
    comparison_id <- paste0("'", selected_comparison_id, "'")
    database <- paste0("'", selected_database, "'")
    run_id <- dbGetQuery(db,
                         stringr::str_interp(
                           paste("SELECT id",
                                 "FROM enrichment_run",
                                 "WHERE (dataset_acc = ${dataset_id}",
                                 "AND comparison_id = ${comparison_id}",
                                 "AND database = ${database})")))
    run_id <- pull(run_id, id)
    enrichment_results$enrichment_run_id <- run_id
    # Upload the enrichment results
    dbAppendTable(db, "enrichment_results", enrichment_results)
    ##########
    
    ### ENRICHMENT LEADING EDGE UPLOAD ###
    # Pull the IDs associated with each gene set in the enrichment results table
    run_id <- paste0("'", run_id, "'")
    results_ids <- dbGetQuery(db,
                              stringr::str_interp(
                                paste("SELECT id, geneset",
                                      "FROM enrichment_results",
                                      "WHERE enrichment_run_id = ${run_id}")))
    # Pull the associated comparison data
    comparison_data <- filter(comparison_data, comparison_id == selected_comparison_id)
    
    # Create a list of dataframes containing the leading edge genes for each significant gene set
    leading_edge_list <- lapply(leading_edge_data$geneSet, function(x) {
      current_id <- results_ids %>%
        filter(geneset == x) %>%
        pull(id)
      
      genes <- leading_edge_data %>%
        filter(geneSet == x) %>% 
        pull(userId)
      genes <- unlist(strsplit(genes, split = ";"))
      
      leading_edge <- comparison_data %>%
        select(gene, log_fc) %>%
        filter(gene %in% genes) %>%
        arrange(desc(log_fc)) %>%
        mutate(enrichment_results_id = current_id) %>%
        rename(score = log_fc)
    })
    # Combine the dataframes 
    leading_edge_results <- do.call("rbind", leading_edge_list)
    
    # Upload the leading edge results
    dbAppendTable(db, "enrichment_leading_edge", leading_edge_results)
    ##########
  }
}
#==============================================================================================================
