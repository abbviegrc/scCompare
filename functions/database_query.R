
#==============================================================================================================
# Format user specified queries 
#==============================================================================================================
format_query_ExactMatch <- function(query) {
  if (is.null(query)) {
    return("'%'")
  } else if (length(query) == 1) {
    return(paste0("'", query, "'"))
  } else {
    return(paste0("'(", paste(query, collapse = "|"), ")'"))
  }
}

format_query_SoftMatch <- function(query) {
  if (is.null(query)) {
    return("'%'")
  } else if (length(query) == 1) {
    return(paste0("'%", query, "%'"))
  } else {
    return(paste0("'%(", paste(query, collapse = "|"), ")%'"))
  }
}
#==============================================================================================================


#==============================================================================================================
# Produce a list of exact diseases from the datasets matching the user query
# Returns a list of diseases
#==============================================================================================================
get_exact_diseases <- function(datasets) {
  # Remove keywords Healthy and nonIBD and pull the unique diseases
  diseases <- datasets %>%
    arrange(disease) %>%
    pull(disease) %>%
    unique()
  
  return(diseases)
}
#==============================================================================================================


#==============================================================================================================
# Query the dataset table for datasets that match the user query
# Returns a dataframe with dataset descriptions
#==============================================================================================================
get_datasets <- function(diseases, sources, treatments, cell_types, db, exact_disease) {
  
  # Import the filtered data description
  datasets <- dbGetQuery(db,
                         stringr::str_interp(
                           paste("SELECT dataset_acc, Disease, Organism, Source,experiment_type, cell_type, Treatment, Title, Summary",
                                 "FROM dataset")))
  attributes <- list("disease" = diseases,
                     "source" = sources, 
                     "cell_type" = cell_types, 
                     "treatment" = treatments)
  
  rows_to_keep <- lapply(names(attributes), function(x) {
    if (is.null(attributes[[x]])) {
      return(rep(TRUE, nrow(datasets)))
    } else {
        return(sapply(datasets[[x]], function(y) any(str_detect(y, attributes[[x]]))))
      }
  })
  rows_to_keep <- Reduce("&", rows_to_keep)
  
  # Filter the dataset table for datasets that match the values for the modified attributes
  filtered_datasets <- filter(datasets, rows_to_keep)
  if(!is.null(attributes$cell_type)) {
    filtered_datasets <- filtered_datasets[sapply(filtered_datasets$cell_type, function(names) {
      split_cell_types <- trimws(unlist(strsplit(names,";")))
      any(split_cell_types %in% attributes$cell_type)
    }),]
  }
  
  return(filtered_datasets)
}
#==============================================================================================================


#==============================================================================================================
# Pull the dataset descriptions displayed in the dataset selection table from the DB
# Returns a dataframe
#==============================================================================================================
get_dataset_table_data <- function(datasets, db) {
  # Format the dataset IDs for the query
  formatted_dataset_acc_list <- paste0("('", paste(datasets$dataset_acc, collapse = "', '"), "')")
  # Pull the datasets
  datasets <- dbGetQuery(db,
                         stringr::str_interp(
                           paste("SELECT dataset_acc, title, disease, source, treatment",
                                 "FROM dataset",
                                 "WHERE dataset_acc IN ${formatted_dataset_acc_list}")))
  
  # Sort the data description by several features and format long features (i.e. add ellipsis)
  datasets <- datasets %>%
    arrange(disease,dataset_acc) %>%
    mutate(title = str_trunc(title, width = 40, ellipsis = "..."),
           disease = str_trunc(disease, width = 25, ellipsis = "..."),
           source = str_trunc(source, width = 25, ellipsis = "..."),
           treatment = str_trunc(treatment, width = 25, ellipsis = "..."))

  
  return(datasets)

}
#==============================================================================================================


#==============================================================================================================
# Query the comparison table for the following matches
# Returns a dataframes with comparison descriptions
#==============================================================================================================
get_comparisons <- function(datasets, diseases, sources, treatments, cell_types, db) {
  # Format the data ID list and other features so that they can be used for the query
  formatted_data_acc <- paste0("('", paste(datasets$dataset_acc, collapse = "', '"), "')")
  
  cellType_str <- paste(paste0("(LOWER(case_ann) LIKE LOWER('%", cell_types, "%') OR LOWER(control_ann) LIKE LOWER('%",cell_types, "%'))"), collapse = " OR ")
  
  # Import the comparisons
  stmt <- stringr::str_interp( 
                              paste("SELECT id, dataset_acc, comparison_group, case_ann, case_sample,  control_ann, control_sample", #
                                    "FROM comparison",
                                    "WHERE (dataset_acc IN ${formatted_data_acc}",
                                    "AND (", cellType_str, "))"))
  
  comparisons <- dbGetQuery(db,stmt)
  
  # If no comparisons match the user specified queries, return None for both the comparisons and comparison data
  if (nrow(comparisons) == 0) {
    return(NULL)
  }
  
  # Filter out comparisons not associated with any of the specified diseases, sources, or treatments
   comparisons <- mutate(comparisons, annotation = paste0(case_ann, ";", control_ann))
   rows_to_keep <- sapply(1:nrow(comparisons), function(x) {
    current_dataset_acc <- comparisons$dataset_acc[x]
    current_annotations <- unlist(strsplit(comparisons$annotation[x], ";"))
    
    # Determine if the comparison is relevant based on the specified diseases
    disease_relevance <- ifelse((comparisons$comparison_group[x] == "Disease;CellType"), any(diseases %in% current_annotations), TRUE)
    
    # Determine if the comparison is relevant based on the specified cell types
    celltype_relevance <- any(cell_types %in% current_annotations)
    
    # Determine if the comparison is relevant based on the specified treatments
    current_treatments <- datasets %>%
      filter(dataset_acc == current_dataset_acc) %>%
      pull(treatment)
    current_treatments <- unlist(strsplit(current_treatments, ";"))
    treatment_relevance <- ifelse(!is.null(treatments) & length(current_treatments) > 1, any(treatments %in% current_annotations), TRUE)
    
    return(disease_relevance & celltype_relevance & treatment_relevance)  
  
  })
  comparisons <- comparisons %>%
    filter(rows_to_keep) %>%
    select(-annotation)
  
  return(comparisons)
}
#==============================================================================================================


#==============================================================================================================
# Query the comparison_data table for the following matches
# Returns a dataframe with comparison metrics
#==============================================================================================================
get_comparison_data <- function(comparisons, genes, db) {
  # If no comparisons match the user specified queries, return None for both the comparisons and comparison data
  if (nrow(comparisons) == 0 | is.null(comparisons)) {
    return(NULL)
  }
  
  comparison_data <- lapply(comparisons$id, function(comparison_id) {
    # Format the primary key ID and gene list so that they can be used for the query
    formatted_comparison_id <- paste0("'", comparison_id, "'")
    formatted_genes <- paste0("('", paste(genes, collapse = "', '"), "')")
    
    # Import the filtered comparison data
    results <- dbGetQuery(db,
                          stringr::str_interp(
                            paste("SELECT comparison_id, gene, log_fc, p_value, p_value_adj",
                                  "FROM comparison_data",
                                  "WHERE (comparison_id = ${formatted_comparison_id}",
                                  "AND gene IN ${formatted_genes})")))
    
    # Convert the gene names to uppercase
    results <- mutate(results, gene = toupper(gene))
    
    return(results)
  })
  comparison_data <- do.call(rbind, comparison_data)
  
  return(comparison_data)
}
#==============================================================================================================


#==============================================================================================================
# Function used to query the comparison_data table for comparison metrics for the given comparison
#==============================================================================================================
get_enrichment_comparison_data <- function(comparison_ids, gene_map, db) {
  
  # Format the comparison IDs so that it can be used for the query
  if (length(comparison_ids) == 1) {
    comparison_ids <- paste0("('", comparison_ids, "')")
  } else {
    comparison_ids <- paste0("('", paste(comparison_ids, collapse = "', '"), "')")
  }
  
  stmt <- stringr::str_interp(
    paste("SELECT comparison_id, gene,log_fc, p_value_adj, p_value,dataset_acc,case_ann,control_ann",
          "FROM comparison_data",
          "WHERE comparison_id IN ${comparison_ids}"))
  print(stmt)
  # Pull the comparison data
  comparison_data <- dbGetQuery(db,stmt)
  
  return(comparison_data)
}
#==============================================================================================================

