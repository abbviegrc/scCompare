
# function

'%||%' <- function(a,b){
  if(!is.null(a)) a else b
} 


#==============================================================================================================
# Generate a table allowing
# Returns 
#==============================================================================================================


create_data_table <- function(input_table,dt_width){
  
  selected_columns <- colnames(input_table)[2:ncol(input_table)]
  colnames(input_table) <- c("Gene",sapply(1:(ncol(input_table)-1),function(i){
    paste0("list",i)}))
  curated_table <- datatable(
    input_table,
    extensions = "Buttons",
    escape = FALSE,
    class = "custom-datatable",
    rownames=FALSE,
    selection = 'none',
    options = list(headerCallback = JS(
      "function(thead, data, start, end, display){",
      "  $(thead).remove();",
      "}"),
      dom = 'ti<"bottom"p><"bottom_buttons"B>', paging = T, ordering = FALSE,
      scrollX=TRUE,buttons = list("copy",
                                  list(extend = "csv", text = "Download Full Results", filename = "data",
                                       exportOptions = list(modifier = list(page = "all")
                                       ))
      )
    )) %>% formatStyle(colnames(input_table),color=styleEqual(c("yes", "no"), c('red', 'white')),
                       backgroundColor = styleEqual(c(
                         "yes", "no"), c('red', 'white'))
    ) %>% formatStyle(columns = c(1), width=paste0(dt_width,'px')) %>%
    formatStyle(columns=c(2:ncol(input_table)),width="20px")
  return(list(output_table=curated_table,list_names=selected_columns))
}




#==============================================================================================================
# Query the comparisons table for the user specified filter criteria
# Returns a list with genes under each comparison id
#==============================================================================================================
# (input$all_pval_option, input$all_logfc_threshold, input$all_pval_threshold, )
get_gene_source <- function(comparisons,pval_option, logfc_option, logfc_threshold, pval_threshold, db){
  #Pull comparison ID's
  comparison_ids <- comparisons$id
  
  # Format the comparison ID's so that they can be used for the query
  formatted_comparison_ids <- paste0("('", paste(comparison_ids, collapse = "', '"), "')")
  
  # Import the associated comparison results
  comparison_results <- dbGetQuery(db,
                                   stringr::str_interp(
                                     paste("SELECT gene, log_fc, p_value_adj, p_value, comparison_id",
                                           "FROM comparison_data",
                                           "WHERE (comparison_id IN ${formatted_comparison_ids})")))  
  
  # Join the selected comaprisons and comparison results dataframes
  combined_results <- left_join(comparisons, comparison_results, 
                                by = c("id" = "comparison_id"))
  
  combined_results$case_ann <- gsub(";", "_", combined_results$case_ann)
  combined_results$control_ann <- gsub(";", "_", combined_results$control_ann)
  combined_results <- mutate(combined_results, annotation = paste0(dataset_acc,"_",case_ann, " vs ", control_ann))
  
  pval_col <- pval_option
  
  # for each data set filter the comparisons 
  if (logfc_option == "up") {
    filtered_comparison <- combined_results %>% 
      filter(log_fc >= logfc_threshold,
             combined_results[[pval_option]] <= pval_threshold)
  } else {
    filtered_comparison <- combined_results %>% 
      filter(log_fc <= logfc_threshold,
             combined_results[[pval_option]] <= pval_threshold)
  }
  
  gene_source <- filtered_comparison %>%
    select(source = dataset_acc, gene_list = annotation) %>%
    distinct()
  
  
  return(gene_source)
}


#==============================================================================================================
# Query the geneset_data table for the user specified filter criteria
# Returns a list with genes under each comparison id
#==============================================================================================================

get_gene_list <- function(comparisons,pval_option,logfc_option, logfc_threshold, pval_threshold, db){
  # Pull the comparison ID's
  comparison_ids <- comparisons$id
  
  # Format the comparison ID's so that they can be used for the query
  formatted_comparison_ids <- paste0("('", paste(comparison_ids, collapse = "', '"), "')")
  
  # Import the associated comparison results
  comparison_results <- dbGetQuery(db,
                                   stringr::str_interp(
                                     paste("SELECT gene, log_fc, p_value_adj, p_value, comparison_id",
                                           "FROM comparison_data",
                                           "WHERE (comparison_id IN ${formatted_comparison_ids})")))  
  
  # Join the selected comaprisons and comparison results dataframes
  combined_results <- left_join(comparisons, comparison_results, 
                                by = c("id" = "comparison_id"))
  
  combined_results$case_ann <- gsub(";", "_", combined_results$case_ann)
  combined_results$control_ann <- gsub(";", "_", combined_results$control_ann)
  combined_results <- mutate(combined_results, annotation = paste0(dataset_acc,"_",case_ann, " vs ", control_ann))
  
  pval_col <- pval_option
  
  # for each data set filter the comparisons 
  if (logfc_option == "up") {
    filtered_comparison <- combined_results %>% 
      filter(log_fc >= logfc_threshold,
             combined_results[[pval_option]] <= pval_threshold)
  } else {
    filtered_comparison <- combined_results %>% 
      filter(log_fc <= logfc_threshold,
             combined_results[[pval_option]] <= pval_threshold)
  }
  
  gene_lists <- split(filtered_comparison$gene, filtered_comparison$annotation)
  
  return(gene_lists)
}
