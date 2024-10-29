#==============================================================================================================
# Function used to generate a heatmap of gene log2fc for every selected comparison
# Returns a list of plots given selected comparisons from the same dataset 
#==============================================================================================================
plot_heatmap <- function(gene, current_comparison, pval_option, plot_type, db, ceiling, floor) {
  
  # Format the comparison ID's so that they can be used for the query
  formatted_comparison_ids <- paste0("('", paste(current_comparison, collapse = "', '"), "')")
  formatted_genes <- paste0("('", paste(gene, collapse = "', '"), "')")
  
  # Pull the comparison results associated with the selected comparisons
  comparison_results <- dbGetQuery(db,
                              stringr::str_interp(
                                paste("SELECT comparison_id,gene, log_fc,p_value, p_value_adj, dataset_acc, case_ann, control_ann",
                                "FROM comparison_data",
                                "WHERE (comparison_id IN ${formatted_comparison_ids}",
                                "AND gene IN ${formatted_genes})"))) 
  
    # Include a space after every ; in the annotation column - necessary for the tick labels to wrap in the plot
  comparison_results$case_ann <- gsub(";", "_", comparison_results$case_ann)
  comparison_results$control_ann <- gsub(";","_", comparison_results$control_ann)
  
  # Lets combine comparison name
  comparison_results$comparison_name <- paste0(comparison_results$dataset_acc,"_",comparison_results$case_ann," vs ",comparison_results$control_ann)
  
  # Lets combine comparison name
  comparison_results$signed_neg_log_pvalue <- -log10(comparison_results$p_value) * sign(comparison_results$log_fc)
  comparison_results$signed_neg_log_pvalue[comparison_results$signed_neg_log_pvalue == Inf] <- NA
  comparison_results$signed_neg_log_pvalue[comparison_results$signed_neg_log_pvalue == -Inf] <- NA
  
  if(plot_type == "log_fc"){
    mat <- reshape2::acast(comparison_results, comparison_name~gene, value.var = "log_fc")
  } else if(plot_type == "p_val") {
    mat <- reshape2::acast(comparison_results, comparison_name~gene, value.var = "signed_neg_log_pvalue")
  }
  
  #reshape the data into a matrix format suitable for a heatmap
  mat[mat == Inf] <- NA
  mat[mat == -Inf] <- NA
  mat[mat < floor] <- floor
  mat[mat > ceiling] <- ceiling
  
  col_fun <- colorRampPalette(c("blue","white","red"))(100)
  
  breaks <- seq(floor, ceiling, length.out=101)
  # Heatmap of logFC for the given dataset/ comparison and gene
  heat_plot <- pheatmap(mat, color = col_fun, na_col = "grey", breaks = breaks, cluster_rows = F, cluster_cols = F,  legend_breaks = c(floor,0 ,ceiling), legend_labels = c(floor,0 ,ceiling), width = 10, height = 8)
  
  return(heat_plot)
}
#==============================================================================================================
