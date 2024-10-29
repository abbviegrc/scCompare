#==============================================================================================================
# Function used to generate a simple table for each disease used for the summary sections
#==============================================================================================================
generate_dataset_summary_table <- function(genes, selected_disease, datasets, comparisons, comparison_data) {
  # Filter the datasets for the given disease
  datasets <- filter(datasets, str_detect(disease, selected_disease))
  # If the disease was not found in the DB, return NULL
  if (nrow(datasets) == 0) {
    return(NULL)
  }
  
  # Filter the comparisons and comparison data
  comparisons <- filter(comparisons, dataset_acc %in% datasets$dataset_acc)
  selected_comparison_ids <- comparisons %>%
    filter(dataset_acc %in% datasets$dataset_acc) %>%
    pull(id)
  comparison_data <- filter(comparison_data, comparison_id %in% selected_comparison_ids)
  
  # Combine the data description and the comparison description
  comparisons <- left_join(comparisons, datasets, by = c("dataset_acc" = "dataset_acc"))
  
  # Add gene columns to the comparison description table 
  # Initialize values to NA - some of these will be changed later
  for (gene in genes) {
    comparisons[gene] <- "NA"
  }
  
  # Iterate over every gene
  for (current_gene in genes) {
    
    # Iterate over every comparison
    for (current_id in comparisons$id) {
      included_genes <- comparison_data %>%
        filter(comparison_id == current_id) %>%
        pull(gene)
      
      # Determine if the given comparison contains data on the given gene
      if (current_gene %in% included_genes) {
        comparisons[comparisons$id == current_id, current_gene] <- "info"
      }
    }
  }
  
  # Remove the unneeded columns, rename some columns, and format the group annotations
  comparisons <- comparisons %>%
    select(-c('id', 'title', 'summary', 'disease', 'organism', 'experiment_type', 'source', 'case_sample', 'control_sample')) %>%
    rename("case samples" = "case_ann", "control samples" = "control_ann") %>%
    mutate(`case samples` = gsub(';', ', ', `case samples`),
           `control samples` = gsub(';', ', ', `control samples`))
  
  return(comparisons)
}
#==============================================================================================================



#==============================================================================================================
# Function used to style the summary table
#==============================================================================================================
style_dataset_summary_table <- function(summary_table) {
  summary_table <- summary_table %>%
    kable("html", escape = FALSE, row.names = FALSE) %>%
    kable_styling(font_size = 12, bootstrap_options = c("striped", "hover", "condensed", "bordered"), full_width = T) %>%  #
    row_spec(0, bold = TRUE, font_size = 14, color = "white", background = "#2C3E4C") %>%
    row_spec(1:nrow(summary_table), font_size = 14) %>%
    column_spec(1:ncol(summary_table)) #, extra_css = "border: 1px solid black;"
}

