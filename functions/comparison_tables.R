
#==============================================================================================================
# Function used to generate a comparison table for each disease
# UNSTYLED
# Returns a list of dataframes
#==============================================================================================================
generate_comparison_tables <- function(genes, selected_diseases, diseases, datasets, comparisons,
                                       comparison_data, pval_option, pval_threshold, logfc_threshold, remove_ns) {
  
  # Produce a set of table(s) for every user specified disease
  table_list <- list()
  
  # Format the case and control samples columns (these are replaced by the group and comparison columns)
  comparisons <- format_comparison(comparisons)
  
  # Pull the dataset ID's from the data description (these will be used to order the dataframe after the join)
  unique_dataset_acc <- unique(comparisons$dataset_acc)
  # Combine the data description and the comparison description
  comparisons <- comparisons %>%
    left_join(datasets, by = c("dataset_acc" = "dataset_acc")) %>% 
    arrange(factor(dataset_acc, levels = unique_dataset_acc))
  
  # Determine if the user specified p-value or adjusted p-value under the SIGNIFICANCE THRESHOLDS option
  pval_col <- pval_option
    
    ##########
    for (current_disease in selected_diseases) {
      
      # Filter the comparisons for the given disease and add gene columns (initialized to NA)
      filtered_comparison <- comparisons[sapply(comparisons$disease, function(d){
        disease_parts <- unlist(strsplit(d,";"))
        any(disease_parts %in% current_disease)
      }), ]
      
      #current_comparisons <- filtered_comparison
      current_comparisons <- filtered_comparison %>%
        filter((comparison_group == "Disease;CellType" & grepl(current_disease, comparison)) | comparison_group != "Disease;CellType" )
      
      if (nrow(current_comparisons) == 0) {
        table_list[[current_disease]] <- NULL
        next
      }
      
      for (gene in genes) {
        current_comparisons[gene] <- "NA"
      }
      
      
      ### Iterate over every gene ###
      for (current_gene in genes) {
        print(current_comparisons$id)
        # Iterate over every comparison
        for (current_id in current_comparisons$id) {
          included_genes <- comparison_data %>%
            filter(comparison_id == current_id) %>%
            pull(gene)
          
          # Determine if the given comparison contains data on the given gene
          if (current_gene %in% included_genes) {
            comparison_info <- filter(comparison_data, comparison_id == current_id & gene == current_gene)
            current_gene_data <- HTML(paste0("name: ", current_gene ,
                                             "<br/> logFC: ", round(comparison_info$log_fc, 4),
                                             "<br/> p-value: ", round(comparison_info$p_value, 6),
                                             "<br/> adj p-value: ", round(comparison_info$p_value_adj, 6)))
            
            # Determine if the given gene passes the user specified thresholds (i.e. p-value, q-value, logFC)
            #significance_status <- comparison_info$p_value <= 0.05 & abs(comparison_info$log_fc) >= 1
            significance_status <- comparison_info[[pval_col]] <= pval_threshold & abs(comparison_info$log_fc) >= logfc_threshold
            if (significance_status) {
              # Add the popup for the given cell (blue background: positive logFC, red background: negative logFC)
              if (comparison_info$log_fc > 0) {
                current_comparisons[current_comparisons$id == current_id, current_gene] <- cell_spec("up",
                                                                                                     popover = current_gene_data,
                                                                                                     color = "#1f37f0",
                                                                                                     background = "#f2bebb")
              } else {
                current_comparisons[current_comparisons$id == current_id, current_gene] <- cell_spec("down",
                                                                                                     popover = current_gene_data,
                                                                                                     color = "#1f37f0",
                                                                                                     background = "#bad8f7")
              }
            } else {
              # Add the popup for the given cell
              current_comparisons[current_comparisons$id == current_id, current_gene] <- cell_spec("NS",
                                                                                                   popover = current_gene_data,
                                                                                                   color = "black")
            }
          } else {
            current_gene_data <- HTML(paste0("name: ", current_gene,
                                             "<br/> No associated data"))
            # Add the popup for the given cell
            current_comparisons[current_comparisons$id == current_id, current_gene] <- cell_spec("NA",
                                                                                                 popover = current_gene_data,
                                                                                                 color = "#db160f")
          }
        }
        
      }
      ##########
      
      # Determine the rows that contain all NA's, all NS's, or all of both
      # This is used to determine which rows to remove based on the user specified option
      gene_df <- select(current_comparisons, all_of(genes))
      row_labels <- apply(gene_df, 1, function(x) {
        row_text <- unname(sapply(x, function(y) ifelse(y == "NA", "NA", str_match(y, ">\\s*([a-zA-Z]+?)\\s*</span>$")[2])))
        if (all(row_text == 'NA')) {
          return("all NA")
        } else if (all(row_text == 'NA' | row_text == 'NS')) {
          return("all NA or NS")
        } else {
          return("keep")
        }
      })
      
      
      ### Remove rows based on the user specified preferences ###
      if (remove_ns) {
        rows_to_keep <- row_labels == "keep"
      } else {
        rows_to_keep <- !(row_labels == "all NA")
      }
      current_comparisons <- filter(current_comparisons, rows_to_keep)
      ### IF ALL ROWS ARE REMOVED, assign NULL for the given disease in the table_list ###
      
      if (nrow(current_comparisons) == 0) {
        table_list[[current_disease]] <- NULL
        next
      }
      
      # Pull the text from the dataset ID column (these are now hyperlinks so we need the text)
      id_text <- sapply(current_comparisons$dataset_acc, function(x) str_match(x, ">\\s*(.+?)\\s*</a>$")[2])
      # Add a column with checkboxes (this is used to create selections for filtering rows and plotting)
      checkbox_col <- paste0('<input type="checkbox" id="', id_text, "_", current_disease, "_", current_comparisons$id, '">')
      current_comparisons["selection"] <- checkbox_col
      
      # Assign the data to the table list
      table_list[[current_disease]] <- current_comparisons
    }
  #}

  ##########
  
  return(table_list)
}
#==============================================================================================================


#==============================================================================================================
# Function used to style the comparison tables produced by the generate_comparison_tables function
# STYLED
# Returns a list of kable objects
#==============================================================================================================
style_comparison_tables <- function(diseases, table_list) {
  
  # Iterate over every disease in order to style the associated dataframe
  for (disease in diseases) {
    
    # Pull the associated dataframe - If null, continue to the next iteration
    comparison_table <- table_list[[disease]]
    if (is.null(comparison_table)) {
      next
    }
    
    # Create headers including the title, summary, organism, type, and source for every data ID
    data_header_df <- select(comparison_table, c("dataset_acc", "title", "organism", "experiment_type", "source", "treatment"))
    header_list <- c()
    for (i in 1:nrow(data_header_df)) {
      # Specify the data description for the given data ID
      study_data <- paste0("Title: ", gsub('"', '', data_header_df$title[i]),
                           "\nOrganism: ", data_header_df$organism[i],
                           "\nExperiment Type: ", data_header_df$experiment_type[i],
                           "\nSource: ", data_header_df$source[i],
                           "\nTreatment: ", data_header_df$treatment[i])
      # Append the data to the header list
      header_list[i] <- study_data
    }
    
    # Remove the ID, title, and summary columns and format the column names
    comparison_table <- comparison_table %>%
      select(-c("id", "disease", "title", "summary", "organism", "experiment_type", "cell_type","source", "treatment", "case_sample", "control_sample")) %>%
      rename("data ID" = "dataset_acc")
    
    # Generate the styled table
    data_info <- factor(header_list, unique(header_list))
    comparison_table <- comparison_table %>% kable('html', escape = FALSE, row.names = FALSE) %>%
      kable_styling(font_size = 12) %>%
      row_spec(0, font_size = 14, color = "white", background = "#2C3E4C") %>%
      row_spec(1:nrow(comparison_table), background = 'white') %>%
      column_spec(1, width = "15em", extra_css = "text-align: left;") %>%
      column_spec(2, width = "20em", extra_css = "text-align: left; padding-left: 0 !important;") %>%  #, extra_css = "text-align: left; padding-left: 0 !important;"
      column_spec(3, width = "30em") %>%  #, extra_css = "text-align: left; padding-left: 0 !important;"
      collapse_rows(1, valign = 'top') %>%
      pack_rows(index = table(data_info), background = "#E1E2E3", 
                label_row_css = "border-top: 2px solid black;")
    
    table_list[[disease]] <- comparison_table
  }
  
  return(table_list)
}
#==============================================================================================================
