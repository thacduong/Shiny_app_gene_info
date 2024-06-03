library(dplyr)
library(stringr)
library(readr)
library(shiny)
library(DT)
library(readr)

file_path_gene_info = "/Users/Thacduong/Desktop/Shiny_app_gene_info-main/Homo_sapiens.gene_info.gz"
file_path_gene2ensembl = "/Users/Thacduong/Desktop/Shiny_app_gene_info-main/gene2ensembl.gz"

# load in gene_info
gene_info <- read_delim(file_path_gene_info, delim = "\t", escape_double = FALSE, trim_ws = TRUE)
gene_info = as.data.frame(gene_info)
# Remove white space in Symbol column
gene_info[["Symbol"]] = str_trim(gene_info[["Symbol"]])
View(head(gene_info))

# load in gene2ensembl
gene2ensembl <- read_delim(file_path_gene2ensembl, delim = "\t", escape_double = FALSE, trim_ws = TRUE)
gene2ensembl = as.data.frame(gene2ensembl)
View(head(gene2ensembl))
# write.csv(gene2ensembl, file = "C:/Users/80026261/OneDrive - Moffitt Cancer Center/Desktop/Shiny_app_gene_info/gene2ensembl.csv", row.names = F)

# ----- generate a big merged protein coding annotation table -----
big_table = merge(gene_info, gene2ensembl, by = "GeneID", all = T) # added "all = T" to this part for keeping all the Protein Coding Gene
colnames(big_table)[which(colnames(big_table) == "Symbol")] <- "gene" # changed the col name from "Symbol" to "gene" 
big_table_filtered = big_table[big_table$type_of_gene == "protein-coding", ] #Filter out only protein coding genes for the annotation table
big_table_filtered = big_table_filtered[big_table_filtered$`#tax_id.x` == "9606", ]
View(head(big_table_filtered, 100))
  # ----- Merge duplicated genes -----
  merge_different_values = function(x) {
    str_c(unique(x), collapse = "; ")
  }

  big_table_filtered_merged = big_table_filtered %>%
    group_by(GeneID) %>% # Changed this from "GeneID" to "gene"
    summarize(across(everything(), ~ ifelse(n_distinct(.) == 1, as.character(first(.)), merge_different_values(.))))
  View(big_table_filtered_merged)
  write.csv(big_table_filtered_merged, file = "/Users/Thacduong/Desktop/Shiny_app_gene_info-main/big_table_filtered_merged_v5_052224.csv", row.names = F)

# ----- Check which entrezID in gene_info doesn't appear on gene2ensembl -----
# missing_GeneID <- setdiff(gene2ensembl$GeneID, gene_info$GeneID)
# # Print the missing Symbols
# if (length(missing_GeneID) > 0) {
#   print("Symbols in gene_info not found in big_table:")
#   print(missing_GeneID)
# } else {
#   print("All Symbols from gene_info are in big_table")
# }
# 
# miss_GeneID_df = as.data.frame(missing_GeneID)
# colnames(miss_GeneID_df)[which(colnames(miss_GeneID_df) == "missing_GeneID")] = "GeneID"
# View(miss_GeneID_df)
# 
# df = gene2ensembl[, c("GeneID", "Ensembl_gene_identifier")]
# 
# miss_GeneID2Symbol_df = merge(miss_GeneID_df, df, by = "GeneID")
# View(miss_GeneID2Symbol_df)
# 
# write.csv(miss_GeneID2Symbol_df, file = "/Users/Thacduong/Desktop/Shiny_app_gene_info-main/miss_GeneID2Symbol.csv", row.names = F)

# # ----- detect duplicated genes -----
# duplicated_genes = unique(big_table_filtered_merged$GeneID[duplicated(big_table_filtered_merged$GeneID)]) # Change the matrix to check for the duplicate values
# 
# if (length(duplicated_genes) > 0) {
#   print("Duplicated geneIDs found:")
#   print(duplicated_genes)
# } else {
#   print("No duplicated geneIDs found")
# }

# ----- detect NA values -----
# column_name <- "Feature_type"  # Replace with the name of your column
# data_frame <- big_table_filtered_merged       # Replace with the name of your data frame
# 
# if (anyNA(data_frame[[column_name]])) {
#   print("There are NA values")
# } else {
#   print("No NA values")
# }

# HOM_MouseHumanSequence = HOM_MouseHumanSequence %>%
#   group_by(HomHumanSymbol) %>%
#   summarize(across(everything(), ~ ifelse(n_distinct(.) == 1, first(.), merge_different_values(.))))


  
# ----- Shiny app -----
shiny.maxRequestSize <- 50 * 1024^10

# Define UI
ui <- fluidPage(
  titlePanel("Table Merging App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file_upload", "Upload additional table"),
      actionButton("merge_tables", "Merge Tables"),
      downloadButton("download_PCGA_data", "Download PCGA Data"),
      downloadButton("download_additional_table_data", "Download Additional Table Data"),
      downloadButton("download_filtered_table_data", "Download Filtered Table Data")
    ),
    mainPanel(
      h3("Main Table"),
      DTOutput("main_table"),
      h3("Preview of Additional Table"),
      DTOutput("preview_table"),
      h3("Preview of Filtered Table (No EntrezID)"),
      DTOutput("filtered_table_output")  # Add this line
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Load initial main table
  gene2entrezID <- read_csv("/Users/Thacduong/Desktop/Shiny_app_gene_info-main/gene2entrezID_v2.csv")
  big_table_filtered_merged <- read_csv("/Users/Thacduong/Desktop/Shiny_app_gene_info-main/big_table_filtered_merged_v5_052224.csv")
  PCGA_table <- reactiveVal(big_table_filtered_merged)
  
  # ----- detect duplicated genes -----
  duplicated_genes = unique(big_table_filtered_merged$gene[duplicated(big_table_filtered_merged$gene)]) # Change the matrix to check for the duplicate values
  
  if (length(duplicated_genes) > 0) {
    print("Duplicated genes found in big_table_filtered_merged:")
    print(duplicated_genes)
  } else {
    print("No duplicated genes found in big_table_filtered_merged")
  }
  
  duplicated_geneIDs = unique(big_table_filtered_merged$GeneID[duplicated(big_table_filtered_merged$GeneID)]) # Change the matrix to check for the duplicate values
  
  if (length(duplicated_geneIDs) > 0) {
    print("Duplicated geneIDs found in big_table_filtered_merged:")
    print(duplicated_geneIDs)
  } else {
    print("No duplicated geneIDs found in big_table_filtered_merged")
  }
  
  # Render main table
  output$main_table <- renderDT({
    datatable(PCGA_table())
  })
  
  # Read additional table
  additional_table <- reactive({
    req(input$file_upload)
    tryCatch({
      filetype <- tools::file_ext(input$file_upload$datapath)
      read_fun <- switch(filetype,
                         tsv = read_tsv,
                         csv = read_csv,
                         read_delim)
      read_fun(input$file_upload$datapath, col_names = TRUE) %>% 
        mutate(gene = str_trim(gene)) %>% 
        merge(gene2entrezID, by = "gene", all.x = T) 
        # select(-gene)
      # Remember that at this point, the additional_table still have the "gene" column, we need to exclude or rename this column so that it won't generate extra gene column when merging with "PCGA()"
    }, error = function(e) {
      stop("Invalid file type or format.")
    })
  })
  
  # Filter rows with blank GeneID using base R
  filtered_table <- reactive({
    req(input$file_upload)
    additional_df <- additional_table()
    additional_df <- additional_df[is.na(additional_df$GeneID) | additional_df$GeneID == "", ]
  })
  
  # Print filtered_table
  output$filtered_table_output <- renderDT({
    req(filtered_table())
    # print(filtered_table())  # This line will print to the console, not the UI
    datatable(filtered_table())
  })
  
  # Download filtered_table
  output$download_filtered_table_data <- downloadHandler(
    filename = function() {
      paste0("Filtered_table_v", "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_table(), file)
    }
  )
  
  # Render preview of additional table
  output$preview_table <- renderDT({
    req(additional_table())
    datatable(additional_table())
  })
  
  # Download additional_table
  output$download_additional_table_data <- downloadHandler(
    filename = function() {
      paste0("Additional_table_v", "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(additional_table(), file)
    }
  )
  
  # Merge tables
  observeEvent(input$merge_tables, {
    req(additional_table())
    add_table = additional_table() %>% 
      select(-gene)
    # merged_table <- merge(PCGA_table(), additional_table(), by = "GeneID", all = TRUE) # Changed to merging by "GeneID"
    merged_table <- merge(PCGA_table(), add_table, by = "GeneID", all = TRUE) # Changed to merging by "GeneID"
    print(nrow(merged_table))
    print(colnames(merged_table))
    # Function to merge different values
    merge_different_values <- function(x) {
      str_c(unique(x), collapse = "; ")
    }
    
    # Summarize merged table
    merged_table <- merged_table %>%
      group_by(GeneID) %>%
      summarize(across(everything(), ~ ifelse(n_distinct(.) == 1, as.character(first(.)), merge_different_values(.))))
    
    # Update main table
    PCGA_table(merged_table)
    # ----- detect duplicated genes -----
    duplicated_genes_PCGA = unique(PCGA_table()$gene[duplicated(PCGA_table()$gene)]) # Change the matrix to check for the duplicate values
    
    if (length(duplicated_genes_PCGA) > 0) {
      print("Duplicated genes found in PCGA:")
      print(duplicated_genes_PCGA)
    } else {
      print("No duplicated genes found in PCGA")
    }
    
    duplicated_geneIDs_PCGA = unique(PCGA_table()$GeneID[duplicated(PCGA_table()$GeneID)]) # Change the matrix to check for the duplicate values
    
    if (length(duplicated_geneIDs_PCGA) > 0) {
      print("Duplicated geneIDs found in PCGA:")
      print(duplicated_geneIDs_PCGA)
    } else {
      print("No duplicated geneIDs found in PCGA")
    }
    
    # ----- Print out number of Protein Coding Gene -----
    Protein_coding_gene_num = (length(which(PCGA_table()$type_of_gene == "protein-coding")))
    print(paste("Number of Protein Coding Gene: ", Protein_coding_gene_num))
    
    # ----- Render updated main table -----
    output$main_table <- renderDT({
      datatable(PCGA_table())
    })
    
    # Download merged data
    output$download_PCGA_data <- downloadHandler(
      filename = function() {
        paste0("ProteinCodingGeneAnnotation_v", "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(PCGA_table(), file, row.names = F)
      }
    )
  })
}

# Run the application
shinyApp(ui, server)
