#-----Load required libraries ----#
library(dplyr)
library(stringr)
library(readr)
library(shiny)
library(DT)
library(readr)

# ----- Shiny app -----  #
shiny.maxRequestSize <- 50 * 1024^10

# Define UI
ui <- fluidPage(
  titlePanel("Protein Coding Gene Annotation Merging App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file_upload", "Upload additional table"),
      actionButton("merge_tables", "Merge Tables"),
      downloadButton("download_PCGA_data", "Download PCGA Data"),
      downloadButton("download_additional_table_data", "Download Additional Table Data"),
      downloadButton("download_filtered_table_data", "Download Filtered Table Data")
    ),
    mainPanel(
      h3("PCGA Table"),
      DTOutput("main_table"),
      h3("Preview of Additional Table"),
      DTOutput("preview_table"),
      h3("Preview of gene with no EntrezID"),
      DTOutput("filtered_table_output")  # Add this line
    )
  )
)

# Define server logic

# Please change to your designated version
PCGA_path = "https://raw.githubusercontent.com/thacduong/Shiny_app_gene_info/main/ProteinCodingGeneAnnotation_20240620_v0.csv"
gene2entrezID_path = "https://raw.githubusercontent.com/thacduong/Shiny_app_gene_info/main/gene2entrezID_v2.csv"
server <- function(input, output) {
  
  # Load initial main table
  PCGA <- read_csv(PCGA_path)
  gene2entrezID <- read_csv(gene2entrezID_path)
  PCGA_table <- reactiveVal(PCGA)
  
  # ----- detect duplicated genes -----
  duplicated_genes = unique(PCGA$gene[duplicated(PCGA$gene)]) # Change the matrix to check for the duplicate values
  
  if (length(duplicated_genes) > 0) {
    print("Duplicated genes found in PCGA:")
    print(duplicated_genes)
  } else {
    print("No duplicated genes found in PCGA")
  }
  
  duplicated_geneIDs = unique(PCGA$GeneID[duplicated(PCGA$GeneID)]) # Change the matrix to check for the duplicate values
  
  if (length(duplicated_geneIDs) > 0) {
    print("Duplicated geneIDs found in PCGA:")
    print(duplicated_geneIDs)
  } else {
    print("No duplicated geneIDs found in PCGA")
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
