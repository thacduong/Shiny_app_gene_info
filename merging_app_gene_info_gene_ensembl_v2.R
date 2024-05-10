library(dplyr)
library(stringr)
library(readr)
library(shiny)
library(DT)
library(readr)

# path = getwd()
# 
# file_path_gene_info = "C:/Users/80026261/OneDrive - Moffitt Cancer Center/Desktop/Shiny_app_gene_info/Homo_sapiens.gene_info.gz"
# file_path_gene2ensembl = "C:/Users/80026261/OneDrive - Moffitt Cancer Center/Desktop/Shiny_app_gene_info/gene2ensembl.gz"
# 
# # load in gene_info
# gene_info <- read_delim(file_path_gene_info, delim = "\t", escape_double = FALSE, trim_ws = TRUE)
# gene_info = as.data.frame(gene_info)
# View(head(gene_info))
# 
# # load in gene2ensembl
# gene2ensembl <- read_delim(file_path_gene2ensembl, delim = "\t", escape_double = FALSE, trim_ws = TRUE)
# gene2ensembl = as.data.frame(gene2ensembl)
# View(head(gene2ensembl))
# 
# 
# # write.csv(gene2ensembl, file = "C:/Users/80026261/OneDrive - Moffitt Cancer Center/Desktop/Shiny_app_gene_info/gene2ensembl.csv", row.names = F)
# # generate a big merged table
# big_table = merge(gene_info, gene2ensembl, by = "GeneID")
# colnames(big_table)[which(colnames(big_table) == "Symbol")] <- "gene"
# big_table_filtered = big_table[big_table$type_of_gene == "protein-coding", ]
# View(head(big_table_filtered, 100))
# View(head(big_table))
# 
# detect duplicated genes
# duplicated_genes = unique(ProteinCodingGeneAnnotation_20240510_HOM_MouseHumanSequence_HumanCentric_v2$GeneID[duplicated(ProteinCodingGeneAnnotation_20240510_HOM_MouseHumanSequence_HumanCentric_v2$GeneID)])
# 
# if (length(duplicated_genes) > 0) {
#   print("Duplicated genes found:")
#   print(duplicated_genes)
# } else {
#   print("No duplicated genes found")
# }

# # detect NA values
# column_name <- "Feature_type"  # Replace with the name of your column
# data_frame <- big_table_filtered_merged       # Replace with the name of your data frame
# 
# if (anyNA(data_frame[[column_name]])) {
#   print("There are NA values")
# } else {
#   print("No NA values")
# }

# # Merge duplicated genes
# merge_different_values = function(x) {
#   str_c(unique(x), collapse = "; ")
# }
# 
# HOM_MouseHumanSequence_filtered_2 = HOM_MouseHumanSequence %>% 
#   group_by(GeneID) %>%
#   summarize(across(everything(), ~ ifelse(n_distinct(.) == 1, as.character(first(.)), merge_different_values(.)))) 

# HOM_MouseHumanSequence = HOM_MouseHumanSequence %>%
#   group_by(HomHumanSymbol) %>%
#   summarize(across(everything(), ~ ifelse(n_distinct(.) == 1, first(.), merge_different_values(.))))

# View(HOM_MouseHumanSequence_filtered_2)

# write.csv(big_table_filtered_merged, file = "C:/Users/80026261/OneDrive - Moffitt Cancer Center/Desktop/Shiny_app_gene_info/big_table_filtered_merged.csv", row.names = F)


options(shiny.maxRequestSize = 30*1024^2)
ui <- fluidPage(
  titlePanel("Table Merging App"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file_upload", "Upload additional table"),
      actionButton("merge_tables", "Merge Tables"),
      downloadButton("download_data", "Download Data")
    ),
    mainPanel(
      h3("Main Table"),
      DTOutput("main_table"),
      h3("Preview of Additional Table"),
      DTOutput("preview_table")
    )
  )
)

server <- function(input, output) {
  
  big_table_filtered_merged <- read_csv("/Users/Thacduong/Desktop/Shiny_app_gene_info-main/ForZenodoUpload/ProteinCodingGeneAnnotation_20240503_v13.csv")
  # big_table_filtered_merged <- read_csv("/Users/Thacduong/Desktop/Shiny_app_gene_info-main/big_table_filtered_merged.csv")
                                        
  output$main_table <- renderDT({
    datatable(big_table_filtered_merged)
  })
  
  additional_table <- reactive({
    req(input$file_upload)
    
    tryCatch({
      filetype <- tools::file_ext(input$file_upload$datapath)
      
      read_fun <- switch(filetype,
                         tsv = read_tsv,
                         csv = read_csv,
                         read_delim)
      
      read_fun(input$file_upload$datapath, col_names = TRUE)
    }, error = function(e) {
      stop("Invalid file type or format.")
    })
  })
  
  output$preview_table <- renderDT({
    req(additional_table())
    datatable(additional_table())
  })
  
  observeEvent(input$merge_tables, {
    req(additional_table())
    str(big_table_filtered_merged$GeneID)
    str(additional_table()$GeneID)
    
    # big_table_filtered_merged <- merge(big_table_filtered_merged, additional_table(), by = "gene", all = TRUE)
    big_table_filtered_merged <- merge(big_table_filtered_merged, additional_table(), by = "GeneID", all = TRUE)
    merge_different_values = function(x) {
      str_c(unique(x), collapse = "; ")
    }
    
    big_table_filtered_merged = big_table_filtered_merged %>% 
      # group_by(gene) %>% 
      group_by(GeneID) %>%
      summarize(across(everything(), ~ ifelse(n_distinct(.) == 1, as.character(first(.)), merge_different_values(.))))
    
    
    output$main_table <- renderDT({
      datatable(big_table_filtered_merged)
    })
    
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("big_table_filtered_merged_v2", "_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(big_table_filtered_merged, file)
      }
    )
  })
}

shinyApp(ui, server)
