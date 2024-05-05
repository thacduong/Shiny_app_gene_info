library(dplyr)
library(stringr)
library(readr)

file_path_gene_info = "C:/Users/80026261/OneDrive - Moffitt Cancer Center/Desktop/Shiny_app_gene_info/Homo_sapiens.gene_info.gz"
file_path_gene2ensembl = "C:/Users/80026261/OneDrive - Moffitt Cancer Center/Desktop/Shiny_app_gene_info/gene2ensembl.gz"

# load in gene_info
gene_info <- read_delim(file_path_gene_info, delim = "\t", escape_double = FALSE, trim_ws = TRUE)
gene_info = as.data.frame(gene_info)
View(head(gene_info))

# load in gene2ensembl
gene2ensembl <- read_delim(file_path_gene2ensembl, delim = "\t", escape_double = FALSE, trim_ws = TRUE)
gene2ensembl = as.data.frame(gene2ensembl)
View(head(gene2ensembl))


# write.csv(gene2ensembl, file = "C:/Users/80026261/OneDrive - Moffitt Cancer Center/Desktop/Shiny_app_gene_info/gene2ensembl.csv", row.names = F)
# generate a big merged table
big_table = merge(gene_info, gene2ensembl, by = "GeneID")
colnames(big_table)[which(colnames(big_table) == "Symbol")] <- "gene"
big_table_filtered = big_table[big_table$type_of_gene == "protein-coding", ]
View(head(big_table_filtered, 100))
View(head(big_table))

# detect duplicated genes
duplicated_genes = unique(big_table_filtered$gene[duplicated(big_table_filtered$gene)])

if (length(duplicated_genes) > 0) {
  print("Duplicated genes found:")
  print(duplicated_genes)
} else {
  print("No duplicated genes found")
}

# Merge duplicated genes
merge_different_values = function(x) {
  str_c(unique(x), collapse = "; ")
}

big_table_filtered_merged = big_table_filtered %>% 
  group_by(gene) %>% 
  summarize(across(everything(), ~ ifelse(n_distinct(.) == 1, first(.), merge_different_values(.))))

View(big_table_filtered_merged)

write.csv(big_table_filtered_merged, file = "C:/Users/80026261/OneDrive - Moffitt Cancer Center/Desktop/Shiny_app_gene_info/big_table_filtered_merged.csv", row.names = F)


library(shiny)
library(DT)
library(readr)

shiny.maxRequestSize = 50 * 1024^10

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
  
  big_table_filtered_merged <- read_csv("C:/Users/80026261/OneDrive - Moffitt Cancer Center/Desktop/Shiny_app_gene_info/big_table_filtered_merged.csv")
  
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
    
    big_table_filtered_merged <- merge(big_table_filtered_merged, additional_table(), by = "gene", all = TRUE) 
    merge_different_values = function(x) {
      str_c(unique(x), collapse = "; ")
    }
    
    big_table_filtered_merged = big_table_filtered_merged %>% 
      group_by(gene) %>% 
      summarize(across(everything(), ~ ifelse(n_distinct(.) == 1, first(.), merge_different_values(.))))
    
    
    output$main_table <- renderDT({
      datatable(big_table_filtered_merged)
    })
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("big_table_filtered_merged_v2", "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(big_table_filtered_merged, file)
    }
  )
}

shinyApp(ui, server)
