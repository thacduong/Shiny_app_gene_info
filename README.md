Installation 

Via Download
•	Download ZIP file from https://github.com/thacduong/Shiny_app_gene_info 
•	Unzip and load into directory as a project in R Studio
•	Open the ‘App.R’ script and write in user input files and options as directed at the top of the script
o	‘App.R’ script begins with example files loaded in 

Requirement
•	R -  https://cran.r-project.org/src/base/R-4/
•	R Studio -  https://www.rstudio.com/products/rstudio/download/

R Dependencies
•	DT_0.31
•	shiny_1.8.0
•	readr_2.1.4
•	stringr_1.5.1
•	dplyr_1.1.4

Required Files

Required User Input Files 
•	PCGA (.csv)
o	Must be comma-delimited which contain gene symbols as “gene” column, and entrez ID as “GeneID” column, along with other info columns. 
•	Gene2entrezID (.csv)
o	Must be comma-delimited with 2 columns, “gene” column as gene symbols, and “GeneID” as entrez ID.
o	This file is required for appending the entrez ID to the additional table by gene symbols. 
•	Additional table (.csv/.tsv)
o	Must be comma-delimited or tab-delimited which contain a “gene” column as gene symbols, and an additional column. 

Getting started script 
•	This script is used for preparing the PCGA and the Gene2entrezID tables from gene_info and gene2ensembl tables. 

Preparing the R Shiny App 
The Shiny App requires just a few user inputs and it can be up and running. Once the app script is loaded all that is needed to update the user input and then it can be run. 

User Input for R Shiny App

User can upload the designated version of PCGA table 
<img width="311" alt="image" src="https://github.com/thacduong/Shiny_app_gene_info/assets/129244589/9b9b6364-0f2e-4e68-96a6-bcedc83b5be6">
1.	Users can change the path of of PCGA table 


The app will be displaying the PCGA table first 
<img width="328" alt="image" src="https://github.com/thacduong/Shiny_app_gene_info/assets/129244589/d2bdf307-5f51-49e9-bbf9-41d43ce032fd">
 
1.	Upload the additional table 


Shiny App Features

Append additional tables to the Protein Coding Gene Annotation table
•	The additional table is appended to the Protein Coding Gene Annotation table by entrez ID.
•	Additional tables can keep being added to the Protein Coding Gene Annotation table. 
•	The updated version of Protein Coding Gene Annotation table can be downloaded for further downstream analysis. 

Preview those genes without the entrez ID from the additional table
•	Those genes without the entrez ID can be keeping track and downloaded for further analysis. 

![image](https://github.com/thacduong/Shiny_app_gene_info/assets/129244589/80a71973-6b9a-4a13-aaa1-fa6905e04f62)
