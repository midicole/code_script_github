### The function needs a downloaded .csv file from the PGScatalog to make sure
### the auto download of metadata of these PRSs run normally.
### Web scraping can be an optimization option, but it may violate data rule of 
### PGScatalog, and really time-consuming at this stage.
autodown_pgsc<-function(summary_file,dest_path){
  library(dplyr)
  ### summary_file: location of the summary file with quotation marks
  ### trait_column: the column name of interest must be trait-related
  ### dest_path: the path you hope to store the downloaded metadata .xlsx files,
  ###            the function will generate folders for the downloaded files

  data<-read.csv(summary_file) ### read data
  data[,1]<-sub(" .*","",data[,1]) ### only keep the characters with "PGSXXXXXX" before the space
  trait_column<-select.list(colnames(data)[grepl("Trait",colnames(data))],title="Choose the column for trait")
  trait_choices<-unique(data[,trait_column]) ### extract unique values for the trait column
  if(length(trait_choices)==0) stop("No traits in the column") ### stop if the column has no values
  selected_trait<-select.list(trait_choices,multiple=TRUE,title="Choose one or more traits:") ### pop-up to choose suitable traits
  if(length(selected_trait)==0) stop("No traits selected") ### stop if no traits selected
  pgsid<-data[data[[trait_column]] %in% selected_trait,1] ### extract pgsid and put them together as a link
  metadata_xlsx_link<-paste0("https://ftp.ebi.ac.uk/pub/databases/spot/pgs/scores/",
                             pgsid,
                             "/Metadata/",
                             pgsid,
                             "_metadata.xlsx")
  
  ### download start
  dest_dir<-paste(sub("/$","",dest_path),paste(selected_trait,collapse = " and "),sep="/") # set directory for destination
  if(!dir.exists(dest_dir)){### check folder and create one if there is no such folder
    dir.create(dest_dir)
    cat("Directory created:",dest_dir,"\n")
  } else {
    cat("Directory already exists:",dest_dir,"\n")
  }
  for(i in metadata_xlsx_link){ ### download 
    filename<-basename(i)
    download.file(i,
                  destfile = paste0(dest_dir,"/",filename),
                  mode="wb")
  }
}
