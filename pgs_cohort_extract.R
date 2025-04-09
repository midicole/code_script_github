###
folder_path<-"/Users/midicole/Library/CloudStorage/OneDrive-TheUniversityofMelbourne/Documents/UoM/PRS/PGSCatalog/metadata/breast carcinoma/"
  
  
pgs_cohort_extract<-function(folder_path,sheet_name){
  ### folder_path: the path of folder containing all metadata files with quotation marks
  ### sheet_name: the name of sheet with quotation marks, options can be either 
  ### "Score Development Samples" or "Evaluation Sample Sets"
  
  
  library(readxl)
  ### list all files in a specific path
  all_files<-list.files(path=folder_path,
                        full.names = TRUE) 
  ### check if all files in the folder are metadata.xlsx
  if(all(grepl(".xlsx",all_files))!=TRUE) stop("The folder includes unqualified files") 
  ### extract all sheet names for each .xlsx files
  sheet_name_list<-list()
  for(i in 1:length(all_files)){
    sheet_name_list[[i]]<-excel_sheets(all_files[i])
  }
  ### check if there is any unseen sheet name for these files
  if(all(sapply(sheet_name_list, function(x) identical(x,sheet_name_list[[1]])))!=TRUE) 
    stop("The sheet names may have been updated, contact author to update the function!")
  
  ## extract the specific sheets from all .xlsx files and store them as a list
  ### create temporary lists for datasets and cohorts
  temp_data<-list()
  temp_cohort<-list()
  ### extract data and save to the list
  for(i in 1:length(all_files)){
    temp_data[[sub("_.*","",basename(all_files)[i])]]<-read_excel(all_files[i],sheet=sheet_name)
  }
  
  ### stop if there is no such column called "Cohort(s)" in the data
  check_list<-list()
  for(i in 1:length(all_files)){
    check_list[[sub("_.*","",basename(all_files)[i])]]<-if("Cohort(s)" %in% colnames(temp_data[[i]])) TRUE else FALSE
  }
  if(all(unlist(check_list))!=TRUE) 
    stop(paste("There is no column \"Cohort(s)\" in the data",
               names(check_list[check_list==FALSE]),
               ", contact author to update the function!"))
  
  ### 
  for(i in 1:length(all_files)){
    temp_cohort[[sub("_.*","",basename(all_files)[i])]]<-unique(unlist(strsplit(paste(unlist(temp_data[[i]][,"Cohort(s)"]),collapse = "|"),"\\|")))
  }
  max_len<-max(sapply(temp_cohort,length))
  padded_vectors<-lapply(temp_cohort, function(x){
    length(x)<-max_len
    return(x)
  })
  output_df<-as.data.frame(padded_vectors,col.names = sub("_.*","",basename(all_files)))
  return(output_df)
}


