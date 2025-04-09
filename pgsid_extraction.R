################### Breast Cancer PGSID Extraction ######################
data<-read.csv("/Users/midicole/Library/CloudStorage/OneDrive-TheUniversityofMelbourne/Documents/UoM/PRS/PGSCatalog/breastcancer_pgs_summary.csv")
data[,1]<-sub(" .*","",data[,1])
bc_only_data<-data[data$Mapped.Trait.s...Ontology.=="breast carcinoma",]
bc_only_data$metadata_link<-paste0("https://ftp.ebi.ac.uk/pub/databases/spot/pgs/scores/",bc_only_data[,1],"/Metadata/",bc_only_data[,1],"_metadata.tar.gz")
https://ftp.ebi.ac.uk/pub/databases/spot/pgs/scores/PGS000001/Metadata/PGS000001_metadata.xlsx
https://ftp.ebi.ac.uk/pub/databases/spot/pgs/scores/PGS004581/Metadata/PGS004581_metadata.xlsx

####### Download metadata for overall breast cancer from PGScatalog
for(i in bc_only_data$metadata_link){
  filename<-basename(i)
  ### download metadata.tar.gz
  download.file(i,destfile=paste0("/Users/midicole/Library/CloudStorage/OneDrive-TheUniversityofMelbourne/Documents/UoM/PRS/PGSCatalog/metadata/breast_cancer/",filename),mode="wb")
  print(paste("Download",filename))
  
  ### unzip .tar.gz
  untar(paste0("/Users/midicole/Library/CloudStorage/OneDrive-TheUniversityofMelbourne/Documents/UoM/PRS/PGSCatalog/metadata/breast_cancer/",filename),exdir=sub(".tar.gz","",paste0("/Users/midicole/Library/CloudStorage/OneDrive-TheUniversityofMelbourne/Documents/UoM/PRS/PGSCatalog/metadata/breast_cancer/",filename)))
  print(paste("Unzipped:",filename))
  
  ### delete raw .tar.gz
  file.remove(paste0("/Users/midicole/Library/CloudStorage/OneDrive-TheUniversityofMelbourne/Documents/UoM/PRS/PGSCatalog/metadata/breast_cancer/",filename))
  print(paste("Removed:",filename))
}

####### extract cohorts separately from "Score Development Samples" and "Evaluation Sample Sets"
library(readxl)
library(dplyr)
### extract all xlsx files in the path
all_files<-list.files(path="/Users/midicole/Library/CloudStorage/OneDrive-TheUniversityofMelbourne/Documents/UoM/PRS/PGSCatalog/metadata/breast_cancer/",
                      pattern="\\.xlsx?$",
                      recursive = TRUE,
                      full.names = TRUE)
### extract column from specific sheet
### the column name can be Cohort(s)
### the sheet name can be "Score Development Samples(SDS)" or "Evaluation Sample Sets(ESS)"
extract_column_from_sheet<-function(file_location,column_name,sheet_name){
  data<-read_excel(file_location,sheet=sheet_name)
  if (column_name %in% colnames(data)){
    output_vector<-as.vector(data[[column_name]])
  } else{
    warning(paste("Column",column_name,"not found in",basename(file_location)))
  }
  output_vector<-unique(unlist(strsplit(paste(output_vector,collapse = "|"),"\\|")))
  return(output_vector)
}

### conduct on SDS
list_SDS<-lapply(all_files, 
                 function(x) extract_column_from_sheet(x,column_name="Cohort(s)",sheet_name="Score Development Samples"))
max_len<-max(sapply(list_SDS,length))
padded_vectors <- lapply(list_SDS, function(x) {
  length(x) <- max_len  # ×Ô¶¯²¹NA
  return(x)
})
df_SDS<-as.data.frame(padded_vectors, col.names = sub("_.*","",basename(all_files)))
df_SDS[df_SDS=="NA"]<-NA

### conduct on ESS
list_ESS<-lapply(all_files, 
                 function(x) extract_column_from_sheet(x,column_name="Cohort(s)",sheet_name="Evaluation Sample Sets"))
max_len_ESS<-max(sapply(list_ESS,length))
padded_vectors_ESS <- lapply(list_ESS, function(x) {
  length(x) <- max_len_ESS  #
  return(x)
})
df_ESS<-as.data.frame(padded_vectors_ESS, col.names = sub("_.*","",basename(all_files)))
df_ESS[df_ESS=="NA"]<-NA


### find PRSs that are not using UKB as development
pgs_list_ukb<-na.omit(colnames(df_SDS)[sapply(df_SDS, function(col) any(col=="UKB"))])
pgs_list_not_ukb<-colnames(df_SDS)[!colnames(df_SDS) %in% pgs_list_ukb]
print(pgs_list_not_ukb)


