###### Clean PGS catalog CRC PRS file ####
load("/Users/midicole/Library/CloudStorage/OneDrive-TheUniversityofMelbourne/Documents/UoM/PRS/PGSCatalog/UKB_SNP_uniqueID.rda")

FlipAllele = function(x){
  if (x=="A") {
    return("T")
  } else if (x=="T") {
    return ("A")
  } else if (x=="C") {
    return ("G")
  } else if (x=="G"){
    return ("C")
  } else {
    return(x)
  }
}

### Function
WeightFileClean = function(filename="/Users/midicole/Library/CloudStorage/OneDrive-TheUniversityofMelbourne/Documents/UoM/PRS/PGSCatalog/PGS000052_hmPOS_GRCh37.txt",
                           skiplineno=19,ID=1,CHR=2,Pos=3,effect.allele=4,other.allele=5,effect.size=6)
{
  ### Read the file and skip the first 19 lines to jump to the formal content
  dat = read.table(file=filename,skip=skiplineno,header=T,sep="\t")
  ### assign number of SNPs using rows of data
  no.snp = nrow(dat)
  ### error check step
  print (paste("The total variant in the weight file is:",no.snp))
  ### check palindrom
  index.palindrome = which((dat[,effect.allele]=="A" & dat[,other.allele] =="T") |
                             (dat[,effect.allele]=="T" & dat[,other.allele] =="A") |
                             (dat[,effect.allele]=="C" & dat[,other.allele] =="G") |
                             (dat[,effect.allele]=="G" & dat[,other.allele] =="C"))
  print (paste("The total palindromic variant in the weight file is:",length(index.palindrome)))
  ### remove data that contains palindrome if exists
  if (length(index.palindrome) !=0 ) {
    dat = dat[-index.palindrome,]}
  
  ### check NA values in ID
  index.na = which(dat[,ID]=="")
  print (paste("The total variant in the weight file with a missing rsis is:",length(index.na)))
  ### remove data that contains NA in ID if exist
  if (length(index.na) !=0 ) {
    dat = dat[-index.na,] }
  ### check the amount of left variants
  no.snp.reduced = nrow(dat)
  print (paste("The total variant left in the weight file is:",no.snp.reduced))
### ID1 is testing for normal cases
### ID2 is testing for cases that the order of effect and other allele is flipped
### ID3 is testing for plus/minus strand
### ID4 is testing for cases that the order of effect and other allele is flipped on the other strand if this is the case
  ID1 = paste(dat[,"chr_name"],dat[,"chr_position"],dat[,"effect_allele"],dat[,"other_allele"],sep=":")
  ID2 = paste(dat[,"chr_name"],dat[,"chr_position"],dat[,"other_allele"],dat[,"effect_allele"],sep=":")
  effect.allele.flip = sapply(dat[,"effect_allele"], function(x) FlipAllele(x))
  other.allele.flip = sapply(dat[,"other_allele"], function(x) FlipAllele(x))
  ID3 = paste(dat[,"chr_name"],dat[,"chr_position"],effect.allele.flip,other.allele.flip,sep=":")
  ID4 = paste(dat[,"chr_name"],dat[,"chr_position"],other.allele.flip,effect.allele.flip,sep=":")

### if index2 extracts qualified SNPs, then they can be ruled out for the next filter
  index2<-which(ID2 %in% UKB_SNP$V1)
  if (length(index2) !=0) {
    ID1[index2] <- ID2[index2]
  }

  ### if index3 extracts qualified SNPs, then they can be ruled out for the next filter
  index3=which(ID3 %in% UKB_SNP$V1)
  index3 = index3[!is.element(index3,index2)]
  if (length(index3) !=0) {
    ID1[index3] = ID3[index3]
    dat[index3,effect.allele] = effect.allele.flip[index3]
    dat[index3,other.allele] = other.allele.flip[index3]
  }
  
  index4=which(ID4 %in% UKB_SNP$V1)
  index4 = index4[!is.element(index4,index2) & !is.element(index4,index3)]
  if (length(index4) !=0) {
    ID1[index4] = ID4[index4]
    dat[index4,effect.allele] = effect.allele.flip[index4]
    dat[index4,other.allele] = other.allele.flip[index4]
  }
  
  match.snp = sum(ID1 %in% UKB_SNP$V1)
  
  print (paste("The matched number of variant is:",match.snp, "and the proportion is:",match.snp/no.snp.reduced))
  
  outfile = paste("CLEANED",filename,sep="_")
  write.table(dat[,c(ID,CHR,Pos,effect.allele,other.allele,effect.size)],file="/Users/midicole/Library/CloudStorage/OneDrive-TheUniversityofMelbourne/Documents/UoM/PRS/PGSCatalog/Cleaned_PGS000052_hmPOS_GRCh37.txt",sep="\t",row.names = F,col.names = T,quote = F)
}


WeightFileClean("PGS002265_hmPOS_GRCh37.txt",19,1,2,3,4,12,5)
WeightFileClean("PGS003851_hmPOS_GRCh37.txt",19,1,2,3,4,5,6)
