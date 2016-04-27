### Load required library
if(!require("doParallel"))
  install.packages("doParallel")
if(!require("DMwR"))
  install.packages("DMwR")
if(!require("caret"))
  install.packages("caret")
if(!require("PMA"))
  install.packages("PMA")
if(!require("CCA"))
  install.packages("CCA")
if(!require("ROCR"))
  install.packages("ROCR")
if(!require("ggplot2"))
  install.packages("ggplot2")
if(!require("RColorBrewer"))
  install.packages("RColorBrewer")

### Build up chemical fingerprints data set by using Package ChemmineR and sdf files downloaded from PubChem database
### Pre-extracted a list of PubChem compounds CID with side effects information in file: "compounds-with-side-effects-information (PubChemCID).csv"
### [PubChem download page]
### https://pubchem.ncbi.nlm.nih.gov/pc_fetch/pc_fetch.cgi
if(!exists("chem")) {
  if(file.exists("../data/chem.csv"))
    chem <- as.matrix(read.csv("../data/chem.csv", row.names = 1, check.names = F))
  else if(file.exists("../data/compounds-with-side-effects-information (SDF).sdf")) {
    library(ChemmineR)
    sdfset <- read.SDFset("../data/compounds-with-side-effects-information (SDF).sdf")
    # Warning message:
    # In read.SDFset("compounds-with-side-effects-information (SDF).sdf") :
    #   24 invalid SDFs detected. To fix, run: valid <- validSDF(sdfset); sdfset <- sdfset[valid]
    valid <- validSDF(sdfset)
    sdfset <- sdfset[valid]
    cid(sdfset) <- sdfid(sdfset)
    fpset <- fp2bit(sdfset)
    chem <- as.matrix(fpset)
  } else
    print("Error: couldn't find the required SDF file: compounds-with-side-effects-information (SDF).sdf")
}

### Build up phenotypic data set from file downloaded from SIDER databases
### [SIDER link]
### http://sideeffects.embl.de/media/download/meddra_all_se.tsv.gz
if(!exists("pheno")) {
  if(file.exists("../data/pheno.csv"))
    pheno <- as.matrix(read.csv("../data/pheno.csv", row.names = 1, check.names = F))
  else if(file.exists("../data/sider-all-se.csv")) {
    tmp <- read.csv("../data/sider-all-se.csv", header = FALSE)
    # remove PubChem CID prefix "CID0...0"
    tmp[, 2] <- apply(tmp[, 2, drop = FALSE], 2, function(x) { gsub("CID0+", "", x) })
    tmp[, 2] <- as.numeric(tmp[, 2])
    # remove duplicate side effects and sort data in alphabetical order for later use as column names
    col_name <- sort(unique(tmp[, 6]))
    pheno <- matrix(0, nrow = nrow(chem), ncol = length(col_name))
    rownames(pheno) <- rownames(chem)
    colnames(pheno) <- col_name

    for(i in 1:nrow(tmp)) {
      row_index <- which(rownames(pheno) == tmp[i, 2])
      col_index <- which(colnames(pheno) == tmp[i, 6])
      # ensure side effect associated PubChem CID exists in chemical fingerprints data set
      if(length(row_index) != 0)
        pheno[row_index, col_index] <- 1
    }
  } else
    print("Error: couldn't find the required file: sider-all-se.csv")
}
### Build up bio data set from files downloaded from DrugBank
### [DrugBank links]
### http://www.drugbank.ca/releases/4-5-0/downloads/all-drug-links
### http://www.drugbank.ca/releases/4-5-0/downloads/target-all-polypeptide-ids
### http://www.drugbank.ca/releases/4-5-0/downloads/enzyme-all-polypeptide-ids
### http://www.drugbank.ca/releases/4-5-0/downloads/carrier-all-polypeptide-ids
### http://www.drugbank.ca/releases/4-5-0/downloads/transporter-all-polypeptide-ids
# tmp[, 7]   : Uniprot.Title
# tmp[, 13]  : DrugBank ID
# links[, 1] : DrugBank ID
# links[, 7] : PubChem CID
# manually combine all the data sets into drugbank-all-bio.csv in excel
if(!exists("bio")) {
  if(file.exists("../data/bio.csv"))
    bio <- as.matrix(read.csv("../data/bio.csv", row.names = 1, check.names = F))
  else if(file.exists("../data/drugbank-all-bio (combine target, transporter, carrier and enzyme).csv") &&
    file.exists("../data/drugbank-all-drug-links.csv")) {
    tmp <- read.csv("../data/drugbank-all-bio (combine target, transporter, carrier and enzyme).csv")
    links <- read.csv("../data/drugbank-all-drug-links.csv")
    # remove duplicate Uniprot title and sort data in alphabetical order for later use as column names
    col_name <- sort(unique(tmp[, 7]))
    bio <- matrix(0, nrow = nrow(chem), ncol = length(col_name))
    rownames(bio) <- rownames(chem)
    colnames(bio) <- col_name

    for(i in 1:nrow(tmp)) {
      col_index <- which(colnames(bio) == tmp[i, 7])
      # split string of DrugBank IDs into vector
      drugbank_ids <- unlist(strsplit(as.character(tmp[i, 13]), ";."))
      for(j in 1:length(drugbank_ids)) {
        # find PubChem CID associated with DrugBank ID
        links_row_index <- which(links[, 1] == drugbank_ids[j])
        pubchem_cid <- links[links_row_index, 7]
        row_index <- which(rownames(bio) == pubchem_cid)
        # ensure bio associated PubChem CID exists in chemical fingerprints data set
        if(length(row_index) != 0)
          bio[row_index, col_index] <- 1
      }
    }
  } else
    print("Error: couldn't find the required file(s): drugbank-all-bio (combine target, transporter, carrier and enzyme).csv,
          drugbank-all-drug-links.csv")
}
