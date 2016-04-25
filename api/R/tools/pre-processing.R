### Separate antidepressant from data set (one time process)
# known antidepressant PubChem CID
antidepressant_list <- c(6434118, 76968116, 76962653, 76960151, 76956911, 73416972, 73416962, 73416955, 73416810, 73416655, 23663953, 15893898, 9884029, 6603149, 6434754, 6433351, 6420022, 6419921, 5353833, 5284550, 5282426, 5282425, 5282318, 5281088, 3045275, 667477, 667468, 443945, 441358, 439280, 198375, 171003, 146571, 146570, 121249, 101726, 71587, 71478, 71424, 68870, 68551, 68539, 65700, 65327, 62884, 62857, 34870, 34869, 33611, 25382, 25381, 22576, 21722, 21087, 21086, 11065, 9419, 9417, 8228, 6305, 5666, 5584, 5355, 5092, 4976, 4543, 4205, 4184, 4011, 3947, 3696, 3386, 3158, 2995, 2895, 2801, 2771, 2160, 444, 144)

antidepressant_idx <- which(rownames(chemmat) %in% antidepressant_list)
antidepressant <- chemmat[antidepressant_idx, ]
write.csv(antidepressant_data, "antidepressant-chem.csv")
without_antidepressant <- chemmat[-antidepressant_idx,]
write.csv(without_antidepressant, "without-antidepressant-chem.csv")
###

### Change label factor from numeric to character (one time process)
without_antidepressant_bio <- as.data.frame(apply(without_antidepressant_bio, 2, factor, levels = c(0, 1), label = c("no", "yes")))
###

### Data loading
# build up chemical fingerprints data set by using Package ChemmineR and sdf files downloaded from PubChem database
# [PubChem download page]
# https://pubchem.ncbi.nlm.nih.gov/pc_fetch/pc_fetch.cgi
library(ChemmineR)
sdfset <- read.SDFset("compounds-with-side-effects-information (SDF).sdf")
# Warning message:
# In read.SDFset("compounds-with-side-effects-information (SDF).sdf") :
#   24 invalid SDFs detected. To fix, run: valid <- validSDF(sdfset); sdfset <- sdfset[valid]
valid <- validSDF(sdfset)
sdfset <- sdfset[valid]
cid(sdfset) <- sdfid(sdfset)
fpset <- fp2bit(sdfset)
tmp <- as.matrix(fpset)

# build up phenotypic data set from files downloaded from DrugBank and Sider databases
# [Sider link]
# http://sideeffects.embl.de/media/download/meddra_all_se.tsv.gz
tmp <- read.csv("sider-all-se.csv")
# remove PubChem CID prefix "CID0...0"
tmp[, 2] <- apply(tmp[, 2, drop = FALSE], 2, function(x) { gsub("CID0+", "", x) })
tmp[, 2] <- as.numeric(tmp[, 2])
# remove duplicate side effects and sort data in alphabetical order for later use as column names
col_name <- sort(unique(tmp[, 6]))
pheno <- as.data.frame(matrix(0, nrow = nrow(chem), ncol = length(col_name)))
row.names(pheno) <- rownames(chem)
colnames(pheno) <- pheno_name

for(i in 1:nrow(tmp)) {
  row_index <- which(rownames(pheno) == tmp[i, 2])
  col_index <- which(colnames(pheno) == tmp[i, 6])
  # ensure side effect associated PubChem CID exists in chemical fingerprints data set
  if(length(row_index) != 0)
    pheno[row_index, col_index] <- 1
}

# build up bio data set from files downloaded from DrugBank
# [DrugBank links]
# http://www.drugbank.ca/releases/4-5-0/downloads/all-drug-links
# http://www.drugbank.ca/releases/4-5-0/downloads/target-all-polypeptide-ids
# http://www.drugbank.ca/releases/4-5-0/downloads/enzyme-all-polypeptide-ids
# http://www.drugbank.ca/releases/4-5-0/downloads/carrier-all-polypeptide-ids
# http://www.drugbank.ca/releases/4-5-0/downloads/transporter-all-polypeptide-ids

# tmp[, 7]   : Uniprot.Title
# tmp[, 13]  : DrugBank ID
# links[, 1] : DrugBank ID
# links[, 7] : PubChem CID

# manually combine all the data sets into drugbank-all-bio.csv in excel
tmp <- read.csv("drugbank-all-bio.csv")
links <- read.csv("drugbank-all-drug-links.csv")
# remove duplicate Uniprot title and sort data in alphabetical order for later use as column names
col_name <- sort(unique(tmp[, 7]))
bio <- as.data.frame(matrix(0, nrow = nrow(chem), ncol = length(col_name)))
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
###

### Data cleaning
# remove features with all 0s (no information gain)
bio_cleaned <- bio[, -which(colSums(bio) == 0)]
chem_cleaned <- chem[, -which(colSums(chem) == 0)]
pheno_cleaned <- pheno[, -which(colSums(pheno) == 0)]

# remove compunds with all 0s
# get the intersected index of all the data sets
# > which(rowSums(chem) == 0)
# named integer(0)
# > which(rowSums(pheno) == 0)
# named integer(0)
# > length(which(rowSums(bio) == 0))
# [1] 713
removed_index <- which(rowSums(bio) == 0)
bio_cleaned <- bio_cleaned[-removed_index,]
chem_cleaned <- chem_cleaned[-removed_index,]
pheno_cleaned <- pheno_cleaned[-removed_index,]
###
