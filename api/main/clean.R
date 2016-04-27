### [Data Info - all compounds are joined by PubChem CID]
### bio  : 816 compounds * 984  features (targets + enzymes + carriers + transporters get from DrugBank)
### chem : 816 compounds * 653  features (fingerprints get from PubChem)
### pheno: 816 compounds * 6111 features (side effects get from Sider)

### Remove columns (features) with all 0s (no information gain)
### Remove rows (compounds) with all 0s
# get the intersected index of all the data sets
# > which(rowSums(chem) == 0)
# named integer(0)
# > which(rowSums(pheno) == 0)
# named integer(0)
# > length(which(rowSums(bio) == 0))
# [1] 713
removed_index <- which(rowSums(bio) == 0)
if(!exists("bio_cleaned")) {
  if(file.exists("../data/bio-cleaned.csv"))
    bio_cleaned <- as.matrix(read.csv("../data/bio-cleaned.csv", row.names = 1, check.names = F))
  else {
    # remove features
    bio_cleaned <- bio[, -which(colSums(bio) == 0)]
    # remove compounds
    bio_cleaned <- bio_cleaned[-removed_index,]
    write.csv(bio_cleaned, "../data/bio-cleaned.csv")
  }
}
if(!exists("chem_cleaned")) {
  if(file.exists("../data/chem-cleaned.csv"))
    chem_cleaned <- as.matrix(read.csv("../data/chem-cleaned.csv", row.names = 1, check.names = F))
  else {
    # remove features
    chem_cleaned <- chem[, -which(colSums(chem) == 0)]
    # remove compounds
    chem_cleaned <- chem_cleaned[-removed_index,]
    write.csv(chem_cleaned, "../data/chem-cleaned.csv")
  }
}
if(!exists("pheno_cleaned")) {
  if(file.exists("../data/pheno-cleaned.csv"))
    pheno_cleaned <- as.matrix(read.csv("../data/pheno-cleaned.csv", row.names = 1, check.names = F))
  else {
    # remove features
    pheno_cleaned <- pheno[, -which(colSums(pheno) == 0)]
    # remove compounds
    pheno_cleaned <- pheno_cleaned[-removed_index,]
    write.csv(pheno_cleaned, "../data/pheno-cleaned.csv")
  }
}

### Separate antidepressant from data set (one time process)
# known antidepressant PubChem CID crawled by python crawler
antidp_list <- c(6434118, 76968116, 76962653, 76960151, 76956911, 73416972, 73416962, 73416955, 73416810, 73416655, 23663953, 15893898, 9884029, 6603149, 6434754, 6433351, 6420022, 6419921, 5353833, 5284550, 5282426, 5282425, 5282318, 5281088, 3045275, 667477, 667468, 443945, 441358, 439280, 198375, 171003, 146571, 146570, 121249, 101726, 71587, 71478, 71424, 68870, 68551, 68539, 65700, 65327, 62884, 62857, 34870, 34869, 33611, 25382, 25381, 22576, 21722, 21087, 21086, 11065, 9419, 9417, 8228, 6305, 5666, 5584, 5355, 5092, 4976, 4543, 4205, 4184, 4011, 3947, 3696, 3386, 3158, 2995, 2895, 2801, 2771, 2160, 444, 144)

antidp_idx <- which(rownames(bio_cleaned) %in% antidp_list)
if(!exists("antidp_bio")) {
  if(file.exists("../data/antidp-bio.csv"))
    antidp_bio <- as.matrix(read.csv("../data/antidp-bio.csv", row.names = 1, check.names = F))
  else {
    antidp_bio <- bio_cleaned[antidp_idx, ]
    write.csv(antidp_bio, "../data/antidp-bio.csv")
  }
}
if(!exists("antidp_chem")) {
  if(file.exists("../data/antidp-chem.csv"))
    antidp_chem <- as.matrix(read.csv("../data/antidp-chem.csv", row.names = 1, check.names = F))
  else {
    antidp_chem <- chem_cleaned[antidp_idx, ]
    write.csv(antidp_chem, "../data/antidp-chem.csv")
  }
}
if(!exists("antidp_pheno")) {
  if(file.exists("../data/antidp-pheno.csv"))
    antidp_pheno <- as.matrix(read.csv("../data/antidp-pheno.csv", row.names = 1, check.names = F))
  else {
    antidp_pheno <- pheno_cleaned[antidp_idx, ]
    write.csv(antidp_pheno, "../data/antidp-pheno.csv")
  }
}
if(!exists("wo_antidp_bio")) {
  if(file.exists("../data/wo-antidp-bio.csv"))
    wo_antidp_bio <- as.matrix(read.csv("../data/wo-antidp-bio.csv", row.names = 1, check.names = F))
  else {
    wo_antidp_bio <- bio_cleaned[-antidp_idx,]
    write.csv(wo_antidp_bio, "../data/wo-antidp-bio.csv")
  }
}
if(!exists("wo_antidp_chem")) {
  if(file.exists("../data/wo-antidp-chem.csv"))
    wo_antidp_chem <- as.matrix(read.csv("../data/wo-antidp-chem.csv", row.names = 1, check.names = F))
  else {
    wo_antidp_chem <- chem_cleaned[-antidp_idx,]
    write.csv(wo_antidp_chem, "../data/wo-antidp-chem.csv")
  }
}
if(!exists("wo_antidp_pheno")) {
  if(file.exists("../data/wo-antidp-pheno.csv"))
    wo_antidp_pheno <- as.matrix(read.csv("../data/wo-antidp-pheno.csv", row.names = 1, check.names = F))
  else {
    wo_antidp_pheno <- pheno_cleaned[-antidp_idx,]
    write.csv(wo_antidp_pheno, "../data/wo-antidp-pheno.csv")
  }
}
