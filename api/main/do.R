### Prepare data
source("./load.R")
source("./clean.R")
source("./func.R")

### Disable scientific notation
options(scipen = 999)

### Start parallel computing
cl <- makeCluster(detectCores())
registerDoParallel(cl)
