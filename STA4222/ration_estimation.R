library(survey)
source("c:/courses/st446/rcode/confintt.r")
# In Excel, save your spreadsheet as a text tab-delimited file
# If variable names are in row 1, then use header=T)
ratio <- read.table("c://courses/st446/Rcode/agsrs.txt", header = T)

N <- 3078 # population size
n <- 300 # sample size
tx <- 964470625 # X population total if known: for Case 3
mux <- tx / N # X population mean if known: for Case 2

ratio_ttl <- tx * ratio$ACRES92
ratio_mn <- ratio_ttl / N

mnhatx <- mean(ratio$ACRES87) # estimated X pop. mean if unknown: Case 4
thatx <- N * mnhatx # estimated X pop. total if unknown: Case 5
ratio_umn <- mnhatx * ratio$ACRES92
ratio_uttl <- N * ratio_umn

fpc <- c(rep(N, n))
ratio <- cbind(ratio, fpc, ratio_ttl, ratio_mn, ratio_uttl, ratio_umn)
ratio <- data.frame(ratio)

# Create the sampling design
agdsgn <- svydesign(data = ratio, id = ~1, fpc = ~fpc)

# Estimation of the ratio (Case 1)
agratio <- svyratio(~ACRES92, ~ACRES87, design = agdsgn)

confint.t(agratio, tdf = n - 1, level = .95)

# Estimation of the y population mean (when tx is known): Case 2
agratio_mean <- svyratio(~ratio_mn, ~ACRES87, design = agdsgn)
confint.t(agratio_mean, tdf = n - 1, level = .95)

# Estimation of the y population total (when tx is known): Case 3
agratio_total <- svyratio(~ratio_ttl, ~ACRES87, design = agdsgn)

confint.t(agratio_total, tdf = n - 1, level = .95)

# Estimation of the y population mean (when tx is unknown): Case 4
agratio_umean <- svyratio(~ratio_umn, ~ACRES87, design = agdsgn)

confint.t(agratio_umean, tdf = n - 1, level = .95)

# Estimation of the y population total (when tx is unknown): Case 5
agratio_utotal <- svyratio(~ratio_uttl, ~ACRES)