# Computing on a data matrix
gene1 = c(1.00, 1.50, 1.25)
gene1

gene2 = c(1.35, 1.55, 1.00)
gene3 = c(-1.10, -1.50, -1.25)
gene4 = c(-1.20, -1.30, -1.00)
genes = c(gene1, gene2, gene3, gene4)
genes

rcnames = list(c("gene1", "gene2", "gene3", "gene4"),
               c("Eric", "Peter", "Anna"))
rcnames

geneData = matrix(genes, nrow=4, ncol=3, byrow=TRUE,
                  dimnames=rcnames)
geneData

apply(geneData, 2, mean)
apply(geneData, 1, mean)
meanExpressions = apply(geneData, 1, mean)

o = order(meanExpressions, decreasing=TRUE)
o
geneData[o, ]

geneData[c(1,2), ]
geneData[c("gene1", "gene2"), ]
meanExpressions > 0
geneData[meanExpressions > 0, ]

class(geneData)

# Constructing a data frame
patients.df = data.frame(
  patientID = c("101", "102", "103", "104"),
  treatment = c("drug", "placebo", "drug", "placebo"),
  age = c(20, 30, 24, 22)
)
patients.df

nrow(patients.df)
ncol(patients.df)
class(patients.df)

# Referencing column vectors in a data frame
patients.df[[2]]
patients.df[["treatment"]]
patients.df$treatment
patients.df[, 2]

patients.df[c(1,3), ]
patients.df[patients.df$treatment=="drug", ]
subset(patients.df, treatment=="drug")

row.names(patients.df) = patients.df$patientID
patients.df[c("101", "103"), ]

patients.df[2]
patients.df["treatment"]

# Constructing a list
list1 = list(
  c("p53", "p63", "p73"),
  matrix(rnorm(10), nrow=2),
  c(TRUE, FALSE, TRUE, FALSE, FALSE)
)
list1

p53FamilyGenes = c("p53", "p63", "p73")
list2 = list(
  genes = p53FamilyGenes,
  geneExpression = matrix(rnorm(10), nrow=2),
  remission = c(TRUE, FALSE, TRUE, FALSE, FALSE)
)

# Subsetting or slicing of a list
list2[[1]]
list2[[1]][1] = "her2"
list2[[1]]

p53FamilyGenes

list2[["genes"]]
list2$genes

# Data path attachment
attach(list2)
genes # global variable already exist.
detach(list2)

attach(patients.df)
treatment
detach(patients.df)

genes = c("p53", "p63", "p73")
attach(list2)
genes
remission
detach(list2)

# Install Packages from Bioconductor
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install()

BiocManager::install("ALL")
library(ALL)

# ALL Data Example
data(ALL)
?ALL
ALL
str(ALL)
class(ALL) # "ExpressionSet"

experimentData(ALL)
sampleNames(ALL)
featureNames(ALL)

dim(exprs(ALL))
exprs(ALL)[1:10, 1:5]

varLabels(ALL)
ALL$sex
ALL$age
ALL$mol.biol

ALL$BT
table(ALL$BT) # unique values count!

BT = ALL$BT %in% c("T", "T1", "T2", "T3", "T4")
y = factor(as.numeric(BT), labels=c("B", "T")) # boolean to string
table(y)

x = t(exprs(ALL))
dim(x)
summary(as.numeric(x))
hist(x, col="orange", nclass=50, main="Expression Data") # nclass = num of bars

x2 = scale(x) # Standard Scailing
dim(x2)
summary(as.numeric(x2))
hist(x2, col="orange", nclass=50, main="Scaled Expression Data")

# golub Data Example
BiocManager::install("multtest")
library(multtest)
data(golub)
class(golub) # "matrix" "array" 

## Acute Lymphoblastic Leukemia (ALL)
## Acute Myeloid Leukemia (AML)
class(golub.cl)
length(golub.cl)
golub.cl # ALL is indicated by 0 and AML by 1

## columns : gene index, ID, Name
class(golub.gnames)
dim(golub.gnames)
head(golub.gnames)

class(golub)
dim(golub)
golub[1:10, 1:5]

## We first focus "M92287_at"
golub.gnames[1042, ]
golub[1042, ]

golubFactor = factor(golub.cl, levels=0:1, labels=c("ALL", "AML"))
golubFactor
golubFactor == "ALL"
golub[1042, golubFactor=="ALL"]

meanALL = apply(golub[ , golubFactor=="ALL"], 1, mean)
summary(meanALL)

meanAML = apply(golub[ , golubFactor=="AML"], 1, mean)
summary(meanAML)

par(mfrow=c(1,2))
hist(meanALL, col="orange", ncalss=50, main="ALL")
hist(meanAML, col="purple", nclass=50, main="AML")

ALL = as.numeric(golub[, golubFactor=="ALL"])
AML = as.numeric(golub[, golubFactor=="AML"])

par(mfrow=c(1,2))
hist(ALL, col="orange", nclass=50, main="ALL")
hist(AML, col="purple", nclass=50, main="AML")

## We are interested in the properties of certain genes (ex : CD33)
cd33 = grep("CD33", golub.gnames[, 2], ignore.case=TRUE) # text search
cd33
golub[cd33, ]
golub.gnames[cd33, ]

ccnd3 = grep("CCND3", golub.gnames[, 2], ignore.case=TRUE)
ccnd3
golub.gnames[ccnd3, ]

# FAMuSS Study
fmsURL = "https://raw.githubusercontent.com/statsun78/kogo/master/data/FMS_data.txt"
fms = read.delim(file=fmsURL, header=TRUE, sep="\t", fileEncoding = "CP949", encoding = "UTF-8")

dim(fms)
str(fms)

attach(fms)
actn3 = factor(actn3_rs540874)
table(actn3)
str(actn3)
par(mfrow=c(1,1))
plot(actn3)

GenoCount = summary(actn3)
GenoCount
NumbObs <- sum(!is.na(actn3))
GenoCount[1:3]/NumbObs

GenoFreq <- as.vector(GenoCount[1:3]/NumbObs)
FreqC <- (2*GenoFreq[1] + GenoFreq[2])/2
FreqT <- (GenoFreq[2] + 2*GenoFreq[3])/2
c(FreqC, FreqT)

install.packages("genetics")
library(genetics)

Geno <- genotype(actn3, sep="")
summary(Geno)

summary(Geno)$allele.freq
min(summary(Geno)$allele.freq[1:2, 2])

fmsSNP <- fms[ ,3:226]
miss <- apply(fmsSNP, 2, function(t) mean(is.na(t)))
range(miss)
sum(miss < 0.5)

data <- fmsSNP[, miss < 0.5]
MAF <- NULL # Minor Allele Frequency

for (i in 1:ncol(data)) {
  geno <- genotype(data[,i], sep="")
  MAF[i] <- min(summary(geno)$allele.freq[1:2, 2])
}

MAF
summary(MAF)
hist(MAF, nclass=20, col="orange") 
