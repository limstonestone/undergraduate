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