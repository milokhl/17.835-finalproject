# Run PCA on the matched dataset.

FINAL_DATA_DIR = '../data/final'
data.full = read.csv(file.path(FINAL_DATA_DIR, 'data_matched_all.csv'))

keep_columns = colnames(data.full)[5:31]
keep_columns = keep_columns[keep_columns != 'SP.POP.GROW' & keep_columns != 'SP.POP.TOTL']
data.relevant = data.full[, keep_columns]
data.relevant.scaled = scale(data.relevant)
model.pca = prcomp(as.matrix(data.relevant.scaled))

# Examine PCA fit.
model.pca$sdev ## standard deviation of each dimension, model.pca$sdev^2 is similar to res.svd$d^2 divided by (n-1).
model.pca$rotation ## similar to res.svd$v [below]
model.pca$center ## mean of each variable, used to center the variables before calculating the scores
model.pca$scale ## will be FALSE here since we didn't standardized the variables - only centered them. 
model.pca$x ## PC scores  - so when you use prcomp you don't need to do the matrix multiplication X%*%v[, 1] as in svd. 

# Summary.
summary(model.pca)

# Screeplot.
screeplot(model.pca)

# Manual screeplot.
## To find the variance we first need to find the square of  model.pca$sdev. 
plot(model.pca$sdev^2/sum(model.pca$sdev^2), 
  type = "b", pch = 16,
  xlab = "Principal Component #", 
  ylab = "Fraction of Variance Explained"
)
