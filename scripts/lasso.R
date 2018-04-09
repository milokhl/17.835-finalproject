library(glmnet) # For LASSO.

FINAL_DATA_DIR = '../data/final'

# Partition the dataset.
data.full = read.csv(file.path(FINAL_DATA_DIR, 'data_matched_all.csv'))

NUM_NA = data.frame(matrix(ncol=37, nrow=1))
for (cname in colnames(data.full)) {
  NUM_NA[, cname] = sum(is.na(data.full[, cname]))
}

data.clean = data.full[, c(cols_to_use, 'stability_index_estimate')]
data.clean = data.clean[complete.cases(data.clean),]

cols_to_use = c('SN.ITK.DEFC.ZS', 'SH.H2O.SAFE.ZS', 'SN.ITK.DFCT', 'AG.PRD.FOOD.XD')
# SN.ITK.DEFC.ZS = pct pop undernourished
# SH.H2O.SAFE.ZS = pct access to safe water
# SN.ITK.DFCT = kilocalorie deficit
# AG.PRD.FOOD.XD

train_pct = 0.75
num_train = round(train_pct * dim(data.clean)[1])
train_idx = sample(dim(data.clean)[1], num_train, replace = FALSE)

data.train = data.clean[train_idx, cols_to_use]
data.test = data.clean[-train_idx, cols_to_use]

data.train.labels = as.numeric(data.clean[train_idx,]$stability_index_estimate)
data.test.labels = as.numeric(data.clean[-train_idx,]$stability_index_estimate)

data.train.features = data.train[, cols_to_use]
data.test.features = data.test[, cols_to_use]

# Note: LASSO automatically standardizes variables.
model.lasso = cv.glmnet(
  y = as.matrix(data.train.labels), x = as.matrix(data.train.features),
  alpha = 1, type.measure = 'mse', standardize=F
)

# Get the LASSO prediction error on training set.
model.train.predictions = predict(model.lasso, newx = as.matrix(data.train.features),
                                  s = model.lasso$lambda.min, type='response')
model.train.mse = mean((model.train.predictions - data.train.labels)^2)
model.train.stderr = mean(abs(model.train.predictions - data.train.labels))
model.train.stability_class = (model.train.predictions >= 0)
data.train.stability_class = (data.train.labels >= 0)
model.train.correct = (data.train.stability_class == model.train.stability_class)
model.train.binary_accuracy = sum(model.train.correct == TRUE) / length(model.train.predictions)

# Get the LASSO prediction error on the test set.
model.test.predictions = predict(model.lasso, newx = as.matrix(data.test.features),
                                 s = model.lasso$lambda.min, type='response')
model.test.mse = mean((model.test.predictions - data.test.labels)^2)
model.test.stderr = mean(abs(model.test.predictions - data.test.labels))
model.test.stability_class = (model.test.predictions >= 0)
data.test.stability_class = (data.test.labels >= 0)
model.test.correct = (data.test.stability_class == model.test.stability_class)
model.test.binary_accuracy = sum(model.test.correct == TRUE) / length(model.test.predictions)

# Investigate coefficients.
model.lasso.coef = coef(model.lasso)
print(model.lasso.coef)

