library(glmnet) # For LASSO.

FINAL_DATA_DIR = '../data/final'

# Partition the dataset.
data.full = read.csv(file.path(FINAL_DATA_DIR, 'data_matched_all.csv'))
summary(data.full$stability_index_stderr)

train_pct = 0.75
num_train = round(train_pct * dim(data.full)[1])

train_idx = sample(dim(data.full)[1], num_train, replace = FALSE)

data.train = data.full[train_idx,]
data.test = data.full[-train_idx,]

data.train.labels = as.numeric(data.train$stability_index_estimate)
data.test.labels = as.numeric(data.test$stability_index_estimate)

# Only care about the features related to food security.
feature_columns = colnames(data.train)[5:30]
data.train.features = data.train[, feature_columns]
data.test.features = data.test[, feature_columns]

# Note: LASSO automatically standardizes variables.
model.lasso = cv.glmnet(
  y = as.matrix(data.train.labels), x = as.matrix(data.train.features),
  alpha = 1, type.measure = 'mse', standardize=T
)

# Get the LASSO prediction error on training set.
model.train.predictions = predict(model.lasso, newx = as.matrix(data.train.features),
                                  s = model.lasso$lambda.min, type='response')
model.train.mse = mean((model.train.predictions - data.train.labels)^2)
model.train.stderr = mean(abs(model.train.predictions - data.train.labels))

# Get the LASSO prediction error on the test set.
model.test.predictions = predict(model.lasso, newx = as.matrix(data.test.features),
                                 s = model.lasso$lambda.min, type='response')
model.test.mse = mean((model.test.predictions - data.test.labels)^2)
model.test.stderr = mean(abs(model.test.predictions - data.test.labels))

# Investigate coefficients.
model.lasso.coef = coef(model.lasso)
print(model.lasso.coef)

