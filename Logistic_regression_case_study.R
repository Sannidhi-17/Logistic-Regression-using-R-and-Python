# Read the dataset
df <- read.csv('Social_Network_Ads.csv')
df = df[3:5]

# Encode the target variable as a factor
df$Purchased <- factor(df$Purchased, levels = c(0,1))
#df$Gender <- as.numeric(as.factor(df$Gender))

# Split the dataset nto the train and test set
library(caTools)
set.seed(101)

sample = sample.split(df$Purchased, SplitRatio = 0.80)
train = subset(df, sample == T)
test = subset(df, sample == F)

head(train)
# data pre-processing (Scaling of the features)
train[-3] = scale(train[-3])
test[-3] = scale(test[-3])

#create the model
model = glm(formula = Purchased ~.,family = binomial, data = train)
pred = predict(model, type = 'response', test)
y_pred = ifelse (pred > 0.5, 1, 0)

# To evaluate the model create the confusion matrix
cm = table(test[,3], y_pred >0.5)
cm
