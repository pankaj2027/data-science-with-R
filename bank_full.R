bank <- read.csv(file.choose())
summary(bank))
str(bank)
head(bank,3)

dim(bank)
colnames(bank)                   

##GLM function use sigmoid curve to produce desirable results 
# The output of sigmoid function lies in between 0-1
model <- glm(bank~.,data=bank,family = "binomial")

