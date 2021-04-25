
library(readr)
library("dplyr")
loans <- read_csv('loans50k.csv')

loans.clean <- loans
loans.clean$loanID <- NULL
loans.clean$employment <- NULL
loans.clean$state <- NULL

loans.clean$term = as.factor(loans.clean$term)
loans.clean$grade = as.factor(loans.clean$grade)
loans.clean$length = as.factor(loans.clean$length)  
loans.clean$home = as.factor(loans.clean$home)
loans.clean$verified = as.factor(loans.clean$verified)
loans.clean$status = as.factor(loans.clean$status)

loans.clean <- loans.clean %>% mutate(reason = case_when(
  (reason == "credit_card" | reason == "debt_consolidation") ~ "Credit Related",
  TRUE ~ "Other"
))

loans.clean <- loans.clean %>% mutate(length = case_when(
  (length == "< 1 year" | length == "1 year") ~ "1 year or less",
  (length == "2 years" | length == "3 years" | length == "4 years" | length == "5 years") ~ "2 to 5 years",
  (length == "6 years" | length == "7 years" | length == "8 years" | length == "9 years") ~ "6 to 9 years",
  (length == "10 years" | length == "10+ years") ~ "10 or more years"
))

loans.clean$length[loans.clean$length =='n/a'] <- NA

loans.clean$length <- as.factor(loans.clean$length)
loans.clean$reason <- as.factor(loans.clean$reason)

loans.clean <- loans.clean %>% mutate(status = case_when(
  (status == "Fully Paid") ~ "Good",
  (status == "Charged Off" | status == "Default") ~ "Bad",
  TRUE ~ "Unknown"
))

loans.clean <- loans.clean[!loans.clean$status == "Unknown", ]
loans.clean$status <- as.factor(loans.clean$status)

summary(loans.clean)

#remove NAs. They are a small percentage of the data (less than 5%).
loans.clean <- na.omit(loans.clean)

#make histograms to evaluate skewness
par(mfrow=c(3,3))
hist(loans.clean$amount)
hist(loans.clean$rate)
hist(loans.clean$payment)
hist(loans.clean$income)
hist(loans.clean$debtIncRat)
hist(loans.clean$delinq2yr)
hist(loans.clean$inq6mth)
hist(loans.clean$openAcc)
hist(loans.clean$pubRec)

par(mfrow=c(3,3))
hist(loans.clean$revolRatio)
hist(loans.clean$totalAcc)
hist(loans.clean$totalBal)
hist(loans.clean$totalRevLim)
hist(loans.clean$accOpen24)
hist(loans.clean$avgBal)
hist(loans.clean$bcOpen)
hist(loans.clean$bcRatio)
hist(loans.clean$totalLim)

par(mfrow=c(3,3))
hist(loans.clean$totalRevBal)
hist(loans.clean$totalBcLim)
hist(loans.clean$totalIlLim)

par(mfrow=c(2,2))
hist(loans.clean$income, col="#56B4E9", main="Income (original)")
hist(log(loans.clean$income), col="#56B4E9", main="Income (log transformation)")
hist(loans.clean$totalBcLim, col="#009E73", main="totalBcLim (original)")
hist(sqrt(loans.clean$totalBcLim), col="#009E73", main="totalBcLim (square root transformation)")

par(mfrow=c(2,2))
boxplot(loans.clean$income)
boxplot(log(loans.clean$income))
boxplot(loans.clean$totalBcLim)
boxplot(log(loans.clean$totalBcLim))

loans.clean$income <- log(loans.clean$income)
loans.clean$totalBcLim <- sqrt(loans.clean$totalBcLim)

#calculate percentages for comparison in bar plots rather than count
grade_percent_df <- loans.clean %>%
  count(status,grade) %>%
  group_by(status) %>%
  mutate(percent = (n/sum(n))*100) %>%
  ungroup()

home_percent_df <- loans.clean %>%
  count(status,home) %>%
  group_by(status) %>%
  mutate(percent = (n/sum(n))*100) %>%
  ungroup()

reason_percent_df <- loans.clean %>%
  count(status,reason) %>%
  group_by(status) %>%
  mutate(percent = (n/sum(n))*100) %>%
  ungroup()

length_percent_df <- loans.clean %>%
  count(status,length) %>%
  group_by(status) %>%
  mutate(percent = (n/sum(n))*100) %>%
  ungroup()
  
library("ggplot2")
library("gridExtra")

p1 <- ggplot( data = grade_percent_df, aes( x = status, y=percent, fill = grade) ) + 
      geom_bar(stat="identity") + 
      xlab("Loan Status")

p2 <- ggplot( data = home_percent_df, aes( x = status, y=percent, fill = home) ) + 
      geom_bar(stat="identity") + 
      xlab("Loan Status")

p3 <- ggplot( data = reason_percent_df, aes( x = status, y=percent, fill = reason) ) + 
      geom_bar(stat="identity") + 
      xlab("Loan Status")

p4 <- ggplot( data = length_percent_df, aes( x = status, y=percent, fill = length) ) + 
      geom_bar(stat="identity") + 
      xlab("Loan Status")

#grid.arrange(p1, p2, p3, p4, nrow=2, top="Categorical Variable Distributions")
grid.arrange(p1, p2, p3, p4, nrow=2,
    top="Selected Categorical Variables")

#data exploration
p1 <- ggplot( data = loans.clean, aes( x = status, y= payment, fill=status) ) + 
      geom_boxplot() + 
      xlab("Status")

p2 <- ggplot( data = loans.clean, aes( x = status, y= income, fill=status) ) + 
      geom_boxplot() + 
      xlab("Status")

p3 <- ggplot( data = loans.clean, aes( x = status, y= debtIncRat, fill=status) ) + 
      geom_boxplot() + 
      xlab("Status")

p4 <- ggplot( data = loans.clean, aes( x = status, y= totalBcLim, fill=status) ) + 
      geom_boxplot() + 
      xlab("Status")

p5 <- ggplot( data = loans.clean, aes( x = status, y= revolRatio, fill=status) ) + 
      geom_boxplot() + 
      xlab("Status")

p6 <- ggplot( data = loans.clean, aes( x = status, y= amount, fill=status) ) + 
      geom_boxplot() + 
      xlab("Status")

grid.arrange(p1, p2, p3, p4, p5, p6, nrow=3)

grid.arrange(p3, p5, nrow=1,
    top="Selected Quantitative Variables")

set.seed(1234)

train.index <- sample(seq_len(nrow(loans.clean)), size = floor(0.8 * nrow(loans.clean)))
train.loans <- loans.clean[train.index, ]
test.loans <- loans.clean[-train.index, ]

train.loans$totalPaid <- NULL

fit.full <- glm(status~., data=train.loans, family="binomial")

summary(fit.full)
extractAIC(fit.full)

fit.forward <- glm(status~1,data=train.loans,family="binomial")
fit.forward <- step(fit.forward,scope=list(lower=fit.forward,upper=fit.full),direction="forward")

fit.backward <- step(fit.full,direction="backward")

predprob <- predict(fit.full, test.loans, type = "response") 
threshold <- 0.5
preds <- cut(predprob, breaks=c(-Inf, threshold, Inf), 
                labels=c("Bad", "Good"))


cTab <- table(test.loans$status, preds) 
addmargins(cTab)
p <- sum(diag(cTab)) / sum(cTab)  # compute the proportion of correct classifications
print(paste('Proportion correctly predicted = ', p)) 

predprob.backward <- predict(fit.backward, test.loans, type = "response") 
threshold <- 0.5
preds.backward <- cut(predprob.backward, breaks=c(-Inf, threshold, Inf), 
                labels=c("Bad", "Good"))

cTab.backward <- table(test.loans$status, preds.backward) 
addmargins(cTab.backward)
p.backward <- sum(diag(cTab.backward)) / sum(cTab.backward)
print(paste('Proportion correctly predicted = ', p.backward)) 

summary(fit.backward)

library('HH')
vifs <- vif(fit.backward)
vifs
max(vifs)

fit.backward2 <- glm(status ~ term + payment + grade + home + debtIncRat + 
    delinq2yr + inq6mth + openAcc + revolRatio + totalAcc + totalRevLim + 
    accOpen24 + avgBal + bcOpen + totalRevBal + totalBcLim + 
    totalIlLim, data=train.loans, family="binomial")
vifs <- vif(fit.backward2)
vifs

predprob.backward <- predict(fit.backward2, test.loans, type = "response") 
threshold <- 0.5
preds.backward <- cut(predprob.backward, breaks=c(-Inf, threshold, Inf), 
                labels=c("Bad", "Good"))

cTab.backward <- table(test.loans$status, preds.backward) 
addmargins(cTab.backward)
p.backward <- sum(diag(cTab.backward)) / sum(cTab.backward)
print(paste('Proportion correctly predicted = ', p.backward)) 

p.loop <- list()
bad.loop <- list()
good.loop <- list()

thresholds <- c(seq(0,1,.1)) 

for (x in thresholds)
{
  preds <- cut(predprob.backward, breaks=c(-Inf, x, Inf), 
                  labels=c("Bad", "Good"))
  cTab <- table(test.loans$status, preds) 
  addmargins(cTab.backward)
  p.loop <- append(p.loop,sum(diag(cTab)) / sum(cTab))
  bad.loop <- append(bad.loop,cTab[1,1]/(cTab[1,1] + cTab[1,2]))
  good.loop <- append(good.loop,cTab[2,2]/(cTab[2,1] + cTab[2,2]))
}
plot(thresholds,good.loop, col="#009900", main="Model Accuracy", xlab="Threshold",ylab="Accuracy")
points(thresholds,bad.loop, col="red")
lines(thresholds,good.loop, col="#009900")
lines(thresholds,bad.loop, col="red")
legend("left", lty=c(1,1),col=c("#009900", "red"), legend = c("Accuracy of Good Loans", "Accuracy of Bad Loans"))

plot(thresholds,p.loop, col="orange", main="Overall Model Accuracy", xlab="Threshold",ylab="Accuracy")
lines(thresholds,p.loop, col="orange")

test.loans$profit <- test.loans$totalPaid - test.loans$amount
profit.loop <- list()
for (x in thresholds)
{
  preds <- cut(predprob.backward, breaks=c(-Inf, x, Inf), 
                  labels=c("Bad", "Good"))
  cTab <- table(test.loans$status, preds) 
  addmargins(cTab.backward)
  profit.loop <- append(profit.loop,sum(test.loans$profit[preds=="Good"]))
}

plot(thresholds,profit.loop, col="#009900", main="Total Loan Profit by Threshold", xlab="Threshold",ylab="Profit")
lines(thresholds,profit.loop, col="#009900")

summary(fit.backward2)

predprob.backward <- predict(fit.backward2, test.loans, type = "response") 
threshold <- 0.7
preds.backward <- cut(predprob.backward, breaks=c(-Inf, threshold, Inf), 
                labels=c("Bad", "Good"))

cTab.backward <- table(test.loans$status, preds.backward) 
addmargins(cTab.backward)
p.backward <- sum(diag(cTab.backward)) / sum(cTab.backward)
print(paste('Proportion correctly predicted = ', p.backward)) 
