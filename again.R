library(qlcMatrix)
library(plotly)

#get file from current file location
data <- read.csv("D:/Clone Gits/mati gw/ree/DataTugas2.csv")

# get data
income.column <- c(data$Pendapatan)
debt.column <- c(data$Hutang)

# membership Function
income.low <- function(x){
  ifelse(x < 0.3, 1, ifelse((x >= 0.3 & x <= 1), (1 - x)/0.7, 0)) 
}

income.normal <- function(x){
  ifelse((x < 0.5 | x > 1.5), 0, ifelse((x >= 0.5 & x <= 1), (x - 0.5)/0.5, (1.5 - x)/0.5))
}

income.high <- function(x){
  ifelse(x < 1, 0, ifelse((x >= 1 & x <= 1.7), (x - 1)/0.7, 1)) 
}

debt.lower <- function(x){
  ifelse(x < 15, 1, ifelse((x >= 15 & x <= 35), (35 - x)/20, 0)) 
}

debt.low <- function(x){
  ifelse((x < 20 | x > 50), 0, ifelse((x >= 20 & x <35 ), (x - 20)/15, (50 - x)/15)) 
}

debt.normal <- function(x){
  ifelse((x < 35 | x > 65), 0, ifelse((x >= 35 & x < 50), (x - 35)/15, (65 - x)/15)) 
}

debt.high <- function(x){
  ifelse(x < 50 | x > 80, 0, ifelse((x >= 50 & x <= 65), (x - 50)/15, (80 - x)/15)) 
}

debt.higher <- function(x){
  ifelse(x < 70, 0, ifelse((x >= 70 & x < 85), (x - 70)/15, 1)) 
}

# fuzzification
## calculate fuzzy input
s.low <- income.low(income.column)
s.avg <- income.normal(income.column)
s.high <- income.high(income.column)

d.lower <- debt.lower(debt.column)
d.low <- debt.low(debt.column)
d.normal <- debt.normal(debt.column)
d.high <- debt.high(debt.column)
d.higher <- debt.higher(debt.column)

# inference
## the rule for the model that we created using mamdani methode
model.yes <- matrix(c(pmin(s.low, d.high), pmin(s.low, d.higher), pmin(s.avg, d.high), pmin(s.avg, d.higher)), ncol = 4)
model.maybe <- matrix(c(pmin(s.low, d.low),pmin(s.avg, d.normal), pmin(s.high, d.high), pmin(s.low, d.normal),  pmin(s.high, d.higher)), ncol = 5)
model.no <- matrix(c(pmin(s.low, d.lower), pmin(s.avg, d.lower), pmin(s.high, d.normal), pmin(s.high, d.low), pmin(s.avg, d.low), pmin(s.high, d.lower)), ncol = 6)


## the rule for fuzzy sets
pre.yes <- c(rowMax(model.yes))
pre.maybe <- c(rowMax(model.maybe))
pre.no <- c(rowMax(model.no))

# defuzzfication
defuzz.no <- function(){runif(3, min = 0.0, max = 40.0)}
defuzz.maybe <- function(){runif(3, min = 45.0, max = 70.0)}
defuzz.yes <- function(){runif(3, min = 80.0, max = 100.0)}

# calculate acceptance for each rows
acceptance <- 
  (pre.yes*sum(defuzz.yes()) + pre.maybe*sum(defuzz.maybe()) + pre.no*sum(defuzz.no())) / (pre.yes*3 + pre.maybe*3 + pre.no*3)

# summary output for data.frame
output <- data.frame(
  income = income.column,
  debt = debt.column,
  accept = acceptance
)

# sorting
output.sorted <- output[order(-output$accept),]
DataToExport <- output.sorted[c(1:20),]
write.csv(DataToExport, file="D:/Clone Gits/mati gw/outputData.csv")
print(DataToExport)


# plot session

data.income <- data.frame(
  x <- income.low(seq(0, 2, by=0.1)),
  y <- income.normal(seq(0, 2, by=0.1)),
  z <- income.high(seq(0, 2, by =0.1)))

income.plot <- plot_ly(data.income, y = ~x, x = seq(0, 2, by=0.1), name = 'Income low', type='scatter', mode = 'lines') %>%
  add_trace(y = ~y, name = 'Income normal', mode = 'lines')%>%
  add_trace(y = ~z, name = 'Income high', mode = 'lines')

print(income.plot)


data.debt <- data.frame(
  a <- debt.lower(seq(0, 100, by=0.1)),
  b <- debt.low(seq(0, 100, by=0.1)),
  c <- debt.normal(seq(0, 100, by=0.1)),
  d <- debt.high(seq(0, 100, by=0.1)),
  e <- debt.higher(seq(0, 100, by=0.1))
)

debt.plot <- plot_ly(data.debt, y = ~a, x = seq(0, 100, by=0.1), name = 'Debt Lower', type='scatter', mode = 'lines')%>%
  add_trace(y = ~b, name = 'Debt Low', mode = 'lines')%>%
  add_trace(y = ~c, name = 'Debt Normal', mode = 'lines')%>%
  add_trace(y = ~d, name = 'Debt High', mode = 'lines')%>%
  add_trace(y = ~e, name = 'Debt Higher', mode = 'lines')

print(debt.plot)

data.res.sort <- data.frame(
  x <- output.sorted$income,
  y <- output.sorted$debt
)

c <- c(rep("green", 20), rep("red", 80))

p <- plot_ly(data.res.sort, y = ~x, x = ~y, mode = 'markers', type = 'scatter', marker = list(color = c))
