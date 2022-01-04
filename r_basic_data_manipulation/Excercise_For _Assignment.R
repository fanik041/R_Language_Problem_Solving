csvFile <- read.csv("Assignment.01/warpbreaks.csv")

isIntegerOrIsNumeric<- print(sapply(csvFile, is.integer) | sapply(csvFile, is.numeric))


print(csvFile)
is.numeric(csvFile$breaks)
isNumeric <- print(sapply(read.csv("Assignment.01/warpbreaks.csv"), is.numeric))
is.integer(csvFile$wool)

print(isIntegerOrIsNumeric)

suppressWarnings(v)

v <- factor(c("2", "3", "5", "7", "11"))
as.character(v)
x <- factor(c(1, 2, 3, 6, 9))
as.character(x)
as.numeric(x)
as.numeric(v)
y <- print(as.numeric(as.character(factor(v))))
typeof(mean) 
mean(1)
typeof(mean)
