library(DescTools)
library(editrules)


irisFile <- print(read.csv("dirty_iris.csv", stringsAsFactors = FALSE))

complete.cases(irisFile)
CountCompCases(irisFile)

percentage_completeFile_decimal <- sum(!is.na(irisFile))/prod(dim(irisFile))

label_percent(accuracy =0.001, scale = 100)(print(percentage_completeFile_decimal))


irisFile_1.1 <- irisFile
irisFile_1.1 <- irisFile[irisFile < 0] <- NA
irisFile_1.2 <- irisFile_1.1[irisFile_1.1 ==0] <- NA


is.special <- function(x){
  if (is.numeric(x)) !is.finite(x) else is.na(x)
}

CheckSpecial <- print(sapply(irisFile, is.special))

editingFile <- (editfile("edits.txt"))
print(editingFile)

vio_edits <- (violatedEdits(editingFile, irisFile))
sum_vio_edit <- summary(vio_edits)
plot(vio_edits)

vio_edits$num7

df1 <- data.frame(vio_edits)
df2 <- data.frame(irisFile)
id= c(1)


boxplot.stats(irisFile$Sepal.Length)

match(irisFile$Sepal.Length)

num_3.2_d <-  print(which(irisFile$Sepal.Length < irisFile$Petal.Length))

library(deducorrect)

ex3.3_a <- correctionRules(expression (
  if  (
    (Petal.Width)<0 && !is.na(Petal.Width) 
    ) (Petal.Width) <- NA
)
)

correctedFile <- print(correctWithRules(ex3.3_a, irisFile))




E <- editmatrix(c(
  "x <=30",
  "x > 0",
  "y > 0",
  "z > 0",
  "x > z"))

dat <- data.frame(
  x = c(irisFile$Sepal.Length),
  y = c(irisFile$Sepal.Width),
  z = c(irisFile$Petal.Length),
  w = c(irisFile$Petal.Width))

# localize all errors in the data
err <- as.editmatrix(editingFile, numeric(nrow(editingFile)))
err

err1 <- localizeErrors(E, dat)
err1



irisFile[apply(err1$adapt, 1, function(x) {
  if (sum(x) > 0) TRUE 
  else FALSE
  }
  ), 
  "Petal.Length"] <- NA

summary(violatedEdits(editingFile, irisFile))

