clean <- function()  {
    test <- read.table("test/X_test.txt")
    y_test <- readLines("test/y_test.txt") 
    lab <- readLines("features.txt")
    colnames(test) <- lab
    test$Type <- y_test 
    train <- read.table("train/X_train.txt")
    y_train <- readLines("train/y_train.txt")
    colnames(train) <- lab
    train$Type <- y_train
    total <- rbind(test, train)
    type <- total$Type
    name <- colnames(total)
    name1 <- grep("mean", name)
    name2 <- grep("std", name)
    name3 <- grep("Mean", name)
    nm <- c(name1, name2, name3)
    nm <- name[nm] 
    part <- subset(total, select=nm)
    act <- readLines("activity_labels.txt")
    part$Type <- act[as.integer(type)]
    n <- ncol(part)
    mt <- c()
    for (i in 1:(n-1))  {
        y <- tapply(part[, i], part[, n], FUN=mean, na.rm=TRUE)
        mt <- cbind(mt, y)
    }
    df <- data.frame(mt)
    names(df) <- nm
    write.csv(df, file = "meandata.csv")
}