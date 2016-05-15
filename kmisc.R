# The MIT License (MIT)
# 
# Copyright (c) 2015-2016 Fabio Gabriel
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

library(car)
library(caret)
library(corrplot)

# readObject
#
# (file, ...) -> data.frame
#
# Read a RDS object if it exists, otherwise read a CSV with specified parameters and writes
# a RDS object from it.
readObject <- function(file, ...) {
    obj_name <- paste(basename(file), '.rds', sep='')
    obj_path <- file.path(dirname(file), obj_name)

    if (file.exists(obj_path)) {
        readRDS(obj_path)
    } else {
        csv <- read.csv(file, ...)
        saveRDS(csv, obj_path)
        csv
    } 
}

# dropColumns
#
# (data.frame, c() of strings) -> data.frame 
#
# Create a new data frame with specifed columns dropped from the original data frame.
dropColumns <- function(obj, cols) {
    obj[, !(colnames(cols) %in% cols)]
}

# selectColumns
#
# (data.frame, c() of strings) -> data.frame 
#
# Create a new data frame keeping specified columns from the original data frame.
selectColumns <- function(obj, cols) {
    obj[, colnames(cols) %in% cols]
}

#getTrainSplit <- function(train, label, holdout.percentage=0.2, drop.labels=T) {
#
#    if (holdout.percentage <= 0 || holdout.percentage >= 1) {
#        stop('Holdout percentage outside (0,1) interval.')
#    }
#
#    n <- nrow(train)
#    offset <- ceiling(n*(1 - holdout.percentage))
#    df <- train[sample(n, n),]
#    train_split <- df[1:offset,]
#    test_split <- df[(offset+1):n,]
#    y <- test_split[[label]]
#
#    if (drop.labels) {
#        test_split[[label]] <- NULL
#    }
#
#    return(list(train=train_split, test=test_split, y=y))
#}

# getDefault
# 
# Returns the value of attribute param in list l if it existing, otherwise return default.
getDefault <- function(l, param, default) {
    v <- l[[param]]
    
    if(is.null(v)) {
        v <- default
    }
    
    return(v)
}

createTestSplit <- function(train, holdout.percentage=0.2) {
    if (holdout.percentage <= 0 || holdout.percentage >= 1) {
        stop('Holdout percentage outside (0,1) interval.')
    }

    n <- nrow(train)
    offset <- ceiling(n * (1 - holdout.percentage))
    return(sample(n, offset))
}

createFormula <- function(object, target, todrop=NULL) {
    todrop <- c(todrop, target)
    cols <- colnames(object)[!(colnames(object) %in% todrop)]
    return(as.formula(paste(target, '~', paste(cols, collapse='+'))))
}

# createDummyVariables
#
# (data.frame, c(), boolean) -> data.frame
#
# Creates a data.frame with additional dummy variables for the selected variables.
createDummyVariables <- function(dataf, vars, full.rank=T, sep='_') {
    if (full.rank) {
        for (vr in vars) {
            for (level in unique(dataf[[vr]])) {
                dataf[paste("dummy", gsub(" ", "", level), sep=sep)] <- ifelse(dataf[[vr]] == level, 1, 0)
            }
        }
        return(dataf)
    } else {
        dummies <- model.matrix(as.formula(paste('~', paste(vars, collapse='+'))), data=dataf)[, -1]
        return(cbind(dataf, dummies))
    }
}

filterByClass <- function(dataf, cl) {
    cols <- as.vector(sapply(dataf, function(x) class(x) == cl))
    return(dataf[, cols])
}

filterNumeric <- function(dataf) {
    cols <- as.vector(sapply(dataf, is.numeric))
    return(dataf[, cols])
}

paster <- function(...) {
    paste(..., collapse='')
}

#
# Heuristic plots
#

# Inspired by 'Data Mining with R' by Luís Torgo
normalityPlot <- function(xs, prob=F, xlab='', main='', showDensity=T, showJitter=T) {
    expr <- deparse(substitute(xs))
    par(mfrow=c(1,2))

    if (main == '') {
        main <- paste('Histogram of', expr)
    }

    if (xlab == '') {
        xlab <- expr
    }

    hist(xs, prob=prob, xlab=xlab, main=main)

    if (showDensity) {
        lines(density(xs, na.rm=T))
    }

    if (showJitter) {
        rug(jitter(xs))
    }

    qqPlot(xs, main=paste('Normal QQ plot of', expr))
    par(mfrow=c(1,1))
}

boxPlot <- function(xs, ylab='', showJitter=T, showMean=T) {
    if (ylab == '') {
        ylab <- deparse(substitute(xs))
    }

    boxplot(xs, ylab=ylab)

    if (showJitter) {
        rug(jitter(xs), side=2)
    }

    if (showMean) {
        abline(h = mean(xs, na.rm=T), lty=2)
    }
}

normHist <- function(data) {
    h <- hist(data)
    multiplier <- h$counts/h$density
    dens <- density(data)
    dens$y <- dens$y * multiplier[1]
    plot(h)
    lines(dens)
}

correlationPlot <- function(dataf) {
    correlations <- cor(dataf)
    corrplot(correlations, order='hclust')
}

#
# Metrics
#
LogLoss <- function(act, pred, eps=1e-15) {
    nr <- length(pred)
    pred <- max(pred, eps)
    pred <- min(pred, 1-eps)
    ll <- sum(act*log(pred))
    ll <- ll * (-1/length(act))
    return (ll)
}

rmse <- function(y, y_hat) {
    sqrt(mean((y-y_hat)^2))
}

# From "R in Action" by Robert Kabacoff
wssplot <- function(data, nc=15, seed=1234) {
    wss <- (nrow(data)-1)*sum(apply(data,2,var))
    for (i in 2:nc) {
        set.seed(seed)
        wss[i] <- sum(kmeans(data, centers=i)$withinss)
    }
    plot(1:nc, wss, type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares")
}

rmCols <- function(data, cols) {
    !names(data) %in% c(cols)
}

