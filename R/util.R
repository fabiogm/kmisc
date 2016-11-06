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

# getDefault
# 
# Returns the value of attribute param in list l if it existing, otherwise return default.
getDefault <- function(l, param, default) {
    if (length(l) == 0) {
        return(default)
    }

    v <- l[[param]]
    
    if(is.null(v)) {
        v <- default
    }
    
    return(v)
}

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
    obj[, !(colnames(obj) %in% cols)]
}

# selectColumns
#
# (data.frame, c() of strings) -> data.frame 
#
# Create a new data frame keeping specified columns from the original data frame.
selectColumns <- function(obj, cols) {
    obj[, colnames(cols) %in% cols]
}

rmCols <- function(data, cols) {
    !names(data) %in% c(cols)
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
