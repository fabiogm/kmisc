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

#
# Metrics
#
.logLoss <- function(act, pred, eps=1e-15) {
    nr <- length(pred)
    pred <- pmax(pred, eps)
    pred <- pmin(pred, 1-eps)
    ll <- sum(act*log(pred) + (1-act)*log(1-pred))
    ll <- ll * (-1/nr)
    return (ll)
}

.mae <- function(actual, predicted) {
    sum(abs(actual - predicted)) / length(actual)
}

.rmse <- function(y, y_hat) {
    sqrt(mean((y-y_hat)^2))
}

VALID_METRICS <- c('logLoss', 'mae', 'rmse')

.getMetric <- function(metricName) {
    switch(metricName,
           logLoss = .logLoss,
           mae = .mae,
           rmse = .rmse)
}

partitionData <- function(traindf, target) {
    inTraining <- caret::createDataPartition(traindf[[target]], p=.75, list=F)
    training <- traindf[inTraining, ]
    testing <- traindf[-inTraining, ]
    labels <- testing[[target]]
    testing[[target]] <- NULL
    return(list(train=training, test=testing, labels=labels))
}

#' create train control
#'
#' Creates a train control to be used in model validation.
#' @param metric The evaluation metric. This can be an user defined function. Defaults to LogLoss.
#' @param n The number of validation rounds. Defaults to 3.
#' @param method The sampling method. Defaults to repeated (Monte-Carlo) sampling.
createControl <- function(metric='logLoss', n=3, method="repeated") {
    # TODO. Implement the remaining strategies.
    if (method != "repeated") {
        stop("repeated is the only implemented sampling approach")
    }

    if (!(method %in% c("repeated", "bootstrap", "cv"))) {
        stop("Invalid validation method: ", method)
    }

    if (!(metric %in% VALID_METRICS)) {
        stop("Invalid metric: ", metric)
    }

    list(metric=.getMetric(metric), n=n, method="repeated")
} 

.performTunningRounds <- function(traindf, methodName, params, target, metric, n, verbose, ...) {
    scoreSum <- 0
    method <- get(methodName)

    for (i in 1:n) {
        l <- partitionData(traindf, target)
        preds <- method(l$train, l$test, params, ...)
        scoreSum <- scoreSum + metric(l$label, preds)
    }
    score <- scoreSum/n
    return(score)
}

displayFmtLn <- function(...) {
    cat(sprintf(...), "\n")
}

displayFmt <- function(...) {
    cat(sprintf(...))
}

.dfToStr <- function(dataf) {
    buffer <- ""

    for (n in names(dataf)) {
        buffer <- sprintf("%s%s=%s", buffer, n, dataf[,n])
        buffer <- sprintf("%s, ", buffer)
    }
    
    substr(buffer, 1, nchar(buffer) - 2)
}

tuneModelParameters <- function(traindf, method, paramGrid, target, control, verbose=F, ...) {
    scores <- c()
    metric <- control$metric
    n <- control$n
    display <- displayFmt
    displayLn <- displayFmtLn

    if (!verbose) {
        display <- function(...) {}
        displayLn <- function(...) {}
    }

    displayLn('Tunning %s', method)

    if (!(target %in% names(traindf))) {
        stop("Not a valid label: ", target)
    }

    for (i in 1:nrow(paramGrid)) {
        paramRow <- paramGrid[i, ]
        displayLn("Iteration %d of %d", i, nrow(paramGrid))
        tm <- proc.time()
        newScore <- .performTunningRounds(traindf, method, paramRow, target, metric, n, verbose=verbose, ...)
        displayLn("%s (%s): %f", method, .dfToStr(paramRow), newScore)
        displayLn("Elapsed time: %f", proc.time()[[3]] - tm[[3]])
        scores <- c(scores, newScore)
    }

    return(cbind(paramGrid, scores))
}

