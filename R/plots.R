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

    car::qqPlot(xs, main=paste('Normal QQ plot of', expr))
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
    corrplot::corrplot(correlations, order='hclust')
}

# From "R in Action" by Robert Kabacoff
# TODO. Evaluate other measures (BIC - Bayesian Information Criterion, etc.).
wssplot <- function(data, nc=15, seed=1234, delta=1^-5) {
    wss <- (nrow(data)-1) * sum(apply(data,2,var))
    for (i in 2:nc) {
        set.seed(seed)
        wss[i] <- sum(kmeans(data, centers=i)$withinss)
        if (abs(wss[i] - wss[i - 1]) < delta) {
            break
        }
    }
    plot(1:i, wss[1:i], type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
    return(which.min(wss))
}
