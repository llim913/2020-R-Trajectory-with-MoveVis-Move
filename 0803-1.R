# 0. 환경설치-------------------------------------https://www.kaggle.com/taindow/simple-lstm-with-r
library(keras)
library(vctrs)
library(tidyverse)
library(dplyr)
library(nnet) #전방 전달 신경망 + 다향 로그선형모형
library(neuralnet)
library(tensorflow)
library(deepnet)
library(RcppDL)
library(rnn)
library(DEEPR)
library(devtools)
library(rlang)
library(sys)
library(reticulate)
library(tidytext)
library(data.table)
library(stringr)
library(rlist)
library(lambda.r)
library(matrixStats)


autoencoder <- function(X, hiddenLayers = c(10, 5, 10), lossFunction = "pseudo-huber",
            dHuber = 1, linearLayers = NA, rectifierLayers = NA,
            sigmoidLayers = NA, standardize = TRUE, learnRate = 1e-06,
            maxEpochs = 100, batchSize = 32, momentum = 0.2, L1 = 0, L2 = 0,
            validLoss = TRUE, validProp = 0.1, verbose = TRUE, earlyStop = FALSE,
            earlyStopEpochs = 50, earlyStopTol = -1e-07, lrSched = FALSE,
            lrSchedEpochs = NA, lrSchedLearnRates = NA, robErrorCov = FALSE)


aeNN <- autoencoder(faithful, hiddenLayers = c(4,1,4), batchSize = 5,
                      learnRate = 1e-5, momentum = 0.5, L1 = 1e-3, L2 = 1e-3,
                      robErrorCov = TRUE)
plot(aeNN)

rX <- reconstruct(aeNN, faithful)
plot(rX, alpha = 0.05)
plot(faithful, col = (rX$mah_p < 0.05)+1, pch = 16)
# }


















