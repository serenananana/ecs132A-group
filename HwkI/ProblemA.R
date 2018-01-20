Drive <- function() {
    return(sample(c(3, 4, 5), 1, prob = c(0.5, 0.25, 0.25)))
}

DebugDrive <- function() {
    kRepeat <- 1000
	result <- vector(mode = 'integer', length = kRepeat)
	for (i in 1:kRepeat) {
		result[i] <- Drive()
	}
	cat('P(3 minutes): ', mean(result == 3), '\n')
	cat('P(4 minutes): ', mean(result == 4), '\n')
	cat('P(5 minutes): ', mean(result == 5), '\n')
}

Sim <- function(repeats) {
	# result[i, j]: at repeat i, j car's minute
	result <- matrix(c(0), nrow = repeats, ncol = 3)
	for (i in 1:repeats) {
		result[i, 1] <- Drive()
		result[i, 2] <- Drive()
		result[i, 3] <- Drive()
	}
	return(result)
}

P1 <- function(model) {
    numFirstIs4 <- 0
    for (i in 1:dim(model)[1]) {
        if ((!(3 %in% model[i, ])) & (4 %in% model[i, ])) {
            numFirstIs4 <- numFirstIs4 + 1
        }
    }
    return(numFirstIs4 / dim(model)[1])
}

P2 <- function(model) {
    numSumIs10 <- 0
    for (i in 1:dim(model)[1]) {
        if (sum(model[i, ]) == 10) {
            numSumIs10 = numSumIs10 + 1
        }
    }
    return(numSumIs10 / dim(model)[1])
}

P3 <- function(model) {
    modelSame <- matrix(c(0), nrow = 0, ncol = 3)
    for (i in 1:dim(model)[1]) {
        if (model[i, 1] == model[i, 2] & model[i, 2] == model[i, 3]) {
            modelSame <- rbind(modelSame, model[i, ])
        }
    }
    numAllIs3 <- 0
    for (i in 1:dim(modelSame)[1]) {
        if (modelSame[i, 1] == 3 & modelSame[i, 2] == 3 & modelSame[i, 3] == 3) {
            numAllIs3 <- numAllIs3 + 1
        }
    }
    return(numAllIs3 / dim(modelSame)[1])
}

Main <- function() {
    kRepeat = 10000
	model <- Sim(kRepeat)
    print(P1(model))
    print(P2(model))
	print(P3(model))
}

Main()
