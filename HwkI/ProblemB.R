IsSending <- function() {
	return(sample(c(TRUE, FALSE), 1, prob = c(0.6, 0.4)))
}

DebugIsSending <- function() {
	kRepeat = 1000
	result <- vector(length = kRepeat)
	for (i in 1:kRepeat) {
		result[i] = IsSending()
	}
	cat('P(IsSending): ', mean(result == TRUE), '\n')
}

IsGenerated <- function() {
	return(sample(c(TRUE, FALSE), 1, prob = c(0.2, 0.8)))
}

DebugIsGenerated <- function() {
	kRepeat = 1000
	result <- vector(length = kRepeat)
	for (i in 1:kRepeat) {
		result[i] = IsGenerated()
	}
	cat('P(IsGenerated): ', mean(result == TRUE), '\n')
}

Sim <- function(repeats) {
    # model[i, j]: at repeat i, epoch j, how many attempts
    model <- matrix(c(-1), nrow = repeats, ncol = 2)
    for (i in 1:dim(model)[1]) {
        node1 = TRUE # currect has message to send, X0 = 2
        node2 = TRUE
        for (j in 1:2) {
            if (node1 & node2) {
                isNode1Sending <- IsSending()
                isNode2Sending <- IsSending()
                if (isNode1Sending & isNode2Sending) {
                    model[i, j] <- 2
                } else if (isNode1Sending) {
                    node1 = FALSE
                    model[i, j] <- 1
                } else if (isNode2Sending) {
                    node2 = FALSE
                    model[i, j] <- 1
                } else {
                    model[i, j] <- 0
                }
            } else if (node1) {
                if (IsSending()) {
                    node1 = FALSE
                    model[i, j] <- 1
                } else {
                    model[i, j] <- 0
                }
                node2 = IsGenerated()
            } else if (node2) {
                if (IsSending()) {
                    node2 = FALSE
                    model[i, j] <- 1
                } else {
                    model[i, j] <- 0
                }
                node1 = IsGenerated()
            } else {
                model[i, j] <- 0
                node1 = IsGenerated()
                node2 = IsGenerated()
            }
        }
    }
    return(model)
}

Filter <- function(model) {
    model2Attempts <- matrix(c(-1), nrow = 0, ncol = 2)
    for (i in 1:dim(model)[1]) {
        if (sum(model[i, ]) == 2) {
            model2Attempts <- rbind(model2Attempts, model[i, ])
        }
    }
    return(model2Attempts)
}

ExistIn2 <- function(model) {
    numExistIn2 <- 0
    for (i in 1:dim(model)[1]) {
        if (model[i, 2] > 0) {
            numExistIn2 <- numExistIn2 + 1
        }
    }
    return(numExistIn2 / dim(model)[1])
}

Main <- function() {
    kRepeat = 1000
    model <- Sim(kRepeat)
    model2Attempts <- Filter(model)
    print(ExistIn2(model2Attempts))
}

Main()
