Sim <- function(numNodes) {
    indegreeVec <- vector(mode = "integer", length = numNodes)
    adjMat <- matrix(c(0), nrow = numNodes, ncol = numNodes)
    return(SimHelper(indegreeVec, adjMat, numNodes))
}

SimHelper <- function(indegreeVec, adjMat, index) {
    if (index == 1) {
        return(list('indegreeVec' = indegreeVec, 'adjMat' = adjMat))
    } else if (index == 2) {
        choose <- 1
    } else {
        res <- SimHelper(indegreeVec, adjMat, index - 1)
        indegreeVec <- res$indegreeVec
        adjMat <- res$adjMat
        choose <- Choose(indegreeVec, endIndex = index - 1)
    }
    indegreeVec[choose] <- indegreeVec[choose] + 1
    indegreeVec[index] <- 1
    adjMat[choose, index] <- adjMat[choose, index] + 1
    adjMat[index, choose] <- adjMat[index, choose] + 1

    return(list('indegreeVec' = indegreeVec, 'adjMat' = adjMat))
}

Choose <- function(indegreeVec, endIndex) {
    return(sample(1:endIndex, 1, prob = indegreeVec[1:endIndex]))
}

DebugChoose <- function() {
    kRepeat <- 1000
    indegreeVec <- c(4, 1, 1, 1, 1)
    chooseVec <- c(0, 0, 0, 0, 0)
    for (i in 1:kRepeat) {
        choose <- Choose(indegreeVec, endIndex = 5)
        chooseVec[choose] <- chooseVec[choose] + 1
    }
    for (i in 1:length(chooseVec)) {
        cat('prob choosing ', i, ' : ', chooseVec[i] / kRepeat, '\n')
    }
}

PAMsim <- function(nNodes) {
    model <- Sim(numNodes = nNodes)
    return(model$indegreeVec)
}

DebugPAMsim <- function() {
    print(PAMsim(nNodes = 1))
    print(PAMsim(nNodes = 2))
    print(PAMsim(nNodes = 3))
    print(PAMsim(nNodes = 10))
}

Verify <- function() {
    kRepeat <- 1000
    countP4Attach1 <- 0
    for (i in 1:kRepeat) {
        adjMat <- Sim(4)$adjMat
        if (adjMat[4, 1] == 1) {
            countP4Attach1 <- countP4Attach1 + 1
        }
    }
    cat('P(4Attach1): ', countP4Attach1 / kRepeat, '\n')
}
