## a function to find which elements of a vector v match those
## in vector x and return the matching indexes of v in the same
## order they appear in x.
## This is inefficient, but will do for a quick kludge.
which.ordered <- function(x, v) {
    nhits <- length(which(v %in% x))
    hits <- rep(0, nhits)
    for (i in 1:nhits) {
        ind <- which(v %in% x[i])
        if (length(ind) > 0) {
            hits[i] <- ind[1]
                                        # for my implementation should
                                        # never be more than one, if there
                                        # is, take the first. If no match,
                                        # skip
        }
    }
    return(hits)
}

## function to return a dataframe containing a node's parents (in order)
## and the corresponding number of levels
## nid = node of interest
## nodes = vector of node names
## parlist = list of vectors of parents for each node
## catlist = list of vectors of categories for each node
parent.parms <- function(nid, nodes, parlist, catlist) {
    parents <- parlist[[nid]]
    if (length(parents) == 0) {
        return(NULL)
    } else {
        nodeids <- which.ordered(parents, nodes)
        levels <- rep(0, length(nodeids))
        for (i in 1:length(nodeids)) {
            levels[i] <- length(catlist[[nodeids[i]]])
        }
    }
    return(data.frame(id=nodeids, nodes=nodes[nodeids], levels=levels))
}

## function to take a vector of levels of parent nodes and recursively
## parse a vector of probabilities into a structured list suitable
## for cnNew.
## levels = vector of levels in parent nodes
## p = vector of probabilities
parseprobs <- function(levels, p) {
    ## number of levels -- how far are we from the bottom
    nl <- length(levels)
    ## number of levels at the current height
    cl <- levels[1]
    curlist <- NULL
    for (i in 1:cl) {
        ## the length of a chunk of p appropriate to this level
        z <- length(p) / cl
        if (z != floor(z))
            stop("parseprobs: Inappropriately lengthed probability vector!\n",
                 "length(p) = ", length(p), "  cl = ", cl, "\n")
        ## find the subset of p suitable for the next step, start and end
        s <- (i - 1) * z + 1
        e <- s + z - 1
        sub.p <- p[s:e]
        if (nl == 1) {
            curlist[[i]] <- sub.p
        } else {
            curlist[[i]] <- parseprobs(levels[-1], sub.p)
        }
    }
    return(curlist)
}

## a simple sample network I used to test my functions
if (FALSE) {
    cnet <- cnNew(nodes = c("a", "b", "c"),
                  cats = list(c("a1","a2"), c("b1","b2"), c("c1","c2")),
              parents = list(NULL, c(1), c(1,2)),
              probs = list(c(0.2,0.8), # A
                           list(c(0.6,0.4),c(0.4,0.6)), # B
                           list(list(c(0.3,0.7),c(0.7,0.3)),  # C for A
                                list(c(0.9,0.1),c(0.1,0.9)))) # C for B
              )
}

## A function to return the conditional probability of a node from
## a cnet object, given a model, node, and data row
## m = a cnet network model
## n = the node of interest
## d = a single-row dataframe with column names equal to the model
##     node names and values indicating the state of each node
conditionalP <- function(m, n, d=NULL) {
    require(catnet)
    ptable <- getPtable(m, n, d)
    if (nrow(ptable) == 1)
        return(ptable)
    ## create a data frame of states and a matrix of
    ## associated probabilities from the parent nodes
    numeric.columns <- rep(FALSE, ncol(ptable))
    for (i in 1:ncol(ptable)) {
        numeric.columns[i] <- is.numeric(ptable[, i])
    }
    parent.states <- ptable[, !numeric.columns]
    ## if there is only one parent node, R converts the column to
    ## a vector, and I'm assuming it's a data frame. Force it into
    ## a data.frame
    if (is.null(dim(parent.states))) {
        parent.states <- data.frame(a=parent.states,
                                    stringsAsFactors=FALSE)
        names(parent.states) <- names(ptable[!numeric.columns])
    }
    parent.probs <- as.data.frame(matrix(0, nrow=nrow(parent.states),
                           ncol=ncol(parent.states)))
    names(parent.probs) <- names(parent.states)
    for (pnode in names(parent.states)) {
        foo <- conditionalP(m, pnode, d)
        for (i in 1:nrow(parent.states)) {
            parent.probs[i, pnode] <- foo[1, parent.states[i, pnode]]
        }
    }
    rowprobs <- apply(parent.probs, 1, prod)
    row.margins <- ptable[, numeric.columns]
    row.results <- row.margins[1, ]
    row.results[1, ] <- rep(NA, ncol(row.results))
    for (j in names(row.results)) {
        row.results[, j] <- sum(rowprobs * row.margins[, j])
    }
    return(row.results)
}

## extract the probability table from a node in a catnet model
## with cnProb and return it as a dataframe
## m = model
## n = node name
## d = a single row dataframe with column names for nodes in
##     the model and fixed values for the state of each node
getPtable <- function(m, n, d=NULL) {
    require(catnet)
    mnames <- cnNodes(m)
    ## set up the dataframe (bar) we're going to return. Call cnProb
    ## to get the basic structure.
    foo <- cnProb(m)[[n]]
    if (is.null(dim(foo))) {
        ## this node has no parents
        foorows <- 1
    } else {
        foorows <- dim(foo)[1]
    }
    bar <- matrix(foo, nrow=foorows)
    bar <- as.data.frame(bar, stringsAsFactors=FALSE)
    if (foorows == 1) {
        names(bar) <- names(foo)
    } else {
        names(bar) <- dimnames(foo)[[2]]
    }
    ## if a column is not a node parent, it's a level probability
    ## so convert it from a string to a number
    numeric.columns <- which(!names(bar) %in% cnParents(m)[[n]])
    for (i in numeric.columns) {
        bar[, i] <- as.numeric(bar[, i])
    }
    ## if we have data for this node, then return the observed state
    ## of the node. First check to see if it is null (no column for
    ## the node in d), then if it is NA (missing for that row).
    ## If we have data much of the previous processing is thrown out,
    ## but I need the most intensive call (cnProb) regardless; doing
    ## it this way saves some conceptual abstractions.
    if (n %in% names(d)) {
         x <- d[, n]
    } else {
        x <- NA
    }
    if (!is.na(x)) {
        bar <- bar[1, numeric.columns]
        bar[1, ] <- rep(0, ncol(bar))
        x.column <- which(names(bar) %in% x)
        if (length(x.column) != 1)
            stop("Node ", n, " value of ", x, " not found in model.\n",
                 "length of x.column =", length(x.column), "\n")
        bar[1, x.column] <- 1
    }
    return(bar)
}
