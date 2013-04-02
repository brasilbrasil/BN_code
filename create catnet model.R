require(XML)
require(catnet)
require(igraph)

source(paste(code_loc,"catnet parsing functions.R", sep=""))

d <- xmlInternalTreeParse(model.file)

## for exploring the file
dnodes <- getNodeSet(d, "//node")
dcpts <- getNodeSet(d, "//cpt")
dpars <- getNodeSet(d, "//parents")

## pull out the nodes and node definition objects
nodes <- xpathSApply(d, "//node", xmlGetAttr, "id")
cpts <- xpathSApply(d, "//cpt", xmlGetAttr, "id")
if (length(setdiff(nodes, cpts)) > 0) {
    stop("Node names and definitions do not match up!")
}

## create a list of category levels for each model node
catlist <- list(NULL)
z <- 1
for (i in nodes) {
    path <- paste0("//cpt[@id='", i, "'][last()]/state[@id]")
    states <- xpathSApply(d, path, xmlGetAttr, "id")
    catlist[[z]] <- states
    z <- z + 1
}

## create a list of parent nodes for each node
parlist <- list(NULL)
z <- 1
for (i in nodes) {
    path <- paste0("//cpt[@id='", i, "'][last()]/parents[last()]")
    parents.node <- getNodeSet(d, path)
    if (length(parents.node) == 0) {
        parlist[[z]] <- NULL
    } else {
        ## in case there's more than one parents definition (shouldn't be)
        ## take the last one
        parentstring <- xmlValue(parents.node[[length(parents.node)]])
        parlist[[z]] <- unlist(strsplit(parentstring, " "))
    }
    z <- z + 1
}

## If the final element of a list is NULL it isn't counted for the
## length of the list. This will cause problems when we use cnNew
## to create the network, so force the list length to match
## the length of the nodes list -- any new nodes forced will have
## a NULL value, which is what we want.
length(parlist) <- length(nodes)


## create a nested list of probabilities for each node given
## the probabilities for each parent node
problist <- list(NULL)
z <- 1
for (i in nodes) {
    path <- paste0("//cpt[@id='", i, "'][last()]/probabilities[last()]")
    probs.node <- getNodeSet(d, path)
    if (length(probs.node) == 0) {
        problist[[z]] <- NULL  # should never happen
    } else {
        ## in case there's more than one probabilities definition
        ## (which should not happen) take the last one
        probstring <- xmlValue(probs.node[[length(probs.node)]])
        p <- as.numeric(unlist(strsplit(probstring, " ")))
        ## id number of this node
        nid <- which(nodes %in% i)
        ## get the number of levels in each of the node's parents
        levels <- parent.parms(nid, nodes, parlist, catlist)$levels
        if (is.null(levels)) {  ## this is a top node
            problist[[z]] <- p
        } else {
            ## add on the number of levels for this node
            levels <- c(levels, length(catlist[[nid]]))
            problist[[z]] <- parseprobs(levels, p)
        }
        z <- z + 1
    }
}

## If the final element of a list is NULL it isn't counted for the
## length of the list. This will cause problems when we use cnNew
## to create the network, so force the list length to match
## the length of the nodes list -- any new nodes forced will have
## a NULL value, which is what we want.
length(problist) <- length(nodes)


foo <- cnNew(nodes, catlist, parlist, problist)
svm2los <- foo
save(svm2los, file="svm2los.Rdata")
