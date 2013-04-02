require(XML)

##f <- "add cases.R"

##cat.data.file <- "qrld.csv"
#model.file.in <- "Species vulnerability model2 landscape only simple3 names changed.xdsl"
##model.file.in <- "dummy empty model.xdsl"
##model.file.out <- "svm2los3.xdsl"

##sppinterest.file <- "spinterest.csv"


## load the species data to process
spp <- read.csv(cat.data.file, stringsAsFactors=FALSE)
## add row names so I can index by species name
row.names(spp) <- spp$sp_name

sppinterest <- read.csv(sppinterest.file, stringsAsFactors=FALSE)
spin <- sppinterest$Species
splist <- spp$sp_name[spp$sp_name %in% spin]

## load the GeNiE model file
d <- xmlInternalTreeParse(model.file.in)

## find the cases node
dcases <- getNodeSet(d, "//cases")[[1]]

## which nodes to try and populate
## the code below will skip any nodes with an NA or "none" as the
## node value -- child nodes should be NA by default
nlist <- names(spp)
nlist <- nlist[!(nlist %in% c("sp_name", "sp_code"))]

for (s in splist) {
    ## create a new case node for the species
    newspp <- newXMLNode("case", attrs=list(name=s), parent=dcases)

    ## now add a new evidence node for each variable
    for (i in nlist) {
        v <- spp[s, i]
        if (is.na(v) | v == "none")
            next
        newXMLNode("evidence", attrs=list(node=i, state=v), parent=newspp)

    }
}

saveXML(d, model.file.out)

popnodes <- function(x) {
    r <- length(x)
    r <- r - sum(is.na(x))
    r <- r - sum(x == "none")
    return(r)
}

foo <- data.frame(nodes=apply(spp[splist, nlist], 1, popnodes))
