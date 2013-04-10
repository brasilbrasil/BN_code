## this script assumes "create catnet model.R" has already been
## run. If the catnet model it creates doesn't exist, run
## it again.
if (!file.exists(paste(wd,project_name,"_svm2los.Rdata", sep=""))) {
  #source("create catnet model.R")
  source(paste(code_loc,"create catnet model.R", sep=""))  
}else{
  load(paste(project_name,"_svm2los.Rdata", sep=""))
}
## if (somehow) the two utility functions from "catnet parsing
## functions.R" aren't here, run that script
if (!(exists("conditionalP") & exists("getPtable"))) {
  source(paste(code_loc,"catnet parsing functions.R", sep=""))
}

## load the species data to process
spp <- read.csv(cat.data.file, stringsAsFactors=FALSE)
## add row names so I can index by species name
row.names(spp) <- spp$sp_name

## a lot of missing values were categorized as "none" which is
## easier to read for humans. For this algorithm I change them
## to NA's
spp[spp == "none"] <- NA

## load the file of species for which we want results. All it
## needs is a column named "Species" whose names match values
## in "sp_code" in spp
sppinterest <- read.csv(sppinterest.file, stringsAsFactors=FALSE)
spin <- sppinterest$Species

## refine the species list to only those entries in spin that
## match values in spp$sp_name
## This is the list of species we will work with.
splist <- spp$sp_name[spp$sp_name %in% spin]
## For testing, look at just a single species.
## splist <- "Sophora chrysophylla"
#splist <- splist[1:3]
if (!is.null(sp_list_offset)){
  splist=splist[sp_list_offset[1]:sp_list_offset[2]]
  
}
if (revert_sp_order){
  splist=rev(splist)
}

## trim the amount of data needed for each pass though by eliminating
## columns whose values are NA for all species of interest
## start by finding # of non-NA's in each column
null.columns <- apply(spp[splist, ], 2, function(x) length(x) - sum(is.na(x)))
spp <- spp[, null.columns > 0]

spp2 <- spp[splist, ]
write.csv(spp2, paste("results/",project_name,"_spp_states.csv", sep=""), row.names=FALSE)

## This function collects the analysis for parallel analysis.
## X is a list element consisting of a 1-row dataframe whose
## columns are the node names and values are the observed
## levels of each node for that row. One column in X, "node"
## is used to determine which model node is to be caluclated.
## It returns the species name (taken from the "sp_name" column
## of the dataframe) the node being calculated, and the
## probability values for each level of the node. These levels
## are not currently labeled, but are assumed to be "Favorable"
## and "Unfavorable" later on. If there are more than two levels
## on an output node this later code will neeed to be reworked.
## It uses the global environment object svm2los which will
## need to be exported to each parallel instance.
lapplyCondP <- function(X) {
  c(X$sp_name, X$calc.node, conditionalP(svm2los, X$calc.node, X))
}

## depending upon the flag set in "process spp data.R" use
## or don't use a parallel processing algorithm
if (!calculate.parallel) {
  ## create a dataframe to hold the results
  out.data <- as.data.frame(matrix("", nrow=length(splist),
                                   ncol=length(output.nodes)+3),
                            stringsAsFactors=FALSE)
  row.names(out.data) <- splist
  colnames0=c(output.nodes, "vulnerability", "additive", "transformed")
  ncols=length(colnames0)
  colnames(out.data)=colnames0
  
  
  ntimestamp(noisy)
  n2cat(noisy, "Starting.\n")
  cat("starting species response calculations")
  s=splist[1]
  for (s in splist) {
    sp_csv=paste("spp_csvs/",project_name,"_",s,"_csv.csv", sep="")
    if (file.exists(sp_csv) & !overwrite_ps){
      jnk <- read.csv(sp_csv, stringsAsFactors=FALSE)
      out.data[s, ]=jnk[1:ncols]
      cat(s, " already calculated. \n")
    }else{
      d <- spp[s, ]
      n=output.nodes[1]
      for (n in output.nodes) {
        ntimestamp(noisy)
        #n3cat(noisy, "Attempting ", s, ", ", n, ".\n\n")
        cat("Attempting ", s, ", ", n, ".\n\n")
        foo <- try(conditionalP(svm2los, n, d))
        if (is.data.frame(foo)) {
          out.data[s, n] <- foo[1, 1]  # get the "Favorable" prob.
        } else {
          cat("Error for ", s, ", ", n, ": ", foo, "\n\n", sep="")
        }
      }
      out.data[s,"vulnerability"]=(1-((as.double(out.data[s,"Resist"])+as.double(out.data[s,"Migrate"]))/2))*(1-as.double(out.data[s,"Micro_refugia"]))  
      out.data[s,"additive"]=((1-((as.double(out.data[s,"Resist"])+as.double(out.data[s,"Migrate"]))/2))+(1-as.double(out.data[s,"Micro_refugia"])))/2  
      #out.data[s,"transformed"]=acos(1-as.double(out.data[s,"vulnerability"]))/(0.5*pi) #old transformation
      out.data[s,"transformed"]=(asin(as.double(out.data[s,"vulnerability"]))^(1/3))*2/pi
      
      jnk=out.data[s,1:ncols]
      write.csv(jnk, sp_csv, row.names=FALSE)
    }
    names(out.data) <- c(output.nodes, "vulnerability", "additive", "transformed")
  }
  ntimestamp(noisy)
  n2cat(noisy, "Done.\n")
  write.csv(out.data, csv_out_data, row.names=TRUE)
} else {
  require(parallel)
  ## make a list of species data -- rows of spp, once for each output for
  ## each species in splist
  spplist <- NULL
  j <- 1
  for (n in output.nodes) {
    for (i in 1:length(splist)) {
      spplist[[j]] <- as.data.frame(c(spp[splist[i], ], calc.node=n),
                                    stringsAsFactors=FALSE)
      j <- j + 1
    }
  }
  if (is.null(cores)) {
    cores <- detectCores()
  }
  cl <- makeCluster(cores)
  ## each cluster member will need these objects from the local environment
  clusterExport(cl, c("conditionalP", "getPtable", "svm2los", "n"))
  out.data.list <- NULL
  ntimestamp(noisy)
  n2cat(noisy, "Starting.\n\n")
  ntimestamp(noisy)
  n2cat(noisy, cores, " cores ", length(spplist)/length(output.nodes),
        " species,", length(output.nodes), "nodes\n\n")
  out.data.list[[n]] <- parLapplyLB(cl, spplist, lapplyCondP)
  stopCluster(cl)
  ntimestamp(noisy)
  n2cat(noisy, "Done.\n\n")
  #out.data <- as.data.frame(matrix(unlist(foo), ncol=4, byrow=TRUE))
  out.data <- as.data.frame(matrix(unlist(out.data.list), ncol=4, byrow=TRUE))
  names(out.data) <- c("Species", "Node", "Favorable", "Unfavorable")
  
  nodes=unique(out.data$Node)
  nodes=as.character(nodes)
  nodes=c("Species", nodes)
  ct=0
  iti = splist[1]
  for (s in splist) {
    jnk=out.data[out.data$Species==s,]
    jnk=as.numeric(as.character(jnk$Favorable))
    jnk=c(s, jnk)
    if (ct==0){
      out.data2=jnk
      out.data2=as.data.frame(t(out.data2))
      ct=1
    }else{
      out.data2=rbind(out.data2, t(jnk))
    }
  }
  #out.data2=out.data2[,-1]
  names(out.data2) <-nodes 
  out.data=out.data2
  out.data[,"vulnerability"]=(1-((as.double(as.character(out.data[,"Resist"]))+as.double(as.character(out.data[,"Migrate"])))/2))*(1-as.double(as.character(out.data[,"Micro_refugia"])))  
  out.data[,"additive"]=((1-((as.double(as.character(out.data[,"Resist"]))+as.double(as.character(out.data[,"Migrate"])))/2))+(1-as.double(as.character(out.data[,"Micro_refugia"]))))/2  
  out.data[,"transformed"]=acos(1-as.double(as.character(out.data[,"vulnerability"])))/(0.5*pi)  
  write.csv(out.data, csv_out_data, row.names=FALSE)
}

scores <- read.csv(csv_out_data, stringsAsFactors=FALSE)
hist(scores[,2])
hist(scores[,3])
hist(scores[,4])
hist(scores[,5])


