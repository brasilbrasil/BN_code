## this simple code just goes through the individual csv files and recomputes the transformed vulnerability using the new formula (see below)


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
    jnk <- read.csv(sp_csv, stringsAsFactors=FALSE)
    out.data[s, ]=jnk[1:ncols]
    
    cat(s, " correcting... \n")
    out.data[s,"vulnerability"]=(1-((as.double(out.data[s,"Resist"])+as.double(out.data[s,"Migrate"]))/2))*(1-as.double(out.data[s,"Micro_refugia"]))  
    out.data[s,"additive"]=((1-((as.double(out.data[s,"Resist"])+as.double(out.data[s,"Migrate"]))/2))+(1-as.double(out.data[s,"Micro_refugia"])))/2  
    #out.data[s,"transformed"]=acos(1-as.double(out.data[s,"vulnerability"]))/(0.5*pi) #old transformation
    out.data[s,"transformed"]=asin(as.double(out.data[s,"vulnerability"]))^(1/3)*2/pi
    
    jnk=out.data[s,1:ncols]
    write.csv(jnk, sp_csv, row.names=FALSE)
    names(out.data) <- c(output.nodes, "vulnerability", "additive", "transformed")
  }
  ntimestamp(noisy)
  n2cat(noisy, "Done.\n")
  write.csv(out.data, csv_out_data, row.names=TRUE)
} 



