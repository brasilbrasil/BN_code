require(XML)

if (plot_hist){
  out_graph_dir=paste(wd, "graphs/", sep="")
  dir.create(out_graph_dir, showWarnings = FALSE, recursive = TRUE, mode = "0777")
}
## functions to take a vector of numeric values and return a vector of
## categorical values based on quantiles (or "none" for missing values)
## x = vector of numeric values
## q = vector of cutpoints for the quantile function; for two categories
##     only the first is used.
q3levels <- function(x, q=c(0.33, 0.66), lv=c("Low", "Medium", "High")) {
  y <- quantile(x, q, na.rm=TRUE)
  y[y==min(x, na.rm = TRUE)]=min(x, na.rm = TRUE)+0.0001
  y[y==max(x, na.rm = TRUE)]=max(x, na.rm = TRUE)-0.0001
  thresholds=y
  quant_data <- rep(lv[2], length(x))
  quant_data[is.na(x)] <- "none"
  quant_data[x < y[1]] <- lv[1]
  quant_data[x > y[2]] <- lv[3]
  return(list(quant_data, thresholds))
}
q2levels <- function(x, q=0.5, lv=c("Low", "High")) {
  y <- quantile(x, q, na.rm=TRUE)  
  y[y==min(x, na.rm = TRUE)]=min(x, na.rm = TRUE)+0.0001
  y[y==max(x, na.rm = TRUE)]=max(x, na.rm = TRUE)-0.0001
  thresholds=y
  quant_data <- rep(lv[1], length(x))
  quant_data[is.na(x)] <- "none"
  quant_data[x > y] <- lv[2]
  return(list(quant_data, thresholds))}

q5levels <- function(x, q=c(0.2, 0.4, 0.6, 0.8), lv=c("Very_small", "Small", "Medium", "Large", "Very_large")) {
  y <- quantile(x, q, na.rm=TRUE)
  y[y==min(x, na.rm = TRUE)]=min(x, na.rm = TRUE)+0.0001
  y[y==max(x, na.rm = TRUE)]=max(x, na.rm = TRUE)-0.0001
  thresholds=y
  quant_data <- rep(lv[3], length(x))
  quant_data[is.na(x)] <- "none"
  quant_data[x < y[2]] <- lv[2]
  quant_data[x < y[1]] <- lv[1]
  quant_data[x > y[3]] <- lv[4]
  quant_data[x > y[4]] <- lv[5] 
  return(list(quant_data, thresholds))}

Abs_q3levels <- function(x, q=c(0.33, 0.66), lv=c("Low", "Medium", "High")) {
  quant_data <- rep(lv[2], length(x))
  quant_data[is.na(x)] <- "none"
  quant_data[x < q[1]] <- lv[1]
  quant_data[x > q[2]] <- lv[3]
  thresholds=q
  return(list(quant_data, thresholds))}

Abs_q2levels <- function(x, q=0.5, lv=c("Low", "High")) {
  quant_data <- rep(lv[1], length(x))
  quant_data[is.na(x)] <- "none"
  quant_data[x > q] <- lv[2]
  thresholds=q
  return(list(quant_data, thresholds))}

Abs_q5levels <- function(x, q=c(0.2, 0.4, 0.6, 0.8), lv=c("Very_small", "Small", "Medium", "Large", "Very_large")) {
  quant_data <- rep(lv[3], length(x))
  quant_data[is.na(x)] <- "none"
  quant_data[x < q[2]] <- lv[2]
  quant_data[x < q[1]] <- lv[1]
  quant_data[x > q[3]] <- lv[4]
  quant_data[x > q[4]] <- lv[5]    
  thresholds=q
  return(list(quant_data, thresholds))}

plot_hist_fx <- function(v, x, q, lv) {
  jpeg_name=paste(out_graph_dir,v,".jpg", sep = "")
  jpeg(jpeg_name,
       width = 5, height = 5, units = "in",
       pointsize = 12, quality = 90, bg = "white", res = 300)
  hist(x, main="", breaks = 25)
  axis(1)
  yax <- axis(2, lty=0, labels=FALSE)
  ymax=max(yax)
  axis(2)
  xax <- axis(1, lty=0, labels=FALSE)
  xmax=max(xax)

  ypos=c(0.9, 0.8, 0.7, 0.6, 0.5)*ymax
  ypos=ypos[1:(length(q)+1)]
  xpos=c(0,q)
  
  ypos=ypos[xpos<=xmax]
  lv=lv[xpos<=xmax]
  xpos=xpos[xpos<=xmax]
        
  abline(v=q, col = "red")
  text(xpos,ypos,lv, pos=4)
  title(v)
  dev.off()  
  return()
}
## load un-categorized data
rld <- read.csv(data.file, stringsAsFactors=FALSE)

## some populations "wink out" from future to current range
## tranlate those to infinite
rld$FCE_distance[rld$FCE_distance == "wink out"] <- "Inf"
rld$FCE_distance <- as.numeric(rld$FCE_distance)

## these have dashes in them -- set dashes to zero
## function to take a vector of strings, replace "-" with "0" and
## return the numeric values
dash20s <- function(x) {
  x[x=="-"] <- "0"
  return(as.numeric(x))
}
rld$TL_ugly <- dash20s(rld$TL_ugly)
rld$MG_ugly <- dash20s(rld$MG_ugly)
rld$zone_aspect_stdTL <- dash20s(rld$zone_aspect_stdTL)
rld$zone_aspect_stdMG <- dash20s(rld$zone_aspect_stdMG)

## load the list of variables and corresponding nodes
vlist <- read.csv(variables.file, stringsAsFactors=FALSE)
vlist$Variable[vlist$Variable=="NA"] <- NA

## extract details of the nodes from the GeNIE model
d <- xmlInternalTreeParse(model.file)
nodes <- xpathSApply(d, "//node", xmlGetAttr, "id")
catlist <- list(NULL)
z <- 1
for (i in nodes) {
  path <- paste0("//cpt[@id='", i, "'][last()]/state[@id]")
  catlist[[z]] <- xpathSApply(d, path, xmlGetAttr, "id")
  z <- z + 1
}
rm(z, i, path)


# ## Qprop_area_dif
# rld$area_dif <- rld$sqkm_area_CCE - rld$sqkm_area_FCE
# rld$prop_area_dif <- rld$area_dif / rld$sqkm_area_CCE

## Qprop_CE_dist
rld$prop_CE_dist <- rld$FCE_distance / sqrt(rld$sqkm_area_CCE)

## Amount of overlap
rld$prop_CE_overlap <- rld$zone_areaTL / rld$sqkm_area_CCE

## Refugia proximity to mountaintops
rld$RF_proximity_to_max_height_of_Mtn <- (1-rld$prop_bioreg_near_top)

rld$prop_MR_lava_area <- rld$zone_lavaflow_area_MR 
rld$prop_TL_lava_area <- rld$zone_lavaflow_areaTL 
rld$prop_MG_lava_area <- rld$zone_lavaflow_areaMG 


## remove information for nodes without variables
vlist <- vlist[!is.na(vlist$Variable), ]

## simplify rld to contain only variables associated with nodes
## (and sp_name and sp_code)
nodevars <- vlist$Variable[vlist$Node %in% nodes]
rld <- rld[, c("sp_name", "sp_code", nodevars)]


## the nodes for which we have data
datanodes <- vlist$Node[vlist$Node %in% nodes]
graph_index=vlist$graph
graphs=unique(graph_index)


## loop through the model nodes, identify associated variables, categorize
## them as appropriate and store the results in the qrld dataframe
n=datanodes[3] #bad 26
for (n in datanodes) {
  node.id <- which(nodes %in% n)
  if (length(node.id) != 1)
    stop("Node ID either not found or found more than once!\n")
  v <- vlist$Variable[which(vlist$Node %in% n)]
  if (length(v) != 1)
    stop("Node not associated with exactly one variable!\n")
  vlistrow <- which(vlist$Node %in% n)
  if (length(vlistrow) != 1)
    stop("Node not found exactly once in vlist!\n")
  categories <- catlist[[node.id]]
  n3cat(noisy, "Node: ", n, "\tvariable: ", v, "\n")
  n3cat(noisy, "\tCategories: ", paste(categories, collapse=", "), "\n")
  ## some variables are special castes to be categorized later
  if (!vlist$Discretize[vlistrow]) {
    n3cat(noisy, "\tDon't discretize.\n")
    next # skip to the next variable
  }
  ## skip all NA columns as well
  if (sum(is.na(rld[, v])) == nrow(rld)) {
    n3cat(noisy, "\tAll NA's. Skipping.\n")
    next
  }
  ## if all zeros, stick with NA's in qrld
  if (sum(as.numeric(rld[, v]), na.rm=TRUE) == 0) {
    n3cat(noisy, "\tAll 0's. Leaving as NA's.\n")
    next
  }
  ## if the node has two categories, break it into pieces by the
  ## percentile in vlist -- or by the median as default
  node_name=vlist$node_names[vlistrow]
  if (length(categories) == 2) {
    if (vlist$standard_threshold[vlistrow]){
      b <- 0.5      
    } else {
      threshold_name=vlist$threshold_name[vlistrow]
      jnk=which(threshold_table$Var %in% threshold_name)
      b <- threshold_table[jnk,2]
    }
    if (vlist$Relative[vlistrow]) {
      if (!vlist$standard_threshold[vlistrow]){
        stop("not standard threshold with relative calc!")}
      temp_res= q2levels(rld[, v], b, categories)
    } else {
      temp_res = Abs_q2levels(rld[, v], b, categories)
    }
    assign("quant_data", temp_res[[1]])
    assign("thresholds", temp_res[[2]])
    rm("temp_res")
    qrld[, n] = quant_data 
    plot_hist_fx(node_name, rld[, v],thresholds,categories)
    rm(quant_data, thresholds, b)
    
    next
  }
  ## for nodes with three categories. Models with nodes having
  ## more than three levels well necessitate expanding this code
  if (length(categories) == 3) {
    if (vlist$standard_threshold[vlistrow]){
      b <- c(0.33, 0.66)
    } else {
      threshold_name=vlist$threshold_name[vlistrow]
      jnk=which(threshold_table$Var %in% threshold_name)
      b <- threshold_table[jnk,3:4]
    }
    
    if (vlist$Relative[vlistrow]) {
      if (!vlist$standard_threshold[vlistrow]){
        stop("not standard threshold with relative calc!")}
      temp_res = q3levels(rld[, v], b, categories)
    } else {
      temp_res = Abs_q3levels(rld[, v], b, categories)
      }
    assign("quant_data", temp_res[[1]])
    assign("thresholds", temp_res[[2]])
    rm("temp_res")
    qrld[, n] = quant_data 
    plot_hist_fx(node_name, rld[, v],thresholds,categories)
    rm(quant_data, thresholds, b)
    next        
  }

  ## for nodes with three categories. Models with nodes having
  ## more than three levels well necessitate expanding this code
  if (length(categories) == 5) {
    if (vlist$standard_threshold[vlistrow]){
      b <- c(0.2, 0.4, 0.6, 0.8)
    } else {
      threshold_name=vlist$threshold_name[vlistrow]
      jnk=which(threshold_table$Var %in% threshold_name)
      b <- threshold_table[jnk,5:8]
    }
    if (vlist$Relative[vlistrow]) {
      if (!vlist$standard_threshold[vlistrow]){
        stop("not standard threshold with relative calc!")}
      temp_res = q5levels(rld[, v], b, categories)
    } else {
      temp_res = Abs_q5levels(rld[, v], b, categories)
    }
    assign("quant_data", temp_res[[1]])
    assign("thresholds", temp_res[[2]])
    rm("temp_res")
    qrld[, n] = quant_data 
    plot_hist_fx(node_name, rld[, v],thresholds,categories)
    rm(quant_data, thresholds, b)
    next
  }
  
  stop("Variable ", v, " has an undefined number of categories.")
}

## Special cases
## These need to be discretized separately. They should have been skipped
## (Discretize == FALSE) in the above loop.

## Pioneer species
qrld$Pioneer_species <- rep(NA, nrow(qrld))
qrld$Pioneer_species[rld$Sp_pioneer_status == 0] <- "No"
qrld$Pioneer_species[rld$Sp_pioneer_status == 1] <- "Yes"

## Persistent in invaded habitat
qrld$Persistence_in_invaded_landscape <- rep(NA, nrow(qrld))
qrld$Persistence_in_invaded_landscape[rld$persist_in_alien_hab == 0] <- "No"
qrld$Persistence_in_invaded_landscape[rld$persist_in_alien_hab == 1] <- "Yes"

## Winkout
qrld$Winkout <- rep(NA, nrow(qrld))
qrld$Winkout[rld$winkout == 0] <- "No"
qrld$Winkout[rld$winkout == 1] <- "Yes"

## CE overlap
qrld$CE_overlap<- rep(NA, nrow(qrld))
qrld$CE_overlap[rld$CE_overlap == 0] <- "No"
qrld$CE_overlap[rld$CE_overlap == 1] <- "Yes"

write.csv(qrld, cat.data.file, row.names=FALSE)
