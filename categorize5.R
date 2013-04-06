require(XML)
library(ggplot2)

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
  quant_data[x < rep(y[1],length(x))] <- lv[1]
  quant_data[x > rep(y[2],length(x))] <- lv[3]
  return(list(quant_data, thresholds))
}
q2levels <- function(x, q=0.5, lv=c("Low", "High")) {
  y <- quantile(x, q, na.rm=TRUE)  
  y[y==min(x, na.rm = TRUE)]=min(x, na.rm = TRUE)+0.0001
  y[y==max(x, na.rm = TRUE)]=max(x, na.rm = TRUE)-0.0001
  thresholds=y
  quant_data <- rep(lv[1], length(x))
  quant_data[is.na(x)] <- "none"
  quant_data[x > rep(y,length(x))] <- lv[2]
  return(list(quant_data, thresholds))}

q5levels <- function(x, q=c(0.2, 0.4, 0.6, 0.8), lv=c("Very_small", "Small", "Medium", "Large", "Very_large")) {
  y <- quantile(x, q, na.rm=TRUE)
  y[y==min(x, na.rm = TRUE)]=min(x, na.rm = TRUE)+0.0001
  y[y==max(x, na.rm = TRUE)]=max(x, na.rm = TRUE)-0.0001
  thresholds=y
  quant_data <- rep(lv[3], length(x))
  quant_data[is.na(x)] <- "none"
  quant_data[x < rep(y[2],length(x))] <- lv[2]
  quant_data[x < rep(y[1],length(x))] <- lv[1]
  quant_data[x > rep(y[3],length(x))] <- lv[4]
  quant_data[x > rep(y[4],length(x))] <- lv[5] 
  return(list(quant_data, thresholds))}

Abs_q3levels <- function(x, q=c(0.33, 0.66), lv=c("Low", "Medium", "High")) {
  quant_data <- rep(lv[2], length(x))
  quant_data[is.na(x)] <- "none"
  quant_data[x < rep(q[1],length(x))] <- lv[1]
  quant_data[x > rep(q[2],length(x))] <- lv[3]
  thresholds=q
  return(list(quant_data, thresholds))}

Abs_q2levels <- function(x, q=0.5, lv=c("Low", "High")) {
  quant_data <- rep(lv[1], length(x))
  quant_data[is.na(x)] <- "none"
  quant_data[x > rep(q, length(x))] <- lv[2]
  thresholds=q
  return(list(quant_data, thresholds))}

Abs_q5levels <- function(x, q=c(0.2, 0.4, 0.6, 0.8), lv=c("Very_small", "Small", "Medium", "Large", "Very_large")) {
  quant_data <- rep(lv[3], length(x))
  quant_data[is.na(x)] <- "none"
  quant_data[x < rep(q[2], length(x))] <- lv[2]
  quant_data[x < rep(q[1], length(x))] <- lv[1]
  quant_data[x > rep(q[3], length(x))] <- lv[4]
  quant_data[x > rep(q[4], length(x))] <- lv[5]    
  thresholds=q
  return(list(quant_data, thresholds))}

plot_hist_fx <- function(v, x, q, lv, graph_xlabel) {
  jpeg_name=paste(out_graph_dir,project_name,"_",v,".jpg", sep = "")
  jpeg(jpeg_name,
       width = 5, height = 5, units = "in",
       pointsize = 12, quality = 90, bg = "white", res = 300)
  hist(x, main="", breaks = 25, xlab=graph_xlabel)
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

plot_multi_hist_fx <- function(v, x, q, lv, temp_colors, temp_legends, temp_lty, temp_border, graph_xlabel) { #graph_name, temp_hists, thresholds, categories, temp_colors, temp_legends
  jpeg_name=paste(out_graph_dir,project_name,"_",v,".jpg", sep = "")
  jpeg(jpeg_name,
       width = 5, height = 5, units = "in",
       pointsize = 12, quality = 90, bg = "white", res = 300)
  for (jkj in 1:length(temp_hists)){
    color=temp_colors[jkj]
    ltyy=temp_lty[jkj]
    borderr=temp_border[jkj]
    if (jkj==1){
      plot(get(temp_hists[jkj]), main="", col=color, lty = ltyy, border = borderr, xlab=graph_xlabel)  # first histogram
    }else{  
      plot(get(temp_hists[jkj]), main="", col=color, lty = ltyy, border = borderr, add=T)  # first histogram
    }   
  }
  
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
  
  legend('topright',temp_legends,
         fill = temp_colors, bty = 'n',
         border = NA)
  dev.off()  
  return()
}

plot_multi_hist_fx2 <- function(v, x, q, lv, temp_colors, temp_legends, graph_xlabel) { #graph_name, temp_hists, thresholds, categories, temp_colors, temp_legends
  jpeg_name=paste(out_graph_dir,project_name,"_",v,".jpg", sep = "")
  jpeg(jpeg_name,
       width = 5, height = 5, units = "in",
       pointsize = 12, quality = 90, bg = "white", res = 300)
  plot.new()
  ggplot(x, aes(gg_vals, fill = gg_zone)) + geom_bar(pos="dodge")
  
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

# ## Scaling of existing variables
# ## Calculated metrics
# rld$Zone_slope_stdMG <- rld$Zone_slope_stdMG / rld$Zone_slope_medMG
# rld$Zone_slope_stdMR <- rld$Zone_slope_stdMR / rld$Zone_slope_medMR
# rld$Zone_slope_stdTL <- rld$Zone_slope_stdTL / rld$Zone_slope_medTL

# ## Qprop_zone_lava_area
# ## Note that zone_lavaflow_area MR has an underscore; the others don't

# 
# ## Qzone_good_quality_habitat
# rld$MR_good <- rld$MR_good / rld$zone_areaMR
# rld$TL_good <- rld$TL_good / rld$zone_areaTL
# rld$MG_good <- rld$MG_good / rld$zone_areaMG
# 
# ## Protected_area
# rld$MR_protected_area <- rld$MR_protected_area / rld$zone_areaMR
# rld$TL_protected_area <- rld$TL_protected_area / rld$zone_areaTL
# rld$MG_protected_area <- rld$MG_protected_area / rld$zone_areaMG
# 
# ## Ung_free_Areas
# rld$MR_Ung_free_Areas <- rld$MR_Ung_free_Areas / rld$zone_areaMR
# rld$TL_Ung_free_Areas <- rld$TL_Ung_free_Areas / rld$zone_areaTL
# rld$MG_Ung_free_Areas <- rld$MG_Ung_free_Areas / rld$zone_areaMG
# 
# ## Proportion of habitat irreversibly destroyed by climate change
# rld$MRzone_slr <- rld$MRzone_slr / rld$zone_areaMR
# rld$MGzone_slr <- rld$MGzone_slr / rld$zone_areaMG
# rld$TLzone_slr <- rld$TLzone_slr / rld$zone_areaTL
# 
# ## Proportion of habitat area actually uninhabitable
# rld$MR_ugly <- rld$MR_ugly / rld$zone_areaMR
# rld$MG_ugly <- rld$MG_ugly / rld$zone_areaMG
# rld$TL_ugly <- rld$TL_ugly / rld$zone_areaTL

## Fragmentation metrics
# rld$MRF_fragmentation <- rld$MRzone_edge / (rld$MRzone_core+rld$MRzone_edge)
# rld$Tol_fragmentation <- rld$TLzone_edge / (rld$TLzone_core+rld$TLzone_edge)
# rld$Mig_fragmentation <- rld$MGzone_edge / (rld$MGzone_core+rld$MGzone_edge)

## remove information for nodes without variables
vlist <- vlist[!is.na(vlist$Variable), ]

## simplify rld to contain only variables associated with nodes
## (and sp_name and sp_code)
nodevars <- vlist$Variable[vlist$Node %in% nodes]
rld <- rld[, c("sp_name", "sp_code", nodevars)]

##debug code
# for (n in nodevars){
#   jnk=rld[,n]
#   cat(n," found", "\n")
# }

## the nodes for which we have data
datanodes <- vlist$Node[vlist$Node %in% nodes]


if (plot_hist){
  #COMPOSITE GRAPHS
  graph_indices=vlist$graph
  #graphs=unique(graph_indices)
  #x <- factor(graph_indices, levels = min(graph_indices, na.rm=T):max(graph_indices, na.rm=T))
  #graph_counts=table(x)
  graph_index=c(1:max(graph_indices, na.rm=T))
  graph_counts=tabulate(graph_indices, max(graph_indices, na.rm=T))
  graph_table=cbind(graph_index,graph_counts)
  graph_table=as.data.frame(graph_table)
  graph_table=graph_table[graph_table$graph_counts>1,]
  graph_indices=graph_table$graph_index
  hist_colors=c(rgb(1,0,0,1/5), rgb(0,1,0,2/5), rgb(0,0,1,1/5))
  hist_legends=c("Lost","Overlap","Gained")
  hist_lty=c(1,2,3)
  hist_line_color=c("red","green","blue")
  
  comp_graph_index = graph_indices[7]
  
  for (comp_graph_index in graph_indices){
    vars_to_plot=vlist$Node[vlist$graph == comp_graph_index]
    vars_to_plot=vars_to_plot[!is.na(vars_to_plot)]
    ko=1
    temp_hists=c()
    temp_colors=c()
    temp_legends=c()
    temp_lty = c()
    temp_border = c()
    zones_data=c()
    vs <- vlist$Variable[which(vlist$Node %in% vars_to_plot)]
    minVs=min(rld[, vs],na.rm=T)
    maxVs=max(rld[, vs],na.rm=T)
    breaks=seq(minVs,maxVs,(maxVs-minVs)/25)
    n = vars_to_plot[1]
    for (n in vars_to_plot) {
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
      if (ko==1){
        graph_name=vlist$graph_name[vlistrow]
        graph_xlabel=vlist$graph_units[vlistrow]
        threshold_name=vlist$threshold_name[vlistrow]
        jnk=which(threshold_table$Var %in% threshold_name)
        if (length(categories) == 2) {
          thresholds <- threshold_table[jnk,5:8]
        }  
        if (length(categories) == 3) {
          thresholds <- threshold_table[jnk,3:4]
        } 
        if (length(categories) == 5) {
          thresholds <- threshold_table[jnk,2]
        }
      }
      graph_zone=vlist$graph_zone[vlistrow]
      temp_hist=paste("p",graph_zone,sep="")
      r=hist(rld[, v], main="", breaks = breaks)
      lines(r, lty = 3, border = "purple")
      assign(temp_hist,r)
      
      temp_hists=c(temp_hists, temp_hist)
      temp_colors=c(temp_colors,hist_colors[graph_zone])
      temp_legends=c(temp_legends, hist_legends[graph_zone])
      temp_lty = c(temp_lty,hist_lty[graph_zone])
      temp_border = c(temp_border,hist_line_color[graph_zone])
      
      zone_data <- data.frame(gg_vals = rld[, v])
      zone_data$gg_zone <- hist_legends[graph_zone]
      zones_data <- rbind(zones_data, zone_data)
      
      ko=ko+1
    }
    ##graph_name, temp_hists, thresholds, categories, temp_colors, temp_legends  
    plot_multi_hist_fx(graph_name, temp_hists, thresholds, categories, temp_colors, temp_legends, temp_lty, temp_border, graph_xlabel)#, temp_legends
    #plot_multi_hist_fx2(graph_name, zones_data, thresholds, categories, temp_colors, temp_legends)#, temp_legends
    rm(p1, p2, p3)
  }
}

## create a dataframe to hold the quantified data. Column names will be
## the model nodes, rows will be individual species. This will make a
## matrix appropriately formatted for supplying conditional cases to catnet.
qrld <- matrix("none", nrow=nrow(rld), ncol=length(nodes)+2)
qrld <- as.data.frame(qrld, stringsAsFactors=FALSE)
names(qrld) <- c("sp_name", "sp_code", nodes)
qrld$sp_name <- rld$sp_name
qrld$sp_code <- rld$sp_code

## loop through the model nodes, identify associated variables, categorize
## them as appropriate and store the results in the qrld dataframe
n=datanodes[8]
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
  graph_xlabel=vlist$graph_units[vlistrow]
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
    if (plot_hist){
      plot_hist_fx(node_name, rld[, v],thresholds,categories,graph_xlabel)
    }
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
    if (plot_hist){
      plot_hist_fx(node_name, rld[, v],thresholds,categories,graph_xlabel)
    }
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
    if (plot_hist){
      plot_hist_fx(node_name, rld[, v],thresholds,categories,graph_xlabel)
    }
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
write.csv(rld, uncat.data.file, row.names=FALSE)
