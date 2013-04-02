threshold_table <- read.csv("threshold_vars_and_vals.csv", stringsAsFactors=FALSE)
rld <- read.csv("allspp.csv", stringsAsFactors=FALSE)


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

## these have dashes in them -- set dashes to zero
## function to take a vector of strings, replace "-" with "0" and
## return the numeric values
dash20s <- function(x) {
  x[x=="-"] <- "0"
  return(as.numeric(x))
}
#Total_zone_mean_inv_suitability

## loop through the model nodes, identify associated variables, categorize
## them as appropriate and store the results in the qrld dataframe
n=threshold_table$Var[17] 
for (n in threshold_table$Var) {  
  varloc=which(threshold_table$Var==n)#node_name=vlist$node_names[vlistrow]

  b <- 0.5
  temp_res= q2levels(rld[, n], b)
  assign("thresholds", temp_res[[2]])
  threshold_table[varloc,2]=thresholds
  rm(temp_res, thresholds, b)
  
  b <- c(0.33, 0.66)
  temp_res = q3levels(rld[, n], b)
  assign("thresholds", temp_res[[2]])
  threshold_table[varloc,3:4]=thresholds
  rm(temp_res, thresholds, b)
  
  b <- c(0.2, 0.4, 0.6, 0.8)
  temp_res = q5levels(rld[, n], b)
  assign("thresholds", temp_res[[2]])
  threshold_table[varloc,5:8]=thresholds
  rm(temp_res, thresholds, b)
}

write.csv(threshold_table, "threshold_vars_and_vals.csv", row.names=FALSE)
