## xdsl file input
xdsl.input <- file("conceptual_model05_unknownfactors_unequalweights_priors.xdsl","rt")
xdsl.str <- readLines(xdsl.input)
close(xdsl.input) #close file

cpt.open <- grep("<cpt",xdsl.str) #line numbers for new CPTs
cpt.close <- grep("</cpt>",xdsl.str) #line numbers for end of CPTs

## xlsx file (for node names)
library("gdata") #for xlsx input
data.xlsx <- read.xls("P_BN_model3_expert2.xlsx")
node.r <- grep("#",data.xlsx[,1])  #row numbers for new Node
Node.name <- as.matrix(data.xlsx[node.r,"Child"])

## OR put all node names into a txt file
#Node.name <- as.matrix(read.table("Node_names.txt",header=F))

node.n <- length(Node.name)  #number of Nodes

xdsl.out <- xdsl.str #make a copy
for(node in 1:node.n){
  node.id <- paste("<cpt id=\"",Node.name[node],"\">",sep="")
  node.id.line <- grep(node.id,xdsl.str) #find node id line in xdsl
  if(length(node.id.line)<1){cat(paste("\tNode ",node,": ",Node.name[node]," NOT FOUND!\n",sep=""));next} #if node not found -> go to next node
  else{
    prob.input <- read.csv(paste("CPTs/", Node.name[node],"_all_Ps.csv",sep=""),header=T) #read node prob csv
    prob.input <- prob.input[,2:ncol(prob.input)] #omit first pSum column
    prob.stack <- stack(prob.input)[,1] #stack columns into a vector
    prob.str <- paste("\t\t\t<probabilities>",paste(prob.stack,collapse=" "),"</probabilities>",sep="") #string of stacked columns with tags
    
    node.cpt.close <- cpt.close[which(cpt.close>node.id.line)][1] #end <cpt> line for current node
    node.prob <- grep("<probabilities>",xdsl.str[node.id.line:node.cpt.close])-1 #prob line within node
    xdsl.out[node.id.line+node.prob] <- prob.str #change prob string
    
    cat(paste("\tNode ",node,": ",Node.name[node]," updated.\n",sep="")) #for debugging
  }
}

xdsl.output <- file("test_out.xdsl","wt")
writeLines(xdsl.out,xdsl.output)
close(xdsl.output)