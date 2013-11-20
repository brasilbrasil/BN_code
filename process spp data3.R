rm(list=ls())
## This is a list of flags indicating what you want done when you
## run this particular file -- useful if you done some computationally
## intensive step and want to skip it (set to FALSE) on later runs.
## In general each step is dependent upon its predecessors, so if you
## set a step to TRUE, you should probably set the steps before it
## to true as well
#requires catnet, xml (install.packages("XML", repos = "http://www.omegahat.org/R"))

## working directory
server=FALSE
if (server){
  wd <- "//10.0.0.5/data2$//PICCC_analysis/BN_vulnerability/Full Process template/"
  code_loc="Y:/code/BN_code/"
}else{
  wd <- "D:/Dropbox/current work/USGS Science/0-ongoing/VAs/HI spp VA/BN vulnerability/Full Process template"
  #wd <- "C:/Users/lfortini/Dropbox/USGS/Science/0-ongoing/VAs/HI spp VA/BN vulnerability/Full Process template/" #C:\Users\lfortini\Dropbox\USGS\Science\0-ongoing\VAs\HI spp VA\BN vulnerability\Full Process template
  code_loc="D:/Dropbox/code/BN_code/"
}
setwd(wd)

project_name="unknwnfacs_eqwgts_priors_thirddispersion" # _thirddispersion #_min_habqual _traitsoff
categorize.GIS.data <- FALSE
calc_priors= FALSE
plot_hist = F ##if categorizing data, this switch controls whether histograms of all model
                 ##variables will be created, with the breakpoints for the categories ploted along
create_factor_mean_table=FALSE
add.spp.to.GeNIe.model <- FALSE # this step could reasonably be skipped
create.catnet.model <- FALSE
calculate.conditional.ps <- F
merge_all_results_and_data=F
do_tables=T
expert_comparison=FALSE
do_correl_analyses= FALSE
create_response_histograms=FALSE
vulnerability_contrasts=T

threshold_table <- read.csv("threshold_vars_and_vals.csv", stringsAsFactors=FALSE)
sp_list_offset=NULL# NULL #c(1,10) #to turn off, put NULL #sp_list_offset=NULL
revert_sp_order=FALSE
csv_out_data=paste("results/",project_name,"_spp_vulnerability_scores.csv", sep="")
overwrite_ps= FALSE
## calculate conditional probabilities using local-socket parallel
## processes? If cores is left at NULL, attempt to use only as
## many physical cores are present. If noisy is greater than zero the
## script will print timestamps and other diagnostics about what
## is going on. Set noisy to 0 to shut things up.
calculate.parallel <- FALSE
config_file=NULL#"config_file4.r"

n_instances=length(system('tasklist /FI "IMAGENAME eq Rscript.exe" ', intern = TRUE))-3
cpucores=as.integer(Sys.getenv('NUMBER_OF_PROCESSORS'))

if (!is.null(config_file)){
  source(paste(code_loc,config_file, sep=""))
}

cores <- NULL
noisy <- 3

## This file is the raw data for each species. The column "sp_name" should
## contain a unique identifier for each species. There may be a sample
## species data file "all_spp_values.xlsx" in which I change the names
## of the many "Unknown" species to "Unknown" with a number, and change
## some spaces in column names to underscores before saving it as a
## .csv file. Some further processing of the data is done in "categorize.R"
data.file <- "allspp.csv"

## a table of variables in the datafile linking them to nodes
## in the GeNiE model. It also contains space for parameters to
## specify which quantiles to use in assigning categories
## (default is to use median for two categories, 1st and 3rd
## quartiles for 3 categories)
variables.file <- "variables.csv"

## a table with a column "Species" containing the species we
## want to add cases for. GeNIe bogs down with too many cases, and
## sometimes we'll only want updates for a subset of species
sppinterest.file <- "spinterest.csv"

## the GeNiE model to use when analyzing the data, and to use as
## a source when creating the catnet version in R
model.file <- "conceptual_model05_unknownfactors_equalweights_priors_thirddispersion.xdsl"

## A sample model file to use when adding cases to the GeNIe model.
## I usually generate this by deleting all cases from the current
## model after deleting all but a single case (which I re-name to
## "delete me" to avoid species name collisions)
## Right now the code needs a blank, unpopulated GeNiE model
## with a single placeholder case
model.file.in <- "conceptual_model05_unknownfactors_equalweights_priors_thirddispersion_template.xdsl"

## this is the file name to use for the output GeNIe model
model.file.out <- paste(project_name,"_test_out.xdsl", sep="")

## After the GIS data has been categorized for each species,
## save it out to this .csv file
cat.data.file <- paste(project_name, "_qrld.csv",sep="")
uncat.data.file <- paste(project_name, "_rld.csv",sep="")


## These are the node values we are intested in. Note that
## "Resist" is the model node name usually interpreted as
## "Tolerate"

#output.nodes <- c("Resist", "Migrate", "Micro_refugia")
output.nodes <- c("Resist", "Migrate", "Micro_refugia","Effective_MRF_area", "Effective_Tol_zone_area", "Effective_Mig_area",
                  "Habitat_qual_MRF", "Tol_Zone_Habitat_qual", "Mig_Zone_Habitat_qual", "Dispersion")

## Some functions to take a previously set flag to determine
## how much progress to report
dir.create("results/", showWarnings=FALSE)
dir.create("spp_csvs/", showWarnings=FALSE)
ntimestamp <- function(noisy) {
  if (noisy  > 0)
    timestamp()
}
n2cat <- function(noisy, ...) {
  if (noisy > 1)
    cat(..., sep="")
}
n3cat <- function(noisy, ...) {
  if (noisy > 2)
    cat(..., sep="")
}

noisy <- 1
if (categorize.GIS.data) {
  n2cat(noisy, "Categorizing input data.\n")
  ntimestamp(noisy)
  source(paste(code_loc,"categorize5.R", sep=""))
  ntimestamp(noisy)
}

if (create_factor_mean_table) {
  n2cat(noisy, "Creating factor mean table.\n")
  ntimestamp(noisy)
  source(paste(code_loc,"make_factor_table.r", sep=""))
  ntimestamp(noisy)
}


if (calc_priors){
  source(paste(code_loc,"calc priors.R", sep=""))    
}
noisy <- 3
if (add.spp.to.GeNIe.model) {
  n2cat(noisy, "Adding cases to GeNIe model.\n")
  ntimestamp(noisy)
  source(paste(code_loc,"add cases to GeNIe.R", sep=""))
  ntimestamp(noisy)
}
if (create.catnet.model) {
  ntimestamp(noisy)
  n2cat(noisy, "Creating catnet version of GeNIe model.\n")
  source(paste(code_loc,"create catnet model.R", sep=""))
  #ntimestamp(noisy)
}
if (calculate.conditional.ps) {
  ## much progress reporting is done within "calculate conditional ps.R"
  source(paste(code_loc,"calculate conditional ps3.R", sep=""))
}
if (merge_all_results_and_data){
  source(paste(code_loc,"merge_all_data.R", sep=""))
}

if (do_tables){
  source(paste(code_loc,"do_tables.R", sep=""))
}

if (expert_comparison){
  dir.create("expert_comparison/", showWarnings=FALSE)
  source(paste(code_loc,"expert_vs_model_ranks_integrated.R", sep=""))
}

if (do_correl_analyses){
  source(paste(code_loc,"correls3.R", sep=""))
}

if (create_response_histograms){
  dir.create("graphs/vuln_graphs/",showWarnings=FALSE)
  source(paste(code_loc,"vulnerability_histograms.R", sep=""))
}

if (vulnerability_contrasts){
  dir.create("comparisons/",showWarnings=FALSE)
  source(paste(code_loc,"vulnerability_contrasts7.r", sep=""))
}
