categorize.GIS.data <- FALSE
calc_priors= FALSE
threshold_table <- read.csv("threshold_vars_and_vals.csv", stringsAsFactors=FALSE)
plot_hist = FALSE ##if categorizing data, this switch controls whether histograms of all model
##variables will be created, with the breakpoints for the categories ploted along
add.spp.to.GeNIe.model <- FALSE # this step could reasonably be skipped
create.catnet.model <- FALSE
calculate.conditional.ps <- TRUE
sp_list_offset=c(250,500) #NULL, c(400,1000) #to turn off, put NULL #sp_list_offset=NULL
revert_sp_order=FALSE
csv_out_data="results/selected_spp_vulnerability_scores.csv"
overwrite_ps= FALSE
## calculate conditional probabilities using local-socket parallel
## processes? If cores is left at NULL, attempt to use only as
## many physical cores are present. If noisy is greater than zero the
## script will print timestamps and other diagnostics about what
## is going on. Set noisy to 0 to shut things up.
calculate.parallel <- FALSE
do_correl_analyses= FALSE
