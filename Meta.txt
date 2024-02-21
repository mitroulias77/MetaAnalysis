rm(list = ls()) #clear list

#load packages
library(xlsx) # Excel-Packet
library(readxl) # Excel-Packet
library(psychmeta) # Meta Analysis
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%

#DataSet INPUT
dat <- read_excel("Meta-CSRD_Taxation Analysis.xlsx")
View(dat)

#Conduct a basic meta-analysis of correlation coefficients using a H&S-type approach (what Hunter and Schmidt call a "bare-bones meta-analysis")
result <- ma_r(dat$Correlation, dat$Sample, ma_method="bb", control=control_psychmeta(cred_level=0.95), sample_id=dat$study_id);
			   
#Απόδοση αποτελεσμάτων μετα-αναλυσης
summary(result)


#heterogeneity analysis - assign the heterogeneity results 
result <- heterogeneity(result) 

result$heterogeneity

as.data.frame(get_metatab(result))
 
het_result <- result$heterogeneity[[1]][[1]]$HS_method

truncate_intervals = TRUE
if (truncate_intervals==TRUE) {
  lim = 1.0
}  else {
  lim = Inf
}
# Now let's summarise all the results in a nice table!
result %>%
    get_metatab() %>%
    as_tibble() %>%
    mutate(CI_95 = sprintf("[%.2f, %.2f]", max(CI_LL_95,-lim), min(CI_UL_95,lim))) %>%
    mutate(Q = max(0,het_result$Q)) %>%
    mutate(CR_95 = sprintf("[%.2f, %.2f]", max(CR_LL_95,-lim), min(CR_UL_95,lim))) %>%  
    dplyr::select(c(k, N, mean_r, var_r, var_e,CI_95, Q)) %>%
    knitr::kable(caption = "Summary of meta-analysis results", digits = 5, align = "c",
                 col.names = c("Number of Effefcts", "Sample", "Effect Size", "Observed Total Variance","Sampling Error Variance","95% CI", sprintf("Q-statistic\u00b2"))) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"), row_label_position="c")
	

#Funnel Plot
ma_res <- plot_funnel(result)
get_plots(ma_res)
get_plots(ma_res)$funnel

#Z-Correlation transfomation - Variance computation in dataset
dat<- escalc(measure="ZCOR", ri= Correlation, ni = Sample, data = dat )

#Rank Correlation Test for Funnel Plot Asymmetry
ranktest(yi,vi, data=dat)

