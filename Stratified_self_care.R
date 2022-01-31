
## Set the root directory to look for source code.
SOURCE_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"
## Set the root location on the user's local machine to save output files.
OUTPUT_ROOT = "/Users/aliya/my_docs/proj/bayesian_meta_analysis/"



source(paste(SOURCE_ROOT, "BayesUpdateStepByStep.R", sep=""), local = TRUE) # this function (BayesUpdateStepByStep) runs the Bayesian meta-analysis that combines qualitative and quantitative evidence 

source(paste(SOURCE_ROOT, "Summary_stats_table_qual_and_quant.R", sep=""), local = TRUE) # this function (BayesUpdateStepByStep) runs the Bayesian meta-analysis that combines qualitative and quantitative evidence, and outputs the summary stats only 


x = read.csv(paste(SOURCE_ROOT, "input.csv", sep="")) #to perform the analysis we require this data for all indexed functions which were indexed by the name of the included constructs (eg., self-efficacy, social support). This is done so the analysis is parsled out for each construct separately. 

data = read.csv(paste(SOURCE_ROOT, "QuantData_CheckedForAccuracy_20March2020.csv", sep=""))  #data extracted from  the quantitative studies, the file lists all data including the data that was not used for the meta-analysis. the data not included in the meta-anslysis is for the cases when insufficient data was reported in the article for it to be pooled in the meta-analysis (for example mean but no SD or variance etc)
JaarsmaInternationalStudy = read.csv(paste(SOURCE_ROOT, "HyperPriorData.csv", sep="")) #data used for eliciting the hyperprior (general physical activity levels in HF estimated from a large internaitonal study (Jaarsma et al., 2013)


#the coplinace rate with the self-care: self-care item : do you exercise regularly? as part of the self-care scale. 

data_self_care = subset(data, data$PA_Varme == "ComplienceRate" &  data$Construct == "SocialSupport")





Results_SocialSupport =   BayesUpdateStepByStep(x =x, Construct = "SocialSupport"  )

Summary_Results_SocialSupport= Summary_stats_table_qual_and_quant(x =x, Construct = "SocialSupport")



write.table(Results_SocialSupport, file = paste(OUTPUT_ROOT, "Results_self_care_stratified_qual_quant.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )

write.table(Summary_Results_SocialSupport, file = paste(OUTPUT_ROOT, "Summary_Results_SocialSupport_self_care_stratified_qual_quant.csv", sep=""), append = FALSE, quote = TRUE, sep = ", ",
            eol = "\r", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "" )


density_by_Construct = function(data, Construct){
  index = data$Construct == Construct
  logOddsRatio = seq( -2, 3 , length=1000)
  filtered_data = filter(data, Construct == data[index,]$Construct)
  
  # the results of the expert elicitation task 
  Prior_qual_density = dnorm(logOddsRatio,
                             filtered_data$logOR_expert_elicitation_task, 
                             filtered_data$variance_expert_elicitation_task)
  
  # posterior resulted from updating hyperprior with the results of the expert elicitaiton task 
  Posterior_qual_only = dnorm(logOddsRatio, 
                              filtered_data$Posterior_qual_only_mean,  
                              filtered_data$Posterior_qual_only_variance)
  
  # likelihood (quantitative evidence only)
  Likelihood = dnorm(logOddsRatio, 
                     filtered_data$LOGOdds_Ratio_quant, 
                     filtered_data$variance_quant)
  
  
  # the posterior resulted from updating prior with likelihood 
  posterior_QualplusQuant = dnorm(logOddsRatio, 
                                  filtered_data$posterior_QualplusQuant_mean,
                                  filtered_data$posterior_QualplusQuant_variance)
  
  # the posterior resulted from updating hyperprior with prior and then with likelihood 
  posterior_All = dnorm(logOddsRatio, 
                        filtered_data$posterior_All_mean,
                        filtered_data$posterior_All_variance)
  
  df = data.frame(logOddsRatio, Construct, Prior_qual_density, Posterior_qual_only, Likelihood, posterior_QualplusQuant, posterior_All)
  colnames(df) = c("logOddsRatio", "Construct", "Prior_qual_density", "Posterior_qual_only", "Likelihood",  "posterior_QualplusQuant", "posterior_All")
  return(df)
}


SocialSupport_density_by_Construct = density_by_Construct(data = Results_SocialSupport, Construct = "SocialSupport")

height = c(rep(10, 1000))

SocialSupport_density_by_Construct = cbind(SocialSupport_density_by_Construct, height)

SocialSupport_density_by_Construct


#plotting the results of the expert elicitation task 
Plot_Prior_qual_density = ggplot(SocialSupport_density_by_Construct, aes(x = logOddsRatio, y = Construct, height=Prior_qual_density, group = Construct)) +
  geom_density_ridges(stat = "identity", scale = 1) +
  xlim(-2, 3 )

print(Plot_Prior_qual_density)

#plotting posterior resulted from updating hyperprior with the results of the expert elicitaiton task 
#Plot_Posterior_qual_only = ggplot(density_ALL_Construct, aes(x = logOddsRatio, y = Construct, height=Posterior_qual_only, group = Construct)) +
#  geom_density_ridges(stat = "identity", scale = 1) +
# xlim(-2, 2.5  )

#print(Plot_Posterior_qual_only)

#plotting likelihood (quantitative evidence only)
Plot_Likelihood = ggplot(SocialSupport_density_by_Construct, aes(x = logOddsRatio, y = Construct, height=Likelihood, group = Construct)) +
  geom_density_ridges(stat = "identity", scale = 1) +
  xlim(-2, 2  )

print(Plot_Likelihood)

#plotting the posterior resulted from updating prior with likelihood 
Plot_posterior_QualplusQuant = ggplot(SocialSupport_density_by_Construct, aes(x = logOddsRatio, y = Construct, height=posterior_QualplusQuant, group = Construct)) +
  geom_density_ridges(stat = "identity", scale = 1) +
  xlim(-2, 3  )

print(Plot_posterior_QualplusQuant)

#plotting the posterior resulted from updating hyperprior with prior and then with likelihood 
#Plot_posterior_All = ggplot(density_ALL_Construct, aes(x = logOddsRatio, y = Construct, height=posterior_All, group = Construct)) +
#  geom_density_ridges(stat = "identity", scale = 1) +
#  xlim(-2, 2.5 )

#print(Plot_posterior_All)




SocialSupport_density_by_Construct
head(SocialSupport_density_by_Construct)


All_constructs_prior = select(SocialSupport_density_by_Construct, logOddsRatio, Construct, Prior_qual_density) 

All_constructs_likelihood = select(SocialSupport_density_by_Construct, logOddsRatio, Construct, Likelihood)

All_constructs_posterior = select(SocialSupport_density_by_Construct, logOddsRatio, Construct, posterior_QualplusQuant) 


Age_density_prior = All_constructs_prior  %>% filter(Construct == 'Age')
unique(Age_density_prior$Construct)


SocialSupport_density_prior =  All_constructs_prior  %>% filter(Construct == "SocialSupport")



SocialSupport_density_likelihood = All_constructs_likelihood  %>% filter(Construct  == "SocialSupport")

SocialSupport_density_posterior = All_constructs_posterior  %>% filter(Construct  == "SocialSupport")


prior_name = rep("Qualitative evidence", times = 1000)

likelihood_name = rep("Quantitative evidence", times = 1000)

posterior_name = rep("Posterior (Qual + QUANT)", times = 1000)


distribution = c(prior_name, likelihood_name, posterior_name)


height = c(rep(10, 1000),
           rep(20, 1000), 
           rep(30, 1000))




d <- data.frame(
  logOddsRatio = SocialSupport_density_by_Construct$logOddsRatio, 
  Construct = c(SocialSupport_density_prior$Construct,
                SocialSupport_density_likelihood$Construct,
                SocialSupport_density_posterior$Construct),
  
  y = c(SocialSupport_density_prior$Prior_qual_density,
        SocialSupport_density_likelihood$Likelihood,
        SocialSupport_density_posterior$posterior_QualplusQuant),
  
  distribution = distribution, 
  
  height = height)

d$group_name = paste0(as.character(d$Construct)," ", as.character(d$distribution))
# colors for the intermediate plots were as follows: 
#prior: "#CC79A7"
#posterior: "#D55E00"
# likelihood: "#009E73"

#we want the distributions to be same color as they are in the intermediate plots so prior should be "#CC79A7" and so on. 
# however, they do not look nice together so I will pick the same colors but more pastel, Set2 is nice: 
#display.brewer.all(colorblindFriendly = TRUE)
#colors = display.brewer.pal(c(1, 2, 4), "Set2")
#print the codes for the colors: 
#brewer.pal(n = 5, name = "Set2")

##############

#colors from the set2: "#66C2A5" "#FC8D62" "#E78AC3"
Compare_distributions_plot = ggplot(d, aes(x = logOddsRatio, 
                                           y = Construct,
                                           height = y, 
                                           group = group_name, 
                                           color = distribution,
                                           fill = distribution)) +
  
  geom_density_ridges(stat = "identity",
                      scale = 1) +
  
  #scale_fill_brewer(palette = "Set2")+
  #scale_color_brewer(palette = "Set2")+
  scale_fill_manual(values = c("#FC8D62" , "#E78AC3" ,"#66C2A5"))+
  scale_color_manual(values = c("#FC8D62" , "#E78AC3" ,"#66C2A5"))+
  
  xlim(-2,3) +
  
  theme(plot.margin = margin(1, 1, 1, 1, "cm"),
        panel.grid.major = element_line(colour = "grey", size = 0.2),
        panel.grid.minor = element_line(colour = "grey", size = 0.1)) 



print(Compare_distributions_plot)


plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)


x_directory_quant <- file.path(paste(OUTPUT_ROOT))
dir.create(x_directory_quant)
file.copy(from=plots.png.paths, to=x_directory_quant)
