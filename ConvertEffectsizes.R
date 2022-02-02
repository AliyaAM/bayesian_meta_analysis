


library(compute.es)
library(dplyr)


#This function computes effect sizes as OR from the reported summary statistic across the included studies 

#read data extracted from the individual included papers: 

ConvertEffectsizes = function(data){

#compute from the F-value 
results_estimated_from_F = data.frame()
#compute from the r coefficient 
results_estimated_from_r = data.frame()
#compute from teh reported mean and SD
results_estimated_from_mean = data.frame()

#cmpute OR from N/frequencies 

results_estimated_Binary = data.frame()

row_id = 1

for (estimate_type in data$estimate_type) {
  
  if (estimate_type =="F_value"){
    estimated_from_F = fes(f = data$F_multi[row_id],
                           n.1 = data$N[row_id],
                           n.2 = data$N[row_id], 
                           level = 95, cer = 0.2,
                           dig = 2,
                           verbose = FALSE)
    
    estimated_from_F$Author = data$Author[row_id]
    estimated_from_F$Year = data$Year[row_id]
    estimated_from_F$Construct = data$Construct[row_id]
     
    
    estimated_from_F$varLOR =  (pi^2*estimated_from_F$var.d)/(3)
    estimated_from_F = cbind(estimated_from_F$varLOR, estimated_from_F) 
   
    
    results_estimated_from_F = rbind(results_estimated_from_F, estimated_from_F)
  }
  
  
  if (estimate_type =="r") {
    estimated_from_r = res(r = data$r_uni[row_id], 
                           n = data$N[row_id],
                           level = 95,
                           verbose = FALSE)
    
    
    
    estimated_from_r$Author = data$Author[row_id]
    estimated_from_r$Year = data$Year[row_id]
    estimated_from_r$Construct = data$Construct[row_id]
    
    
    estimated_from_r$varLOR =  (pi^2*estimated_from_r$var.d)/(3)
    estimated_from_r = cbind(estimated_from_r$varLOR, estimated_from_r) 
    results_estimated_from_r = rbind(results_estimated_from_r, estimated_from_r)
  } 
  
  if (estimate_type == "Mean_SD") {
    estimated_from_mean = mes(m.1 = data$Mean_PA_X[row_id],
                              m.2 = data$Mean_PA_noX[row_id],
                              sd.1 = data$SD_PA_X[row_id],
                              sd.2 = data$SD_PA_noX[row_id],
                              n.1 = data$N_PA_X[row_id],
                              n.2 = data$N_PA_noX[row_id],
                              level = 95,
                              cer = 0.2,
                              dig = 2,
                              verbose = FALSE) 
    
    estimated_from_mean$Author = data$Author[row_id]
    estimated_from_mean$Year = data$Year[row_id]
    estimated_from_mean$Construct = data$Construct[row_id]
    estimated_from_mean$varLOR =  (pi^2*estimated_from_mean$var.d)/(3)
    estimated_from_mean = cbind(estimated_from_mean$varLOR, estimated_from_mean) 
    results_estimated_from_mean = rbind(results_estimated_from_mean, estimated_from_mean) 
  }
  
  if (estimate_type == "Binary") {
    
    AB = data$N_PA_noX[row_id] + data$N_PA_X[row_id]
    CD = data$N[row_id] - AB 
    
    estimated_from_Binary = propes(p1 = data$Proportion_Percent_PA_X[row_id],
                                   p2 = data$Proportion_Percent_PA_noX[row_id],
                                   n.ab = AB,
                                   n.cd = CD,
                                   level = 95, cer = 0.2,
                                   dig = 2,
                                   verbose = FALSE)
    
    estimated_from_Binary$Author = data$Author[row_id]
    estimated_from_Binary$Year = data$Year[row_id]
    estimated_from_Binary$Construct = data$Construct[row_id]
    estimated_from_Binary$varLOR =  (pi^2*estimated_from_mean$var.d)/(3)
    estimated_from_Binary = cbind(estimated_from_Binary$varLOR, estimated_from_Binary) 
    results_estimated_Binary= rbind(results_estimated_Binary, estimated_from_Binary) 
  }
  
  row_id = row_id + 1
}




 EffectSize_conversion_lOR = c(results_estimated_from_mean$lOR, 
                              results_estimated_from_r$lOR, 
                              results_estimated_from_F$lOR,
                              results_estimated_Binary$lOR) 

 EffectSize_conversion_lOR_lCI = c(results_estimated_from_mean$l.lor,
                                  results_estimated_from_r$l.lor,
                                  results_estimated_from_F$l.lor,
                                  results_estimated_Binary$l.or)

 EffectSize_conversion_lOR_uCI = c(results_estimated_from_mean$u.lor, 
                                   results_estimated_from_r$u.lor, 
                                   results_estimated_from_F$u.lor,
                                   results_estimated_Binary$u.lor) 

 

 EffectSize_conversion_OR = c(results_estimated_from_mean$OR, 
                              results_estimated_from_r$OR, 
                              results_estimated_from_F$OR,
                              results_estimated_Binary$OR) 
 
 
 
 
 EffectSize_conversion_OR_lCI= c(results_estimated_from_mean$l.or,
                                 results_estimated_from_r$l.or, 
                                 results_estimated_from_F$l.or,
                                 results_estimated_Binary$l.or) 
           
 
 EffectSize_conversion_OR_uCI = c(results_estimated_from_mean$u.or, 
                                  results_estimated_from_r$u.or, 
                                  results_estimated_from_F$u.or,
                                  results_estimated_Binary$u.or)
 

 Author = c(results_estimated_from_mean$Author, 
            results_estimated_from_r$Author, 
            results_estimated_from_F$Author,
            results_estimated_Binary$Author)
 
 Year = c(results_estimated_from_mean$Year, 
          results_estimated_from_r$Year, 
          results_estimated_from_F$Year,
          results_estimated_Binary$Year)
 

 Construct = c(results_estimated_from_mean$Construct, 
               results_estimated_from_r$Construct, 
               results_estimated_from_F$Construct,
               results_estimated_Binary$Construct)
 

 varLOR = c(results_estimated_from_mean$varLOR, 
            results_estimated_from_r$varLOR, 
            results_estimated_from_F$varLOR,
            results_estimated_Binary$varLOR)
 
 Fisher_Z = c(results_estimated_from_mean$fisher.z, 
              results_estimated_from_r$fisher.z, 
              results_estimated_from_F$fisher.z,
              results_estimated_Binary$fisher.z)
 
 
 variance_Fisher_Z = c(results_estimated_from_mean$var.z, 
                       results_estimated_from_r$var.z, 
                       results_estimated_from_F$var.z,
                       results_estimated_Binary$var.z)
 
 raw_correlation_coefficient =  c(results_estimated_from_mean$r, 
                                  results_estimated_from_r$r, 
                                  results_estimated_from_F$r,
                                  results_estimated_Binary$r)
 
 Total_N =  c(results_estimated_from_mean$N.total, 
              results_estimated_from_r$N.total, 
              results_estimated_from_F$N.total,
              results_estimated_Binary$N.total)
 


 likelihood_data  = cbind.data.frame(Author, 
                                     Year, 
                                     Construct, 
                                     EffectSize_conversion_lOR, 
                                     EffectSize_conversion_lOR_lCI, 
                                     EffectSize_conversion_lOR_uCI,
                                     EffectSize_conversion_OR, 
                                     EffectSize_conversion_OR_lCI, 
                                     EffectSize_conversion_OR_uCI,
                                     varLOR,
                                     Fisher_Z,
                                     variance_Fisher_Z,
                                     raw_correlation_coefficient,
                                     Total_N) 
 
 #Bind the results for individual studies below: 
 
 colnames (likelihood_data) = c('Author', 
                                "Year", 
                                'Construct',
                                'lOR',
                                'l.lor', 
                                'u.lor', 
                                'OR',
                                'l.or', 
                                'u.or', 
                                "varLOR", 
                                "Fisher_Z", 
                                "variance_Fisher_Z",
                                "raw_correlation_coefficient", 
                                "Total_N")
 

 filename_likelihood_data <- file.path(paste(OUTPUT_ROOT, "likelihood_data.csv", sep=""))
 fn_filename_likelihood_data <- as.filename(filename_likelihood_data)
 make_path(fn_filename_likelihood_data)
 write.table(likelihood_data, file = filename_likelihood_data, 
             append = FALSE, 
             quote = TRUE, 
             sep = ",", 
             eol = "\r", 
             na = "NA", 
             dec = ".",
             row.names = FALSE, 
             col.names = TRUE, 
             fileEncoding = "" )
 
return(params = (likelihood_data))
}
