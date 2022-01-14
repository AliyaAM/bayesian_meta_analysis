


VarData = read.csv('/Users/aliya/my_docs/proj/bayes_meta/VariableTypePairing.csv')
print(VarData)


  
VarPairType = function(VarData, Construct){
  print("gpt into the VarPairType") 
  
  
  index = VarData$Construct == Construct
  

  ContiniousPairs = filter(VarData, Construct == VarData[index,]$Construct & ContBoth == "2")
  number_ofStudies_ContBoth =  nrow(ContiniousPairs)
  
  ContPA_CatPred_Pairs = filter(VarData,Construct == VarData[index,]$Construct & ContPA_CatPred == "2")
  number_ofStudies_ContPA_CatPred =  nrow(ContPA_CatPred_Pairs)
  
  ContPA_BinaryPred_Pairs = filter(VarData,Construct == VarData[index,]$Construct & ContPA_BinaryPred == "2")
  number_ofStudies_ContPA_BinaryPred =  nrow(ContPA_BinaryPred_Pairs)

  
  ContPA_BinaryPred_Pairs = filter(VarData,Construct == VarData[index,]$Construct & ContPA_BinaryPred == "2")
  number_ofStudies_ContPA_BinaryPred =  nrow(ContPA_BinaryPred_Pairs)
  
  CatPA_ContPred_Pairs = filter(VarData, Construct == VarData[index,]$Construct & CatPA_ContPred == "2")
  number_ofStudies_CatPA_ContPred =  nrow(CatPA_ContPred_Pairs)
  
  
  CatBoth_Pairs = filter(VarData, Construct == VarData[index,]$Construct & CatBoth == "2")
  number_ofStudies_CatBoth_Pairs =  nrow(CatBoth_Pairs)
  
  CatPA_BinaryPred_Pairs = filter(VarData, Construct == VarData[index,]$Construct & CatPA_BinaryPred == "2")
  number_ofStudies_CatPA_BinaryPred =  nrow(CatPA_BinaryPred_Pairs)
  
  BinaryPA_ContPred_Pairs = filter(VarData,Construct == VarData[index,]$Construct & BinaryPA_ContPred == "2")
  number_ofStudies_BinaryPA_ContPred =  nrow(BinaryPA_ContPred_Pairs)
  
  BinaryPA_CategoricalPred_Pairs = filter(VarData,Construct == VarData[index,]$Construct & BinaryPA_CategoricalPred == "2")
  number_ofStudies_BinaryPA_CategoricalPred =  nrow(BinaryPA_CategoricalPred_Pairs)

  BinaryBoth_Pairs = filter(VarData, Construct == VarData[index,]$Construct & BinaryBoth == "2")
  number_ofStudies_BinaryBoth =  nrow(BinaryBoth_Pairs)

  Estimate = filter(VarData, Construct == VarData[index,]$Construct)
  
  Estimate_type = Estimate$estimate_type
  
   Type_ofComparison = data.frame(number_ofStudies_ContBoth,
                                  number_ofStudies_ContPA_CatPred, 
                                  number_ofStudies_ContPA_BinaryPred, 
                                  number_ofStudies_CatPA_ContPred,
                                  number_ofStudies_CatBoth_Pairs,
                                  number_ofStudies_CatPA_BinaryPred, 
                                  number_ofStudies_BinaryPA_ContPred, 
                                  number_ofStudies_BinaryPA_CategoricalPred, 
                                  number_ofStudies_BinaryBoth)
   
    colnames(Type_ofComparison) = c( "ContiniousNumStudies",
                                    "ContPA_CatPredNumStudies",
                                   "ContPA_BinaryPredNumStudies", 
                                    "CatPA_ContPredNumStudies",
                                    "CatBoth_PairsNumStudies",
                                    "Studies_CatPA_BinaryPredNumStudies", 
                                    "BinaryPA_ContPredNumStudies", 
                                    "BinaryPA_CategoricalPredNumStudies", 
                                    "Studies_BinaryBothNumStudies")
    
   Estimate = filter(VarData, Construct == VarData[index,]$Construct)
    
    Estimate_type = Estimate$estimate_type
#    Construct = Estimate$Construct
    
    
#    Estimate_Construct = VarData.frame(Estimate_type, Construct)
#    colnames(Estimate_Construct) = c("estimate_type", "Construct") 
    
   
#    Type_ofComparison_TABLE =  cbind(Type_ofComparison, Estimate_Construct)
    
  
  
return(Type_ofComparison)
}





#VarType_Pairs_PAType = function(VarData, PA_Var_Name){
#  index = VarData$PA_Var_Name == PA_Var_Name
  

#  PA_Var_Name_Construct = filter(VarData, PA_Var_Name == VarData[index,]$PA_Var_Name)
#  number_ofStudies = nrow(PA_Var_Name_Construct)
  
#  return(number_ofStudies)
#}

#EE = VarType_Pairs_PAType(VarData = VarData, PA_Var_Name = "EnergyExpend_total")
#Steps = VarType_Pairs_PAType(VarData = VarData, PA_Var_Name = "Steps/d_total")
#Complience = VarType_Pairs_PAType(VarData = VarData, PA_Var_Name = "Exercise_complient_Binary")
#Duration_Minutes = VarType_Pairs_PAType(VarData = VarData, PA_Var_Name = "Duration_dayMins")
#Duration_Hours = VarType_Pairs_PAType(VarData = VarData, PA_Var_Name = "Duration_dayHours")
#Accelerometer = VarType_Pairs_PAType(VarData = VarData, PA_Var_Name = "AccelerometerUnits")



