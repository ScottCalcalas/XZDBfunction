#Function
#Binary endpoint univariate
#LRT test for the variable that has more than two categories.
dig=2





#' Univariate Logistic Regression (Categorical Predictor)
#'
#' Fits a univariate logistic regression model for a binary outcome and a 
#' categorical predictor. Returns odds ratios (OR), 95% confidence intervals,
#' Wald p-values for 2-level predictors, and likelihood ratio test (LRT)
#' p-values for predictors with >2 categories.
#'
#' @param data_fun Data frame containing variables.
#' @param response Character. Name of the binary outcome variable (0/1).
#' @param var Character. Name of the categorical predictor.
#' @param dig Integer. Number of digits for rounding OR and CI. Default = 2.
#'
#' @details
#' • Removes incomplete cases using only the variables in \code{response} and \code{var}.  
#' • For categorical predictors with:
#'   - **2 levels** → Wald p-value for the coefficient  
#'   - **>2 levels** → Likelihood ratio test (LRT) comparing model vs. intercept-only model  
#'
#' @return A character matrix with rows = levels of the variable and 
#' columns = \code{"OR (95% CI)"} and \code{"p value"}.
#'
#' @examples
#' \dontrun{
#' logist_univar_fun(mydata, response = "Y", var = "Gender")
#' }
#'
#' @export

logist_univar_fun =  function(data_fun, response, var){
  data_fun_1 = data_fun[complete.cases(data_fun[,c(response,var)]),]
  if(response == var | (response=="OR" & var=="OR"))
  {
  var_n_cat = length(levels(data_fun_1[,var]))
  res = matrix("", nrow=var_n_cat+1, ncol=2)
  colnames(res)<-c("OR (95% CI)", "p value")
  row_nm = c(var, paste0("  ",levels(data_fun_1[,var])[1]," (Ref)", sep="") ,
             paste0("  ",levels(data_fun_1[,var])[2:var_n_cat],sep=""))
  rownames(res) =   row_nm
  
  } else {
  x = glm( as.formula(paste(response, ' ~ ', var,sep="")), data = data_fun_1, family = binomial() )
  sum_x <- summary(x)
  #var = attr(sum_x$terms,"term.labels")
  #test=data_fun_1[,var]
  #test[,var]=lapply(test, factor)
  #test=unlist(test)
  #test=apply(test ,2, factor)
  #var_n_cat = length(levels(test))
  var_n_cat = length(levels(data_fun_1[,var]))
  
  res = matrix("", nrow=var_n_cat+1, ncol=2)
  colnames(res)<-c("OR (95% CI)", "p value")
  row_nm = c(var, paste0("  ",levels(data_fun_1[,var])[1]," (Ref)", sep="") ,              paste0("  ",levels(data_fun_1[,var])[2:var_n_cat],sep=""))
  #row_nm = c(var, paste0("  ",levels(test)[1]," (Ref)", sep="") ,                         paste0("  ",levels(test)[2:var_n_cat],sep=""))
  rownames(res) =   row_nm
  
    coef_row_name = paste0(var, levels(data_fun_1[,var])[2:var_n_cat], sep="")
    coef_index_extract = rownames(sum_x$coefficients) %in% coef_row_name
    OR = round( exp(sum_x$coefficients[coef_index_extract,"Estimate"]), dig)
    int = confint.default(x)
    OR.confint.lower <- round( exp(int[coef_index_extract,"2.5 %"]), dig)
    OR.confint.upper <- round( exp(int[coef_index_extract,"97.5 %"]), dig)
    OR <- paste0(OR, " (",
                 OR.confint.lower, " - ", OR.confint.upper, ")")
    p.value.unadj = round(sum_x$coefficients[coef_index_extract,"Pr(>|z|)"],3)
    p.value.unadj[which(p.value.unadj<0.001)]="<0.001"
    res_ct = cbind(OR, p.value.unadj)
    coef_name_extract = rownames(sum_x$coefficients)[coef_index_extract]
    res_name = paste0("  ",substr(coef_name_extract,nchar(var)+1,nchar(coef_row_name)),sep="")
    res[res_name,] = res_ct
    
  if(var_n_cat==2){
  res[,"p value"] = c(p.value.unadj, rep("",var_n_cat))
  }
  
  if(var_n_cat>2){
    model0 = glm( as.formula(paste(response, ' ~ ', 1,sep="")), data = data_fun_1, family = binomial())
    LRT_p = round(anova(model0, x, test = 'LRT')[2,"Pr(>Chi)"],3)
    LRT_p = ifelse(LRT_p<0.001,"<0.001",LRT_p)
    res[,"p value"]= c(LRT_p, rep("",var_n_cat))
  }#when categories of the variable is larger than 2, than add a blank column
  
  }
  return(res)
  #return(exp(cbind(coef(x),confint(x))))
}









#' Univariate Logistic Regression (Continuous Predictor)
#'
#' Fits a univariate logistic regression model for a binary response and 
#' a continuous predictor. Returns odds ratio (per-unit increase) with 
#' 95% confidence intervals and Wald p-value.
#'
#' @param data_fun Data frame containing variables.
#' @param response Character. Name of binary outcome variable.
#' @param var Character. Name of continuous predictor.
#' @param dig Number of digits to round results.
#'
#' @return A 1-row matrix with OR, CI and p-value.
#'
#' @examples
#' \dontrun{
#' logist_univar_fun_cts(mydata, "Y", "Age")
#' }
#'
#' @export

logist_univar_fun_cts =  function(data_fun, response, var){
  data_fun_1 = data_fun[complete.cases(data_fun[,c(response,var)]),]
  x = glm( as.formula(paste(response, ' ~ ', var,sep="")), data = data_fun_1, family = binomial() )
  sum_x <- summary(x)
  #var_n_cat = length(var)
  var_n_cat = length(levels(data_fun_1[,var]))
  res = matrix("", nrow=var_n_cat+1, ncol=2)
  colnames(res)<-c("OR (95% CI)", "p value")
  row_nm = var
  rownames(res) =   row_nm
  
  OR = round(exp(sum_x$coef[2:length(x$coef),1]), dig);#exp(beta)
  int = confint.default(x)
  OR.confint.lower <- round( exp(int[2:length(x$coef),"2.5 %"]), dig)
  OR.confint.upper <- round( exp(int[2:length(x$coef),"97.5 %"]), dig)
  OR <- paste0(OR, " (",
               OR.confint.lower, " - ", OR.confint.upper, ")")
  p.value.unadj = round(sum_x$coefficients[2:length(x$coef),"Pr(>|z|)"],3)
  p.value.unadj[which(p.value.unadj<0.001)]="<0.001"
  res_ct <- cbind(OR,p.value.unadj)
  
  res[var_n_cat+1,] = res_ct
  #res[var_n_cat,] = res_ct
  
  return(res)
}

#Binary endpoint multivariate
#var_model: variables that will be included in this speicific model;








#' Multivariable Logistic Regression (Categorical Predictor)
#'
#' Extracts adjusted odds ratios from a multivariable logistic regression 
#' model. Performs Wald tests for 2-category predictors and likelihood 
#' ratio tests (LRT) for predictors with more than 2 categories.
#'
#' @param var_model Character vector of variables included in the multivariable model.
#' @param var Character. The specific categorical variable to extract results for.
#' @param response Character. Name of binary outcome variable.
#' @param data_fun Data frame containing variables.
#' @param multi_var_all Character vector of full model candidate variables.
#'   Used to determine whether \code{var} is included in \code{var_model}.
#' @param dig Number of digits to round OR and CI.
#'
#' @details
#' • Categorical levels are compared to the first level (reference).  
#' • If \code{var} is not included in \code{var_model}, a blank row is returned.  
#' • For >2 categories, LRT compares:
#'   full model vs. model without the variable.
#'
#' @return A matrix of adjusted ORs and p-values.
#'
#' @examples
#' \dontrun{
#' logist_multi_fun(
#'   var_model = c("Age", "Gender"),
#'   var       = "Gender",
#'   response  = "Y",
#'   data_fun  = mydata,
#'   multi_var_all = c("Age", "Gender", "BMI")
#' )
#' }
#'
#' @export

logist_multi_fun = function(var_model, var, response, data_fun, multi_var_all=multi_var_all){
  not_in_model = setdiff(multi_var_all, var_model)
  data_fun_1 = data_fun[complete.cases(data_fun[,c(response,var_model)]),]
  logit_m = glm(as.formula(paste(response," ~ ",paste(var_model,collapse =" + "), sep="")),
                data = data_fun_1, family = binomial() )
  sum_x = summary(logit_m)
  
  var_n_cat = length(levels(data_fun_1[,var]))
  res = matrix("", nrow=var_n_cat+1, ncol=2)
  colnames(res)<-c("OR (95% CI)", "p value")
  row_nm = c(var, paste0("  ",levels(data_fun_1[,var])[1]," (Ref)", sep="") ,
             paste0("  ",levels(data_fun_1[,var])[2:var_n_cat],sep=""))
  rownames(res) =   row_nm
  
  if (var %in% not_in_model) {
    res = res
  }
  
  if (var %in% var_model) {
    coef_row_name = paste0(var, levels(data_fun_1[,var])[2:var_n_cat], sep="")
    coef_index_extract = rownames(sum_x$coefficients) %in% coef_row_name
    OR = round( exp(sum_x$coefficients[coef_index_extract,"Estimate"]), dig)
    int = confint.default(logit_m)
    OR.confint.lower <- round( exp(int[coef_index_extract,"2.5 %"]), dig)
    OR.confint.upper <- round( exp(int[coef_index_extract,"97.5 %"]), dig)
    OR <- paste0(OR, " (",
                 OR.confint.lower, " - ", OR.confint.upper, ")")
    p.value.adj = round(sum_x$coefficients[coef_index_extract,"Pr(>|z|)"],3)
    p.value.adj[which(p.value.adj<0.001)]="<0.001"
    res_ct = cbind(OR, p.value.adj)
    coef_name_extract = rownames(sum_x$coefficients)[coef_index_extract]
    res_name = paste0("  ",substr(coef_name_extract,nchar(var)+1,nchar(coef_row_name)),sep="")
    res[res_name,] = res_ct
    if(var_n_cat==2){
      res[,"p value"] = c(p.value.adj, rep("",var_n_cat))
    }
    
    if(var_n_cat>2){
      model0 = glm( as.formula(paste(response, ' ~ ',
                                     exp_var_multi = paste0(setdiff(var_model, var), collapse = "+"),sep="")),
                    data = data_fun_1, family = binomial())
      
      LRT_p = round(anova(model0, logit_m, test = 'LRT')[2,"Pr(>Chi)"],3)
      LRT_p = ifelse(LRT_p<0.001,"<0.001",LRT_p)
      res[,"p value"]= c(LRT_p, rep("",var_n_cat))
    }
  }
  return(res)
}







#' Multivariable Logistic Regression (Continuous Predictor)
#'
#' Extracts adjusted odds ratio for a continuous variable from a 
#' multivariable logistic regression model.
#'
#' @inheritParams logist_multi_fun
#'
#' @return A single-row matrix containing adjusted OR, CI, and p-value.
#'
#' @examples
#' \dontrun{
#' logist_multi_fun_cts(
#'   var_model = c("Age", "Gender"),
#'   var       = "Age",
#'   response  = "Y",
#'   data_fun  = mydata
#' )
#' }
#'
#' @export

logist_multi_fun_cts = function(var_model, var, response, data_fun, multi_var_all=multi_var_all){
  not_in_model = setdiff(multi_var_all, var_model)
  data_fun_1 = data_fun[complete.cases(data_fun[,c(response,var_model)]),]
  logit_m = glm(as.formula(paste(response," ~ ",paste(var_model,collapse =" + "), sep="")),
                data = data_fun_1, family = binomial() )
  sum_x = summary(logit_m)
  
  var_n_cat = length(levels(data_fun_1[,var]))
  res = matrix("", nrow=var_n_cat+1, ncol=2)
  colnames(res)<-c("OR (95% CI)", "p value")
  row_nm = c(var)
  rownames(res) =   row_nm
  
  if (var %in% not_in_model) {
    res = res
  }
  
  if (var %in% var_model) {
    coef_row_name = paste0(var, levels(data_fun_1[,var])[2:var_n_cat], sep="")
    OR = round( exp(sum_x$coefficients[coef_row_name,"Estimate"]), dig)
    int = confint.default(logit_m)
    OR.confint.lower <- round( exp(int[coef_row_name,"2.5 %"]), dig)
    OR.confint.upper <- round( exp(int[coef_row_name,"97.5 %"]), dig)
    OR <- paste0(OR, " (",
                 OR.confint.lower, " - ", OR.confint.upper, ")")
    p.value.adj = round(sum_x$coefficients[coef_row_name,"Pr(>|z|)"],3)
    p.value.adj[which(p.value.adj<0.001)]="<0.001"
    res_ct = cbind(OR, p.value.adj)
    res[(var_n_cat+1),] = res_ct
  }
  return(res)
}










#Survival endpoint univariate
#' Univariate Cox Regression (Categorical Predictor)
#'
#' Fits a univariate Cox proportional hazards model for time-to-event data
#' using a categorical predictor. Reports hazard ratios (HR), 95% CI, 
#' Wald p-values for 2-category variables, and likelihood ratio test (LRT) 
#' results for variables with >2 categories.
#'
#' @param data_fun Data frame containing survival data.
#' @param res_time Character. Name of survival time variable.
#' @param res_event Character. Name of event indicator (1 = event, 0 = censored).
#' @param var Character. Categorical predictor variable.
#' @param dig Rounding digits.
#'
#' @return A matrix of HRs, CIs, and p-values.
#'
#' @examples
#' \dontrun{
#' cox_univar_fun(mydata, "OS_time", "OS_event", "Stage")
#' }
#'
#' @export

cox_univar_fun =  function(data_fun, res_time, res_event, var){
  
  data_fun_1 = data_fun[complete.cases(data_fun[,c(res_time,res_event,var)]),]
  response = paste0("Surv(", res_time, ", ", res_event,")", sep= "")
  x = coxph( as.formula(paste(response, ' ~ ', var,sep="")), data = data_fun_1 )
  sum_x <- summary(x)
  var_n_cat = length(levels(data_fun_1[,var]))
  
  
  res = matrix("", nrow=var_n_cat+1, ncol=2)
  colnames(res)<-c("HR (95% CI)", "p value")
  row_nm = c(var, paste0("  ",levels(data_fun_1[,var])[1]," (Ref)", sep="") ,
             paste0("  ",levels(data_fun_1[,var])[2:var_n_cat],sep=""))
  rownames(res) =   row_nm
  
  HR = round(exp(sum_x$coef[1:length(x$coef),1]), dig);#exp(beta)
  int = confint.default(x)
  HR.confint.lower <- round( exp(int[1:length(x$coef),"2.5 %"]), dig)
  HR.confint.upper <- round( exp(int[1:length(x$coef),"97.5 %"]), dig)
  HR <- paste0(HR, " (",
               HR.confint.lower, " - ", HR.confint.upper, ")")
  p.value.unadj = round(sum_x$coefficients[1:length(x$coef),"Pr(>|z|)"],3)
  p.value.unadj[which(p.value.unadj<0.001)]="<0.001"
  res_ct <- cbind(HR,p.value.unadj)
  res[3:(var_n_cat+1),] = res_ct
  if(var_n_cat==2){
    res[,"p value"] = c(p.value.unadj, rep("",var_n_cat))
  }
  if(var_n_cat>2){
    model0 = coxph( as.formula(paste(response, ' ~ ', 1,sep="")), data = data_fun_1)
    LRT_p = round(anova(model0, x, test = 'LRT')[2,"Pr(>|Chi|)"],3)
    LRT_p = ifelse(LRT_p<0.001,"<0.001",LRT_p)
    res[,"p value"]= c(LRT_p, rep("",var_n_cat))
  }#when categories of the variable is larger than 2, than add a blank column
  return(res)
}







#' Univariate Cox Regression (Continuous Predictor)
#'
#' Fits a univariate Cox model for a continuous predictor and returns the HR,
#' 95% CI, and Wald p-value.
#'
#' @inheritParams cox_univar_fun
#'
#' @return A single-row matrix of HR and p-value.
#'
#' @examples
#' \dontrun{
#' cox_univar_fun_cts(mydata, "OS_time", "OS_event", "Age")
#' }
#'
#' @export

cox_univar_fun_cts =  function(data_fun, res_time, res_event, var){
  
  data_fun_1 = data_fun[complete.cases(data_fun[,c(res_time,res_event,var)]),]
  response = paste0("Surv(", res_time, ", ", res_event,")", sep= "")
  x = coxph( as.formula(paste(response, ' ~ ', var,sep="")), data = data_fun_1 )
  sum_x <- summary(x)
  var_n_cat = length(levels(data_fun_1[,var]))
  #var_n_cat=length(var)
  
  res = matrix("", nrow=var_n_cat+1, ncol=2)
  #res = matrix("", nrow=var_n_cat, ncol=2)
  colnames(res)<-c("HR (95% CI)", "p value")
  row_nm = var
  rownames(res) =   row_nm
  
  HR = round(exp(sum_x$coef[1:length(x$coef),1]), dig);#exp(beta)
  int = confint.default(x)
  HR.confint.lower <- round( exp(int[1:length(x$coef),"2.5 %"]), dig)
  HR.confint.upper <- round( exp(int[1:length(x$coef),"97.5 %"]), dig)
  HR <- paste0(HR, " (",
               HR.confint.lower, " - ", HR.confint.upper, ")")
  p.value.unadj = round(sum_x$coefficients[1:length(x$coef),"Pr(>|z|)"],3)
  p.value.unadj[which(p.value.unadj<0.001)]="<0.001"
  res_ct <- cbind(HR,p.value.unadj)
  res[(var_n_cat+1),] = res_ct
  #res[(var_n_cat),] = res_ct
  
  return(res)
}







#' Multivariable Cox Regression (Categorical Predictor)
#'
#' Extracts adjusted hazard ratios (HR) for a categorical variable from a 
#' multivariable Cox proportional hazards model. Performs LRT for variables 
#' with >2 categories.
#'
#' @inheritParams cox_univar_fun
#' @inheritParams logist_multi_fun
#'
#' @return A matrix of adjusted HRs and p-values.
#'
#' @examples
#' \dontrun{
#' cox_multivar_fun(mydata, "OS_time", "OS_event",
#'                  var_model = c("Stage","Age"),
#'                  var = "Stage")
#' }
#'
#' @export

#Survival endpoint multivariate
cox_multivar_fun =  function(data_fun, res_time, res_event, var_model, var, multi_var_all=multi_var_all){
  not_in_model = setdiff(multi_var_all, var_model)
  data_fun_1 = data_fun[complete.cases(data_fun[,c(res_time,res_event,var_model)]),]
  response = paste0("Surv(", res_time, ", ", res_event,")", sep= "")
  x = coxph( as.formula(paste(response, ' ~ ', paste(var_model,collapse =" + "), sep="")), data = data_fun_1 )
  sum_x = summary(x)
  var_n_cat = length(levels(data_fun_1[,var]))
  
  res = matrix("", nrow=var_n_cat+1, ncol=2)
  colnames(res)<-c("HR (95% CI)", "p value")
  row_nm = c(var, paste0("  ",levels(data_fun_1[,var])[1]," (Ref)", sep="") ,
             paste0("  ",levels(data_fun_1[,var])[2:var_n_cat],sep=""))
  rownames(res) =   row_nm
  
  if (var %in% not_in_model) {
    res = res
  }
  
  if (var %in% var_model) {
    coef_row_name = paste0(var, levels(data_fun_1[,var])[2:var_n_cat], sep="")
    HR = round(exp(sum_x$coef[coef_row_name,"coef"]), dig);#exp(beta)
    int = confint.default(x)
    HR.confint.lower <- round( exp(int[coef_row_name,"2.5 %"]), dig)
    HR.confint.upper <- round( exp(int[coef_row_name,"97.5 %"]), dig)
    HR <- paste0(HR, " (",
                 HR.confint.lower, " - ", HR.confint.upper, ")")
    p.value.adj = round(sum_x$coef[coef_row_name,"Pr(>|z|)"],3)
    p.value.adj[which(p.value.adj<0.001)]="<0.001"
    res_ct <- cbind(HR,p.value.adj)
    res[3:(var_n_cat+1),] = res_ct
    if(var_n_cat==2){
      res[,"p value"] = c(p.value.adj, rep("",var_n_cat))
    }
    
    if(var_n_cat>2){
      model0 = coxph( as.formula(paste(response, ' ~ ',
                                       exp_var_multi = paste0(setdiff(var_model, var), collapse = "+"),sep="")),
                      data = data_fun_1)
      LRT_p = round(anova(model0, x, test = 'LRT')[2,"Pr(>|Chi|)"],3)
      LRT_p = ifelse(LRT_p<0.001,"<0.001",LRT_p)
      res[,"p value"]= c(LRT_p, rep("",var_n_cat))
    }
  }
  return(res)
}









#' Multivariable Cox Regression (Continuous Predictor)
#'
#' Extracts adjusted hazard ratio for a continuous variable from a 
#' multivariable Cox model.
#'
#' @inheritParams cox_multivar_fun
#'
#' @return A one-row matrix of adjusted HR and p-value.
#'
#' @examples
#' \dontrun{
#' cox_multivar_fun_cts(mydata, "OS_time", "OS_event",
#'                     var_model = c("Age","Stage"),
#'                     var = "Age")
#' }
#'
#' @export


cox_multivar_fun_cts =  function(data_fun, res_time, res_event, var_model, var, multi_var_all=multi_var_all){
  not_in_model = setdiff(multi_var_all, var_model)
  data_fun_1 = data_fun[complete.cases(data_fun[,c(res_time,res_event,var_model)]),]
  response = paste0("Surv(", res_time, ", ", res_event,")", sep= "")
  x = coxph( as.formula(paste(response, ' ~ ', paste(var_model,collapse =" + "), sep="")), data = data_fun_1 )
  sum_x = summary(x)
  var_n_cat = length(var)
  
  res = matrix("", nrow=var_n_cat+1, ncol=2)
  colnames(res)<-c("HR (95% CI)", "p value")
  row_nm = c(var)
  rownames(res) =   row_nm
  
  if (var %in% not_in_model) {
    res = res
  }
  
  if (var %in% var_model) {
    coef_row_name = paste0(var, levels(data_fun_1[,var])[2:var_n_cat], sep="")
    HR = round(exp(sum_x$coef[coef_row_name,"coef"]), dig);#exp(beta)
    int = confint.default(x)
    HR.confint.lower <- round( exp(int[coef_row_name,"2.5 %"]), dig)
    HR.confint.upper <- round( exp(int[coef_row_name,"97.5 %"]), dig)
    HR <- paste0(HR, " (",
                 HR.confint.lower, " - ", HR.confint.upper, ")")
    p.value.adj = round(sum_x$coef[coef_row_name,"Pr(>|z|)"],3)
    p.value.adj[which(p.value.adj<0.001)]="<0.001"
    res_ct <- cbind(HR,p.value.adj)
    res[(var_n_cat+1),] = res_ct
  }
  return(res)
}





#' Find Row Locations of Variable Levels
#'
#' Computes which rows of a summary table correspond to the levels of 
#' categorical variables (used for indentation in publication tables).
#'
#' @param var Character vector of variable names.
#' @param data Data frame containing these variables as factors.
#'
#' @return Integer vector of row indices corresponding to non-header rows.
#'
#' @examples
#' \dontrun{
#' ind_loc_fun(c("Stage","Gender"), mydata)
#' }
#'
#' @export

#indent location function for table
ind_loc_fun = function(var,data){
  n_var = length(var)
  n_var_level = unlist(lapply(var, function(x){length(levels(data[,x]))}))
  
  var_row_loc_end = cumsum((n_var_level+1))
  var_row_loc_1 = cumsum((n_var_level+1))+1
  var_row_loc = c(1,var_row_loc_1[-n_var])
  
  final_loc = unlist(lapply(c(1:n_var), function(i){loc = c((var_row_loc+1)[i] :var_row_loc_end [i])}))
  #cts_loc = which(n_var_level==0)
  #delet_loc = (cts_loc-1)+sum(n_var_level[1:(cts_loc-1)])
  return(final_loc)
}




#' Identify Significant Variables
#'
#' Scans a results table and returns variable names with p-values below 
#' a significance threshold.
#'
#' @param res_tab Results table with a column named \code{"p value"}.
#' @param sig_lev Numeric significance threshold. Default = 0.1.
#'
#' @return Character vector of significant variable names.
#'
#' @examples
#' \dontrun{
#' find_sig_var(result_table, sig_lev = 0.05)
#' }
#'
#' @export

#find significant variables
find_sig_var = function(res_tab,sig_lev = 0.1){
  sig_var = names(which(as.numeric(res_tab[,"p value"])< sig_lev | res_tab[,"p value"]=="<0.001"))
  return(sig_var)
}


#' Format Life-Expectancy or Restricted Mean Survival Output
#'
#' Converts summary output from survival models (e.g., rmst2, survfit) into 
#' publication-ready strings containing median and 95% CI.
#'
#' @param fit A model object with elements \code{n} and \code{table}.
#' @param resp Character. Label to append (e.g. "Months").
#' @param levs Character vector of group names.
#'
#' @return A character vector with formatted survival statistics for each group.
#'
#' @examples
#' \dontrun{
#' led_fun_2(fit, "months", c("Low","High"))
#' }
#'
#' @export

led_fun_2 = function(fit,resp,levs){
    
    med = round(unname(summary(fit)$table[,'median']),1)
    LCL = round(unname(summary(fit)$table[,'0.95LCL']),1)
    UCL = round(unname(summary(fit)$table[,'0.95UCL']),1)
    med_1 = ifelse(is.na(med),"NR",med)
    
    led=c()
    for (i in 1:length(levs))
    {   na_vec = c(med[i],LCL[i],UCL[i])
    if(sum(is.na(na_vec))<3){
        led[i]=paste(levs[i]," (n=", fit$n[i],")",resp,"(95% CI) ", med_1[i]," (",LCL[i],", ",UCL[i],") ",sep="")}
    else {
            led[i]=paste(levs[i]," (n=", fit$n[i],")",resp,"(95% CI) ", med_1[i],sep="")}
    }
    return(led)
}
