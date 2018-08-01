# Implement (Pending):

# Print best fit and present options accordingly -- Not possible with fitdistrplus (Will need explicit function)
# Time series : Auto read time/date variable
# Memory management -- parallel package explored but can only be incorporated for cases where data subsets are unrelated
# Transformation suggestions -- Explicit function needed

## Analysis summary report generation -- Major upgrade

# Transformations(log, dummification) -- Done
# Normalisation and standardisation -- Done
# Minimum no.of datapoints required and suggested confidence -- Done
# Outlier imputation -- Done
# Logs for error handling -- Done
# Overwrite file or save -- Done
# Resolve error print through trycatch -- Done
# Cramer's V report -- Done
# Interclass Correlation Coefficient report -- Done
# Duplicates check -- Done
# Dimensionality reduction -- Done
# Class Imbalance correction -- Done (Binary only)
# Regsubsets -- Done
# Partial Least Squares -- Done
# VIF check -- Done


#' Pre-process dataset
#'
#' GUI assisted data transformation and manipulation, outlier detection and missing value treatment
#' @param dataset The dataset to be analysed. The dataset should be structured with column names preferably present
#' @return The pre-processed dataset
#' @export

Pre_Proc<-function(dataset){
  require(data.table)
  require(svDialogs)
  require(DMwR)
  require(dummies)
  require(ROSE)

  dt<-as.data.table(dataset)
  dlgMessage(message = c("Please choose your working directory",
                         "All files/reports generated will be saved in this destination by default"))
  setwd(choose.dir())
  col_no<-ncol(dt)
  row_no<-nrow(dt)
  cls<-character()
  for(i in 1:ncol(dt)){
    int_cls<-class(dt[[i]])
    if(length(int_cls) > 1){
      cls[i]<-paste(int_cls,collapse = " ")
    }
    else {
      cls[i]<-int_cls
    }
  }
  cls_unq<-unique(cls)
  cls_info<-vector()
  for(i in 1:length(cls_unq)){
    cls_no<-length(which(cls == cls_unq[i]))
    cls_info[i]<-paste("Number of",cls_unq[i],"variables:",cls_no,sep = " ")
  }

  dlgMessage(message = c("The dataset has been successfully read",paste("Number of variables",col_no,sep = ": "),
                         paste("Number of observations",row_no,sep = ": ")),type = "ok")
  dlgMessage(message = c("Variable Summary:",cls_info),type = "ok")
  col_list<-strsplit(x = colnames(dt),split = " ")
  col_word<-unlist(lapply(X = col_list,FUN = length))
  col_re<-which(col_word > 1)
  if(length(col_re) >= 1){
    for(i in col_re){
      colnames(dt)[i]<-paste(col_list[[i]],collapse = "_")
    }
  }
  col_names<-colnames(dt)
  struc<-dlgMessage(message = "Do you want to view the data structure?",type = "yesno")$res
  if(struc == "yes"){
    print(str(dt))
  }
  summ<-dlgMessage(message = "Do you want to view the data summary?",type = "yesno")$res
  if(summ == "yes"){
    print(summary(dt))
  }

  ################################-----Variable drop-----################################

  n_col<-ncol(dt)
  n_obs<-nrow(dt)
  if(n_obs < (8*n_col + 50)){
    dlgMessage(message = c("There are very few observations in the data for the number of variables present",
                           "You should consider dropping some variables, employ dimensionality reduction or get more data"),
               type = "ok")
  }

  drop<-dlgMessage(message = "Do you want to drop any variable(s)?",type = "yesno")$res
  if(drop == "yes"){
    drop_var<-dlgList(choices = col_names,multiple = T,title = "Choose the variables to drop")$res
    dt<-dt[,-drop_var,with = F]
  }
  drop_col<-vector()
  unq_val<-vector()
  for(i in 1:ncol(dt)){
    unq<-unique(dt[[i]])
    l_unq<-length(unq)
    if(l_unq == 1){
      drop_col<-c(drop_col,colnames(dt)[i])
      unq_val<-c(unq_val,unq)
    }
  }
  if(length(drop_col) > 0){
    redrop<-dlgMessage(message = c("Variables have been detected in the data with a single constant value",
                                   "Do you want to remove them?"),type = "yesno")$res
    drop_choice<-paste(drop_col,": Unique value - ",unq_val,sep = "")
    if(redrop == "no"){
      dlgMessage(message = "The constant variables have been retained in the data",type = "ok")
    }
    else if(redrop == "yes"){
      redrop_var<-dlgList(choices = c(drop_choice,"Do not drop any variable"),multiple = T,
                          title = "Choose variable to drop")$res
      if(length(redrop_var) > 0 && "Do not drop any variable" %in% redrop_var == F){
        split<-strsplit(x = redrop_var,split = ": Unique value - ")
        colnm<-vector()
        for(i in 1:length(split)){
          colnm[i]<-split[[i]][1]
        }
        dt<-dt[,-colnm,with = F]
        dlgMessage(message = "The chosen variables were dropped successfully",type = "ok")
      }
    }
  }

  ################################-----Duplicates check-----################################

  dup<-which(duplicated(x = dt) == T)
  if(length(dup) > 0){
    dup_check<-dlgMessage(message = c("The input data has duplicated entries","Would you like to inpect them further?"),
                          type = "yesno")$res
    if(dup_check == "yes"){
      write.csv(x = dt[dup,],file = "Pre_Proc-Duplicates.csv",row.names = F)
      dlgMessage(message = "The duplicated data has been saved in your working directory",type = "ok")
      dup_rem<-dlgMessage(message = "Do you want to remove the duplicates in your data?",type = "yesno")$res
      if(dup_rem == "yes"){
        dup_rem2<-dlgMessage(message = c(paste("A total of",length(dup),"observations/rows will be removed from your data",sep = " "),
                                         "Are you sure you want to remove them?"),type = "yesno")$res
        if(dup_rem2 == "yes"){
          dt<-dt[-dup,]
          dlgMessage(message = "The duplicated entries in the data have been successfully removed",type = "ok")
        }
      }
    }
    else {
      dlgMessage(message = "The duplicates in the data have been retained",type = "ok")
    }
  }
  else {
    dlgMessage(message = "The input data has no duplicated entries",type = "ok")
  }

  ################################-----Variable conversions-----################################

  cls<-character()
  for(i in 1:ncol(dt)){
    int_cls<-class(dt[[i]])
    if(length(int_cls) > 1){
      cls[i]<-paste(int_cls,collapse = " ")
    }
    else {
      cls[i]<-int_cls
    }
  }
  if("character" %in% cls){
    char<-which(cls == "character")
    char_no<-length(char)
    char_con<-dlgMessage(message = c(paste("There are",char_no,"character variable(s) in the data",sep = " "),
                                     "Do you want to convert them?"),type = "yesno")$res
    suppressWarnings(while(char_con == "yes"){
      conv_to<-dlgList(choices = c("Factor","Integer","Numeric","Logical","Date"),preselect = "Factor",multiple = F,
                       title = "Convert variable to?")$res
      conv_col<-dlgList(choices = colnames(dt)[char],multiple = T,
                        title = paste("Select",conv_to,"variable(s)",sep = " "))$res
      if(conv_to == "Factor"){
        for(i in 1:length(conv_col)){
          dt[,conv_col[i] := as.factor(dt[[conv_col[i]]])]
        }
      }
      else if(conv_to == "Integer"){
        for(i in 1:length(conv_col)){
          dt[,conv_col[i] := as.integer(dt[[conv_col[i]]])]
        }
      }
      else if(conv_to == "Numeric"){
        for(i in 1:length(conv_col)){
          dt[,conv_col[i] := as.numeric(dt[[conv_col[i]]])]
        }
      }
      else if(conv_to == "Logical"){
        for(i in 1:length(conv_col)){
          dt[,conv_col[i] := as.logical(dt[[conv_col[i]]])]
        }
      }
      else if(conv_to == "Date"){
        dt_form<-dlgList(choices = c("Day-Month(in numbers)-Year(full)","Day-Month(in numbers)-Year(last 2 digits)",
                                     "Day-Month(in alphabets)-Year(full)","Day-Month(in alphabets)-Year(last 2 digits)",
                                     "Month(in numbers)-Day-Year(full)","Month(in numbers)-Day-Year(last 2 digits)",
                                     "Month(in alphabets)-Day-Year(full)","Month(in alphabets)-Day-Year(last 2 digits)",
                                     "Year(full)-Month(in numbers)-Day","Year(full)-Month(in alphabets)-Day",
                                     "Year(last 2 digits)-Month(in numbers)-Day","Year(last 2 digits)-Month(in alphabets)-Day",
                                     "Year(full)-Day-Month(in numbers)","Year(full)-Day-Month(in alphabets)",
                                     "Year(last 2 digits)-Day-Month(in numbers)","Year(last 2 digits)-Day-Months(in alphabets)"),
                         preselect = "Day-Month(in numbers)-Year(full)",multiple = F,
                         title = "Choose date format in the data")$res
        dt_sep<-dlgInput(message = c("Enter the separator used for date in the data","For Ex: '-','/' (without '')"),default = "-")$res

        if(length(dt_sep) <= 0){
          dt_sep<-""
        }

        if(dt_form == "Day-Month(in numbers)-Year(full)"){
          dform<-paste("%d","%m","%Y",sep = dt_sep)
        }
        else if(dt_form == "Day-Month(in numbers)-Year(last 2 digits)"){
          dform<-paste("%d","%m","%y",sep = dt_sep)
        }
        else if(dt_form == "Day-Month(in alphabets)-Year(full)"){
          dform<-paste("%d","%b","%Y",sep = dt_sep)
        }
        else if(dt_form == "Day-Month(in alphabets)-Year(last 2 digits)"){
          dform<-paste("%d","%b","%y",sep = dt_sep)
        }
        else if(dt_form == "Month(in numbers)-Day-Year(full)"){
          dform<-paste("%m","%d","%Y",sep = dt_sep)
        }
        else if(dt_form == "Month(in numbers)-Day-Year(last 2 digits)"){
          dform<-paste("%m","%d","%y",sep = dt_sep)
        }
        else if(dt_form == "Month(in alphabets)-Day-Year(full)"){
          dform<-paste("%b","%d","%Y",sep = dt_sep)
        }
        else if(dt_form == "Month(in alphabets)-Day-Year(last 2 digits)"){
          dform<-paste("%b","%d","%y",sep = dt_sep)
        }
        else if(dt_form == "Year(full)-Month(in numbers)-Day"){
          dform<-paste("%Y","%m","%d",sep = dt_sep)
        }
        else if(dt_form == "Year(full)-Month(in alphabets)-Day"){
          dform<-paste("%Y","%b","%d",sep = dt_sep)
        }
        else if(dt_form == "Year(last 2 digits)-Month(in numbers)-Day"){
          dform<-paste("%y","%m","%d",sep = dt_sep)
        }
        else if(dt_form == "Year(last 2 digits)-Month(in alphabets)-Day"){
          dform<-paste("%y","%b","%d",sep = dt_sep)
        }
        else if(dt_form == "Year(full)-Day-Month(in numbers)"){
          dform<-paste("%Y","%d","%m",sep = dt_sep)
        }
        else if(dt_form == "Year(full)-Day-Month(in alphabets)"){
          dform<-paste("%Y","%d","%b",sep = dt_sep)
        }
        else if(dt_form == "Year(last 2 digits)-Day-Month(in numbers)"){
          dform<-paste("%y","%d","%m",sep = dt_sep)
        }
        else if(dt_form == "Year(last 2 digits)-Day-Months(in alphabets)"){
          dform<-paste("%y","%d","%b",sep = dt_sep)
        }
        for(i in 1:length(conv_col)){
          dt[,conv_col[i] := as.Date(x = dt[[conv_col[i]]],format = dform)]
        }
      }

      cls<-character()
      for(i in 1:ncol(dt)){
        int_cls<-class(dt[[i]])
        if(length(int_cls) > 1){
          cls[i]<-paste(int_cls,collapse = " ")
        }
        else {
          cls[i]<-int_cls
        }
      }
      if("character" %in% cls){
        char<-which(cls == "character")
        char_no<-length(char)
        char_con<-dlgMessage(message = c(paste("There are",char_no,"character variable(s) still left in the data",sep = " "),
                                         "Do you want to convert them?"),type = "yesno")$res
      }
      else {
        char_con<-"no"
      }
    })
  }

  if("integer" %in% cls){
    conv_num<-colnames(dt)[which(cls == "integer")]
    for(i in 1:length(conv_num)){
      dt[,conv_num[i] := as.numeric(dt[[conv_num[i]]])]
    }
  }

  ################################-----Outlier detection and replacement-----################################

  if("numeric" %in% cls | "integer" %in% cls){
    perf_out<-dlgMessage(message = "Do you want to perform Outlier Analysis?",type = "yesno")$res
    if(perf_out == "yes"){
      cls<-character()
      for(i in 1:ncol(dt)){
        int_cls<-class(dt[[i]])
        if(length(int_cls) > 1){
          cls[i]<-paste(int_cls,collapse = " ")
        }
        else {
          cls[i]<-int_cls
        }
      }
      num_cls<-which(cls %in% c("numeric","integer"))
      num_dt<-dt[,num_cls,with = F]
      colnames(num_dt)<-colnames(dt)[num_cls]
      out<-dlgList(choices = c("Detect outliers using standard deviation (6 sigma)","Detect outliers using Interquartile range (IQR)"),
                   preselect = "Detect outliers using standard deviation (6 sigma)",multiple = F,title = "Outlier detection")$res
      if(out == "Detect outliers using standard deviation (6 sigma)"){
        out_list<-vector(length = ncol(num_dt),mode = "list")
        names(out_list)<-colnames(num_dt)
        for(i in 1:ncol(num_dt)){
          sdev<-sd(x = num_dt[[i]],na.rm = T)
          mean_val<-mean(x = num_dt[[i]],na.rm = T)
          outlier<-which(num_dt[[i]] > (mean_val + (3*sdev)) | num_dt[[i]] < (mean_val - (3*sdev)))
          out_list[[i]]<-outlier
        }
        out_dt<-dt[unique(unlist(out_list)),]
        if(nrow(out_dt) >= 1){
          write.csv(x = out_dt,file = "Pre_Proc-Outliers.csv",row.names = F)
          dlgMessage(message = c("The data containing entries with the outliers has been saved in your working directory",
                                 "Please review the file before proceeding"))
        }
        out_l<-unlist(lapply(X = out_list,FUN = length))
        names(out_l)<-names(out_list)
        out_lmax<-max(out_l,na.rm = T)
        if(out_lmax <= 0){
          dlgMessage(message = "No outliers have been detected in the data",type = "ok")
        }
        else if(out_lmax > 0){
          rem_out<-dlgMessage(message = c("Do you want to remove/replace the outliers in your data?",
                                          "To retain the outliers, choose 'No'"),type = "yesno")$res
          if(rem_out == "no"){
            dlgMessage(message = "Outliers have been retained in the data",type = "ok")
          }
          if(rem_out == "yes"){
            out_act<-dlgList(choices = c("Remove outliers","Replace outliers"),multiple = F,
                             preselect = "Remove outliers",title = "Outlier action")$res
            out_ex<-out_l[which(out_l > 0)]
            names(out_ex)<-names(out_l)[which(out_l > 0)]
            out_var<-names(out_ex)
            out_L<-paste(out_ex,"outliers",sep = " ")
            out_choice<-paste(out_var,out_L,sep = " : ")
            if(out_act == "Remove outliers"){
              rem_out_var<-dlgList(choices = c(out_choice,"Do not remove any outliers                            ")
                                   ,multiple = T,title = "Choose the variables to remove outliers from")$res
              if("Do not remove any outliers                            " %in% rem_out_var){
                dlgMessage(message = "Outliers have been retained in the data")
              }
              else{
                rem_dt<-which(out_choice %in% rem_out_var)
                rem_col<-names(out_ex)[rem_dt]
                out_ls<-which(names(out_list) %in% rem_col)
                rem_ID<-vector()
                for(i in 1:length(out_ls)){
                  rem_id<-out_list[[out_ls[i]]]
                  rem_ID<-c(rem_ID,rem_id)
                }
                rem_ID<-unique(rem_ID)
                dt<-dt[-rem_ID,]
                dlgMessage(message = "The outliers have been removed successfully",type = "ok")
              }
            }
            if(out_act == "Replace outliers"){
              for(i in 1:length(out_var)){
                dt[dt[[out_var[i]]] < (mean(x = dt[[out_var[i]]],na.rm = T) - 3*sd(x = dt[[out_var[i]]],na.rm = T)),
                   out_var[i] := as.numeric(mean(x = dt[[out_var[i]]],na.rm = T) - 3*sd(x = dt[[out_var[i]]],na.rm = T))]
                dt[dt[[out_var[i]]] > (mean(x = dt[[out_var[i]]],na.rm = T) + 3*sd(x = dt[[out_var[i]]],na.rm = T)),
                   out_var[i] := as.numeric(mean(x = dt[[out_var[i]]],na.rm = T) + 3*sd(x = dt[[out_var[i]]],na.rm = T))]
              }
              dlgMessage(message = "Outliers have been successfully replaced")
            }
          }
        }
      }
      else if(out == "Detect outliers using Interquartile range (IQR)"){
        out_list<-vector(length = ncol(num_dt),mode = "list")
        names(out_list)<-colnames(num_dt)
        for(i in 1:ncol(num_dt)){
          quant<-quantile(num_dt[[i]],na.rm = T)
          iqr<-IQR(num_dt[[i]],na.rm = T)
          q1<-quant[2]
          q3<-quant[4]
          outlier<-which(num_dt[[i]] < (q1 - (1.5*iqr)) | num_dt[[i]] > (q3 + (1.5*iqr)))
          out_list[[i]]<-outlier
        }
        out_dt<-dt[unique(unlist(out_list)),]
        if(nrow(out_dt) >= 1){
          write.csv(x = out_dt,file = "Pre_Proc-Outliers.csv",row.names = F)
          dlgMessage(message = c("The data containing entries with the outliers has been saved in your working directory",
                                 "Please review the file before proceeding"))
        }
        out_l<-unlist(lapply(X = out_list,FUN = length))
        names(out_l)<-names(out_list)
        out_lmax<-max(out_l,na.rm = T)
        if(out_lmax <= 0){
          dlgMessage(message = "No outliers have been detected in the data",type = "ok")
        }
        else if(out_lmax > 0){
          rem_out<-dlgMessage(message = c("Do you want to remove/replace the outliers in your data?",
                                          "To retain the outliers, choose 'No'"),
                              type = "yesno")$res
          if(rem_out == "no"){
            dlgMessage(message = "Outliers have been retained in the data",type = "ok")
          }
          if(rem_out == "yes"){
            out_act<-dlgList(choices = c("Remove outliers","Replace outliers"),multiple = F,
                             preselect = "Remove outliers",title = "Outlier action")$res
            out_ex<-out_l[which(out_l > 0)]
            names(out_ex)<-names(out_l)[which(out_l > 0)]
            out_var<-names(out_ex)
            out_L<-paste(out_ex,"outliers",sep = " ")
            out_choice<-paste(out_var,out_L,sep = " : ")
            if(out_act == "Remove outliers"){
              rem_out_var<-dlgList(choices = c(out_choice,"Do not remove any outliers                            ")
                                   ,multiple = T,title = "Choose the variables to remove outliers from")$res
              if("Do not remove any outliers                            " %in% rem_out_var){
                dlgMessage(message = "Outliers have been retained in the data")
              }
              else{
                rem_dt<-which(out_choice %in% rem_out_var)
                rem_col<-names(out_ex)[rem_dt]
                out_ls<-which(names(out_list) %in% rem_col)
                rem_ID<-vector()
                for(i in 1:length(out_ls)){
                  rem_id<-out_list[[out_ls[i]]]
                  rem_ID<-c(rem_ID,rem_id)
                }
                rem_ID<-unique(rem_ID)
                dt<-dt[-rem_ID,]
                dlgMessage(message = "The outliers have been removed successfully",type = "ok")
              }
            }
            if(out_act == "Replace outliers"){
              for(i in 1:length(out_var)){
                dt[dt[[out_var[i]]] < (quantile(x = dt[[out_var[i]]],na.rm = T)[2] - (1.5*IQR(x = dt[[out_var[i]]],na.rm = T))),
                   out_var[i] := (quantile(x = dt[[out_var[i]]],na.rm = T)[2] - (1.5*IQR(x = dt[[out_var[i]]],na.rm = T)))]
                dt[dt[[out_var[i]]] > (quantile(x = dt[[out_var[i]]],na.rm = T)[4] + (1.5*IQR(x = dt[[out_var[i]]],na.rm = T))),
                   out_var[i] := (quantile(x = dt[[out_var[i]]],na.rm = T)[4] + (1.5*IQR(x = dt[[out_var[i]]],na.rm = T)))]
              }
              dlgMessage(message = "Outliers have been successfully replaced")
            }
          }
        }
      }
    }
  }
  ################################-----Variable transformation-----################################

  trans<-dlgMessage(message = "Do you want to perform any data transformations?",type = "yesno")$res
  if(trans == "yes"){
    cls<-character()
    for(i in 1:ncol(dt)){
      int_cls<-class(dt[[i]])
      if(length(int_cls) > 1){
        cls[i]<-paste(int_cls,collapse = " ")
      }
      else {
        cls[i]<-int_cls
      }
    }
    num_cls<-which(cls %in% c("numeric","integer"))
    num_dt<-dt[,num_cls,with = F]
    colnames(num_dt)<-colnames(dt)[num_cls]
    cat_cls<-which(cls %in% c("numeric","integer") == F)
    cls_dt<-dt[,cat_cls,with = F]
    colnames(cls_dt)<-colnames(dt)[cat_cls]
    t_choice<-vector()
    if(ncol(num_dt) > 0){
      t_choice<-c(t_choice,"Logarithmic transformation","Normalisation","Standardisation")
    }
    if(ncol(cls_dt) > 0){
      t_choice<-c(t_choice,"Dummification")
    }
    while(trans == "yes"){
      trans_type<-dlgList(choices = t_choice,multiple = T,title = "Choose the method")$res
      if("Logarithmic transformation" %in% trans_type){
        t_log<-dlgList(choices = colnames(num_dt),multiple = T,title = "Log transform")$res
        log_base<-dlgList(choices = c("Exp(e)           ",10,2),preselect = "Exp(e)           ",multiple = F,
                          title = "Log base")$res
        if(log_base == "Exp(e)           "){
          log_base<-exp(1)
        }
        for(i in 1:length(t_log)){
          dt[,t_log[i] := log(x = dt[[t_log[i]]],base = as.numeric(log_base))]
        }
        dlgMessage(message = "Log transformation completed",type = "ok")
      }
      if("Normalisation" %in% trans_type){
        t_norm<-dlgList(choices = colnames(num_dt),multiple = T,title = "Normalise")$res
        f_norm<-function(x){
          (x - min(x,na.rm = T))/(max(x,na.rm = T) - min(x,na.rm = T))
        }
        for(i in 1:length(t_norm)){
          dt[,t_norm[i] := f_norm(dt[[t_norm[i]]])]
        }
        dlgMessage(message = "Normalisation completed",type = "ok")
      }
      if("Standardisation" %in% trans_type){
        t_stan<-dlgList(choices = colnames(num_dt),multiple = T,title = "Standardise")$res
        for(i in 1:length(t_stan)){
          dt[,t_stan[i] := scale(x = dt[[t_stan[i]]],center = T,scale = T)]
        }
        dlgMessage(message = "Standardisation completed",type = "ok")
      }
      if("Dummification" %in% trans_type){
        t_dum<-dlgList(choices = colnames(cls_dt),multiple = T,title = "Dummify")$res
        for(i in 1:length(t_dum)){
          pre<-which(colnames(dt) == t_dum[i]) - 1
          post<-pre + 2
          dum<-dummy(x = t_dum[i],data = dt,sep = "-")
          if(pre <= 0){
            dt<-as.data.table(cbind(dum,dt[,post:ncol(dt),with = F]))
          }
          else if(post > ncol(dt)){
            dt<-as.data.table(cbind(dt[,1:pre,with = F],dum))
          }
          else{
            dt<-as.data.table(cbind(dt[,1:pre,with = F],dum,dt[,post:ncol(dt),with = F]))
          }
        }
        dlgMessage(message = "Dummification completed",type = "ok")
      }
      trans<-dlgMessage(message = "Do you want to perform more data transformations?",type = "yesno")$res
    }
  }

  ################################-----Missing value treatment-----################################

  na_val<-dlgInput(message = c("Enter the missing value character(s)","Use '&' to separate multiple characters"),
                   default = NA)$res
  na_val<-unlist(strsplit(x = na_val,split = "&"))
  if("NA" %in% na_val){
    na_val[which(na_val == "NA")]<-NA
  }
  if("NULL" %in% na_val){
    na_val<-na_val[-which(na_val == "NULL")]
  }
  for(i in 1:ncol(dt)){
    set(x = dt,i = which(dt[[i]] %in% na_val),j = i,value = NA)
  }
  dt[is.null(dt) == T]<-NA

  dt_na<-dt[,lapply(X = .SD,FUN = function(x){
    round(x = (sum(is.na(x))/nrow(dt)*100),digits = 1)
  }),.SDcols = colnames(dt)]
  dt_na<-as.numeric(dt_na)
  names(dt_na)<-colnames(dt)
  if(max(dt_na) <= 0){
    dlgMessage(message = "There are no missing values present in the data",type = "ok")
  }
  else if(max(dt_na) > 0){
    drop_na<-dlgMessage(message = c("Missing values are present in the data","Do you want to remove variables with high missing values?")
                        ,type = "yesno")$res
    if(drop_na == "no"){
      dlgMessage(message = c("The missing values have been retained in the data","It is highly advised to inspect or impute them"),
                 type = "ok")
    }
    else if(drop_na == "yes"){
      na_meth<-dlgList(choices = c("Manually inspect and choose variables to drop","Set missing value threshold (%)"),
                       preselect = "Manually inspect and choose variables to drop",multiple = F,
                       title = "NA omission method")$res
      if(na_meth == "Manually inspect and choose variables to drop"){
        na_col<-names(dt_na)[which(dt_na > 0)]
        na_val<-paste(dt_na[which(dt_na > 0)],"% missing",sep = " ")
        na_choice<-paste(na_col,na_val,sep = " : ")
        na_var<-dlgList(choices = c(na_choice,"Do not drop any variable"),multiple = T,
                        title = "Choose variable(s) to drop")$res
        if("Do not drop any variable" %in% na_var){
          dlgMessage(message = "No variables have been dropped",type = "ok")
        }
        else {
          dt<-dt[,-na_col[which(na_choice %in% na_var)],with = F]
          dlgMessage(message = "The variable(s) have been successfully dropped",type = "ok")
        }
      }
      else if(na_meth == "Set missing value threshold (%)"){
        set_thresh<-"yes"
        while(set_thresh == "yes"){
          na_thresh<-as.numeric(dlgInput(message = "Enter the threshold value (%) for missing values",
                                         default = 30)$res)
          rem_id<-which(dt_na > na_thresh)
          rem_l<-length(rem_id)
          if(rem_l <= 0){
            set_thresh<-dlgMessage(message = c(paste("There are no variables in the data with more than",na_thresh,"% missing values",sep = " "),
                                               "Do you want to reduce the threshold value?"),type = "yesno")$res
            if(set_thresh == "no"){
              dlgMessage(message = "No variables have been dropped",type = "ok")
            }
          }
          else if(rem_l > 0){
            dt<-dt[,-which(dt_na > na_thresh),with = F]
            dlgMessage(message = paste(rem_l,"variable(s) have been successfully dropped",sep = " "))
            set_thresh<-"no"
          }
        }
      }
    }
    dt_nar<-apply(X = dt,MARGIN = 1,FUN = function(x){
      row_na<-length(which(is.na(x) == T))
      l<-length(x)
      na_pct<-round(x = (row_na/l)*100,digits = 1)
      return(na_pct)
    })
    setr_thresh<-"yes"
    while(setr_thresh == "yes"){
      rmv_th<-as.numeric(dlgInput(message = "Enter threshold value (%) for missing values (along rows)",default = 30)$res)
      rmv_row<-which(dt_nar >= rmv_th)
      rmv_l<-length(rmv_row)
      if(rmv_l <= 0){
        setr_thresh<-dlgMessage(message = c(paste("There are no observations with more than",rmv_th,"% missing values",sep = " "),
                                            "Do you want to reduce the threshold value?"),type = "yesno")$res
        if(setr_thresh == "no"){
          dlgMessage(message = "No observations have been dropped",type = "ok")
        }
      }
      if(rmv_l > 0){
        rmv<-dlgMessage(message = c(paste("There are",rmv_l,"observation(s) with more than",rmv_th,"% missing values",sep = " "),
                                    "Do you want to omit them?"),type = "yesno")$res
        if(rmv == "no"){
          dlgMessage(message = c("The observation(s) with missing values have been retained",
                                 "It is highly advised to inspect or impute them"),type = "ok")
        }
        if(rmv == "yes"){
          dt<-dt[-rmv_row,]
          dlgMessage(message = paste(rmv_l,"observation(s) have been omitted",sep = " "),type = "ok")
        }
        setr_thresh<-"no"
      }
    }
  }

  ################################-----Missing value imputation-----################################

  rem_na<-sum(is.na(dt))
  while(rem_na > 0){
    dlgMessage(message = paste("There are",rem_na,"missing values still remaining in the data",sep = " "),type = "ok")
    imp<-dlgMessage(message = "Do you want to impute the remaining missing values?",type = "yesno")$res
    if(imp == "yes"){
      imp_meth<-dlgList(choices = c("knnImputation","CentralImputation","Impute with Mean"),preselect = "CentralImputation",multiple = F,
                        title = "Select imputation technique")$res
      if(imp_meth == "CentralImputation"){
        dt<-centralImputation(data = dt)
        dlgMessage(message = "Imputation completed",type = "ok")
      }
      else if(imp_meth == "knnImputation"){
        imp_cl<-character()
        for(i in 1:ncol(dt)){
          int_cls<-class(dt[[i]])
          if(length(int_cls) > 1){
            imp_cl[i]<-paste(int_cls,collapse = " ")
          }
          else {
            imp_cl[i]<-int_cls
          }
        }
        if("character" %in% imp_cl | "factor" %in% imp_cl | "logical" %in% imp_cl){
          col_names<-colnames(dt)
          ch_cl<-which(imp_cl %in% c("numeric","integer") == F)
          num_cl<-which(imp_cl %in% c("numeric","integer"))
          ch_l<-length(ch_cl)
          dlgMessage(message = c("knn will only impute missing values for the numerical data",
                                 "Use CentralImputation for imputing characters"),type = "ok")
          dt_num<-dt[,num_cl,with = F]
          dt_ch<-dt[,ch_cl,with = F]
          if(nrow(dt_num) <= 0 | ncol(dt_num) <= 0){
            dlgMessage(message = c("There are no more numerical variables with missing values",
                                   "Please use CentralImputation to impute the missing values in the categorical variables"),
                       type = "ok")
          }
          else {
            k_val<-as.integer(dlgInput(message = "Enter the number of nearest neighbors (k) to consider",default = 10)$res)
            k_meth<-dlgList(choices = c("weighAvg","median"),preselect = "weighAvg",multiple = F,title = "Choose knn method")$res
            dlgMessage(message = c("Imputing missing values using knn","Please wait as this might take a while"),
                       type = "ok")
            dt_imp<-knnImputation(data = dt_num,k = k_val,meth = k_meth,scale = T)
            dt<-as.data.table(cbind(dt_imp,dt_ch))
            dt<-dt[,col_names,with = F]
            dlgMessage(message = "Imputation completed",type = "ok")
          }
        }
        else {
          k_val<-as.integer(dlgInput(message = "Enter the number of nearest neighbors (k) to consider",default = 10)$res)
          k_meth<-dlgList(choices = c("weighAvg","median"),preselect = "weighAvg",multiple = F,title = "Choose knn imputation method")$res
          dlgMessage(message = c("Imputing missing values using knn","Please wait as this might take a while"),
                     type = "ok")
          dt<-knnImputation(data = dt,k = k_val,meth = k_meth)
          dlgMessage(message = "Imputation completed",type = "ok")
        }
      }
      else if(imp_meth == "Impute with Mean"){
        imp_cl<-character()
        for(i in 1:ncol(dt)){
          int_cls<-class(dt[[i]])
          if(length(int_cls) > 1){
            imp_cl[i]<-paste(int_cls,collapse = " ")
          }
          else {
            imp_cl[i]<-int_cls
          }
        }
        if("character" %in% imp_cl | "factor" %in% imp_cl | "logical" %in% imp_cl){
          col_names<-colnames(dt)
          ch_cl<-which(imp_cl %in% c("numeric","integer") == F)
          ch_l<-length(ch_cl)
          dlgMessage(message = c("Imputation with Mean will only impute missing values for the numerical data",
                                 "Use CentralImputation to impute missing values in the categorical variables"),type = "ok")
          dt_num<-dt[,-ch_cl,with = F]
          dt_ch<-dt[,ch_cl,with = F]
          Impute_Mean<-function(x){
            x[which(is.na(x) == T)]<-mean(x,na.rm = T)
          }
          if(nrow(dt_num) <= 0 | ncol(dt_num) <= 0){
            dlgMessage(message = c("There are no more numerical variables with missing values",
                                   "Please use CentralImputation to impute the missing values in the categorical variables"),
                       type = "ok")
          }
          else {
            dlgMessage(message = "Imputing missing values with the mean value",type = "ok")
            dt_imp<-dt_num[,lapply(X = .SD,FUN = Impute_Mean),.SDcols = colnames(dt_num)]
            dt<-as.data.table(cbind(dt_imp,dt_ch))
            dt<-dt[,col_names,with = F]
            dlgMessage(message = "Imputation completed",type = "ok")
          }
        }
        else {
          Impute_Mean<-function(x){
            x[which(is.na(x) == T)]<-mean(x,na.rm = T)
          }
          dlgMessage(message = "Imputing missing values with the mean value",type = "ok")
          dt<-dt[,lapply(X = .SD,FUN = Impute_Mean),.SDcols = colnames(dt)]
          dlgMessage(message = "Imputation completed",type = "ok")
        }
      }
      rem_na<-sum(is.na(dt))
    }
    else if(imp == "no"){
      rem_na<-0
    }
  }

  ################################-----Class Imbalance Correction-----################################

  tar<-dlgMessage(message = "Does the data contain a target/response variable?",type = "yesno")$res
  if(tar == "yes"){
    target<-dlgList(choices = colnames(dt),multiple = F,title = "Choose the target/response variable")$res
    target<-which(colnames(dt) == target)
    pred_type<-dlgList(choices = c("Regression                    ","Classification"),multiple = F,title = "Predictive Analysis Type")$res
  }
  else if(tar == "no"){
    target<-vector()
    pred_type<-"None"
  }

  if(length(target) > 0 && pred_type == "Classification" && length(unique(dt[[target]])) <= 2){
    tar_tab<-table(dt[[target]])
    major<-names(which.max(tar_tab))
    major_cnt<-max(tar_tab)
    minor<-names(which.min(tar_tab))
    minor_cnt<-min(tar_tab)
    tar_prop<-major_cnt/minor_cnt
    if(major_cnt == minor_cnt){
      dlgMessage(message = "The input dataset has no class imbalance",type = "ok")
      fix_imb<-"no"
    }
    else if(major_cnt != minor_cnt && tar_prop < 3) {
      fix_imb<-dlgMessage(message = c(paste(major,"is the major target class. Number of occurences:",major_cnt,sep = " "),
                             paste(minor,"is the minor target class. Number of occurences:",minor_cnt,sep = " "),
                             "Do you want to fix the class imbalance?"),type = "yesno")$res
    }
    else if(major_cnt != minor_cnt && tar_prop >= 3){
      fix_imb<-dlgMessage(message = c(paste(major,"is the major target class. Number of occurences:",major_cnt,sep = " "),
                                      paste(minor,"is the minor target class. Number of occurences:",minor_cnt,sep = " "),
                                      "Do you want to fix the class imbalance?",
                                      "NOTE: Major class imbalance has been observed in your dataset"),type = "yesno")$res
    }
    if(fix_imb == "yes"){
      rose_form<-as.formula(paste(colnames(dt)[target],"~ .",sep = " "))
      dt<-as.data.table(ROSE(formula = rose_form,data = dt)$data)
      dlgMessage(message = "The class imbalance in the data has been fixed successfully",type = "ok")
    }
  }
  else if(length(target) > 0 && pred_type == "Classification" && length(unique(dt[[target]])) > 2) {
    dlgMessage(message = c("The target variable contains more than 2 classes",
                           "Class Imbalance correction will be skipped"),type = "ok")
  }
  else if(length(target) > 0 && pred_type != "Classification"){
    dlgMessage(message = c("The dataset is for Regression analysis",
                           "Class Imbalance correction will be skipped"),type = "ok")
  }

  ################################-----Output-----################################

  sv_pre<-dlgMessage(message = c("Pre-processing completed","Do you want to save the pre-processed data?"),
             type = "yesno")$res
  if(sv_pre == "yes"){
    sv_form<-dlgList(choices = c(".csv",".xls",".xlsx"),preselect = ".xlsx",multiple = F,
                     title = "Choose output file format")$res
    if(sv_form == ".csv"){
      write.csv2(x = dt,row.names = F,file = "Pre_Proc-Data_Pre-processed.csv")
    }
    else if(sv_form == ".xls"){
      write.xlsx(x = dt,col.names = T,row.names = F,file = "Pre_Proc-Data_Pre-processed.xls")
    }
    else if(sv_form == ".xlsx"){
      write.xlsx(x = dt,col.names = T,row.names = F,file = "Pre_Proc-Data_Pre-processed.xlsx")
    }

    dlgMessage(message = "The output dataset has been saved in your working directory",type = "ok")
  }
  return(dt)
}


#' Perform Exploratory Data Analysis
#'
#' Generates a variety of plots and reports summarising the variable characteristics with simple user selections
#' @param dataset The pre-processed dataset to be analysed
#' @export

Auto_EDA<-function(dataset){
  require(data.table)
  require(svDialogs)
  require(moments)
  require(openxlsx)
  require(ggplot2)
  require(fitdistrplus)
  require(DescTools)
  require(ICC)
  require(leaps)
  require(pls)
  require(corrplot)
  require(car)
  require(mctest)

  start<-dlgMessage(message = c("It is highly advised to pre-process data using the 'Pre_Proc' function in this package before EDA",
                                "Do you want to continue?"),type = "yesno")$res
  suppressWarnings(if(start == "yes"){
    dlgMessage(message = "Choose the directory where reports and plots will be saved",type = "ok")
    sv_path<-choose.dir()
    file.create(paste(sv_path,"\\Auto_EDA-Error_log.txt",sep = ""))
    dt<-as.data.table(dataset)
    tar<-dlgMessage(message = "Does the data contain a target/response variable?",type = "yesno")$res
    if(tar == "yes"){
      target_nm<-dlgList(choices = colnames(dt),multiple = F,title = "Choose the target/response variable")$res
      target<-which(colnames(dt) == target_nm)
      pred_type<-dlgList(choices = c("Regression                    ","Classification"),multiple = F,title = "Predictive Analysis Type")$res
      if(pred_type == "Classification"){
        dt[[target]]<-as.factor(dt[[target]])
      }
    }
    else if(tar == "no"){
      target<-vector()
      pred_type<-"None"
    }
    cls<-character()
    for(i in 1:ncol(dt)){
      int_cls<-class(dt[[i]])
      if(length(int_cls) > 1){
        cls[i]<-paste(int_cls,collapse = " ")
      }
      else {
        cls[i]<-int_cls
      }
    }
    dt_num<-dt[,which(cls %in% c("numeric","integer")),with = F]
    dt_cat<-dt[,which(cls %in% c("numeric","integer") == F),with = F]

    ################################-----Summary stats-----################################

    if(ncol(dt_num) > 0){
      quants<-vector(mode = "list",length = ncol(dt_num))
      min_val<-q1_val<-med_val<-mean_val<-q3_val<-max_val<-skew_val<-kurt_val<-remarks<-sd_val<-vector(length = ncol(dt_num))
      names(min_val)<-names(q1_val)<-names(med_val)<-names(mean_val)<-names(q3_val)<-names(max_val)<-colnames(dt_num)
      names(skew_val)<-names(kurt_val)<-names(remarks)<-names(sd_val)<-colnames(dt_num)
      err_num_sum<-err_num_sum_var<-vector()
      for(i in 1:ncol(dt_num)){
        tryCatch({
          quants[[i]]<-quantile(dt_num[[i]])
          min_val[i]<-quants[[i]][1]
          q1_val[i]<-quants[[i]][2]
          med_val[i]<-quants[[i]][3]
          mean_val[i]<-mean(x = dt_num[[i]],na.rm = T)
          q3_val[i]<-quants[[i]][4]
          max_val[i]<-quants[[i]][5]
          sd_val[i]<-sd(x = dt_num[[i]],na.rm = T)
          skew_val[i]<-skewness(x = dt_num[[i]],na.rm = T)
          if(is.nan(skew_val[i]) == T){
            skew_val[i]<-0
          }
          kurt_val[i]<-kurtosis(x = dt_num[[i]],na.rm = T)-3
          if(is.nan(kurt_val[i]) == T){
            kurt_val[i]<-(-1.2)
          }
          if(skew_val[i] < 0){
            remarks[i]<-"The variable is left skewed"
          }
          else if(skew_val[i] == 0){
            remarks[i]<-"The variable is symmetrically distributed"
          }
          else {
            remarks[i]<-"The variable is right skewed"
          }
          if(kurt_val[i] < 0){
            remarks[i]<-paste(remarks[i],"and platykurtic",sep = " ")
          }
          else if(kurt_val[i] == 0){
            remarks[i]<-paste(remarks[i],"and mesokurtic",sep = " ")
          }
          else {
            remarks[i]<-paste(remarks[i],"and leptokurtic",sep = " ")
          }
        },error = function(e){
          write(x = paste(paste(Sys.time(),"Data summary could not be generated for the variable :",colnames(dt_num)[i],sep = " "),
                          paste("ERROR:",e$message,sep = " "),sep = "\n"),
                file = paste(sv_path,"\\Auto_EDA-Error_log.txt",sep = ""),append = TRUE)
        })
      }

      dt_sum<-data.table("Variable" = colnames(dt_num),"Minimum" = min_val,"1st Quartile" = q1_val,
                         "Median" = med_val,"Mean" = mean_val,"3rd Quartile" = q3_val,"Maximum" = max_val,
                         "Standard Deviation" = sd_val,"Skewness Measure" = skew_val,
                         "Excess Kurtosis" = kurt_val,"Remarks" = remarks,stringsAsFactors = F)
      cat("\n\nDATA SUMMARY (Numeric/integer variables)\n\n")
      print(dt_sum)
      sv<-dlgMessage(message = c("The numerical data summary has been printed in the console",
                                 "Do you want to save the summary report?"),type = "yesno")$res
      if(sv == "yes"){
        sv_form<-dlgList(choices = c(".csv",".xls",".xlsx"),preselect = ".xlsx",multiple = F,
                         title = "Choose output file format")$res
        if(sv_form == ".csv"){
          write.csv2(x = dt_sum,row.names = F,file = paste(sv_path,"Auto_EDA-Data_Summary_Num.csv",sep = "\\"))
        }
        else if(sv_form == ".xls"){
          write.xlsx(x = dt_sum,col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-Data_Summary_Num.xls",sep = "\\"))
        }
        else if(sv_form == ".xlsx"){
          write.xlsx(x = dt_sum,col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-Data_Summary_Num.xlsx",sep = "\\"))
        }

        dlgMessage(message = "The data summary report has been successfully saved in the chosen directory",type = "ok")
      }
    }
    else{
      dlgMessage(message = c("There are no numeric/integer variables in the data",
                             "Data summary will be provided only for the categorical variables"),type = "ok")
    }

    if(ncol(dt_cat) > 0){
      unq_lvl<-mode_val<-mode_cnt<-mode_pct<-vector(length = ncol(dt_cat))
      err_cat_sum<-err_cat_sum_var<-vector()
      for(i in 1:ncol(dt_cat)){
        tryCatch({
          unq_lvl[i]<-length(unique(dt_cat[[i]]))
          tab<-table(dt_cat[[i]])
          mode_val[i]<-names(which.max(tab))
          mode_cnt[i]<-tab[which.max(tab)]
          mode_pct[i]<-(mode_cnt[i]/length(dt_cat[[i]]))*100
        },error = function(e){
          write(x = paste(paste(Sys.time(),"Data summary could not be generated for the variable :",colnames(dt_num)[i],sep = " "),
                          paste("ERROR:",e$message,sep = " "),sep = "\n"),
                file = paste(sv_path,"\\Auto_EDA-Error_log.txt",sep = ""),append = TRUE)
        })
      }

      dt_sum_cat<-data.table("Variable" = colnames(dt_cat),"Unique values/levels" = unq_lvl,
                             "Mode" = mode_val,"Mode count" = mode_cnt,"Mode count (%)" = mode_pct,stringsAsFactors = F)
      cat("\n\nDATA SUMMARY (Character/factor variables)\n\n")
      print(dt_sum_cat)
      sv_cat<-dlgMessage(message = c("The categorical data summary has been printed in the console",
                                     "Do you want to save the summary report?"),type = "yesno")$res
      if(sv_cat == "yes"){
        sv_form<-dlgList(choices = c(".csv",".xls",".xlsx"),preselect = ".xlsx",multiple = F,
                         title = "Choose output file format")$res
        if(sv_form == ".csv"){
          write.csv2(x = dt_sum_cat,row.names = F,file = paste(sv_path,"Auto_EDA-Data_Summary_Cat.csv",sep = "\\"))
        }
        else if(sv_form == ".xls"){
          write.xlsx(x = dt_sum_cat,col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-Data_Summary_Cat.xls",sep = "\\"))
        }
        else if(sv_form == ".xlsx"){
          write.xlsx(x = dt_sum_cat,col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-Data_Summary_Cat.xlsx",sep = "\\"))
        }

        dlgMessage(message = "The data summary report has been successfully saved in the chosen directory",type = "ok")
      }
    }
    else{
      dlgMessage(message = c("There are no character/factor variables in the data",
                             "Data summary will be provided only for the numeric/integer variables"),type = "ok")
    }

    if(ncol(dt_cat) > 0){
      cv<-dlgMessage(message = "Do you want to calculate Cramer's V for the categorical variables?",
                     type = "yesno")$res
      if(cv == "yes"){
        cv_mat<-PairApply(x = dt_cat,FUN = CramerV,symmetric = TRUE)
        var_nm<-rownames(cv_mat)
        cv_tab<-as.data.table(cbind("Variable" = var_nm,cv_mat))
        print(cv_tab)
        sv_cv<-dlgMessage(message = c("The Cramer's V for the categorical variables has been printed in the console",
                                      "Do you want to save it as a report?"),type = "yesno")$res
        if(sv_cv == "yes"){
          sv_form<-dlgList(choices = c(".csv",".xls",".xlsx"),preselect = ".xlsx",multiple = F,
                           title = "Choose output file format")$res
          if(sv_form == ".csv"){
            write.csv2(x = cv_tab,row.names = F,file = paste(sv_path,"Auto_EDA-Cramer's_V.csv",sep = "\\"))
          }
          else if(sv_form == ".xls"){
            write.xlsx(x = cv_tab,col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-Cramer's_V.xls",sep = "\\"))
          }
          else if(sv_form == ".xlsx"){
            write.xlsx(x = cv_tab,col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-Cramer's_V.xlsx",sep = "\\"))
          }

          dlgMessage(message = "The Cramer's V report has been successfully saved in the chosen directory",type = "ok")
        }
      }
    }

    if(length(target) > 0 && cls[target] %in% c("numeric","integer") && ncol(dt_cat) > 0){
      icc<-dlgMessage(message = c("Do you want to calculate the Interclass Correlation Coefficients?",
                                  "NOTE : The ICC is calculated for each categorical variable with respect to the target"),
                      type = "yesno")$res
      if(icc == "yes"){
        feats<-colnames(dt_cat)
        icc_val<-vector()
        for(i in 1:length(feats)){
          icc_val[i]<-ICCbare(x = dt_cat[[i]],y = dt[[target]])
        }
        dt_icc<-data.table("Variable" = feats,"ICC" = icc_val)
        print(dt_icc)
        sv_icc<-dlgMessage(message = c("The ICC for each variable have been printed in the console",
                                       "Do you want to save it as a report?"),type = "yesno")$res
        if(sv_icc == "yes"){
          sv_form<-dlgList(choices = c(".csv",".xls",".xlsx"),preselect = ".xlsx",multiple = F,
                           title = "Choose output file format")$res
          if(sv_form == ".csv"){
            write.csv2(x = dt_icc,row.names = F,file = paste(sv_path,"Auto_EDA-ICC.csv",sep = "\\"))
          }
          else if(sv_form == ".xls"){
            write.xlsx(x = dt_icc,col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-ICC.xls",sep = "\\"))
          }
          else if(sv_form == ".xlsx"){
            write.xlsx(x = dt_icc,col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-ICC.xlsx",sep = "\\"))
          }

          dlgMessage(message = "The ICC report has been successfully saved in the chosen directory",type = "ok")
        }
      }
    }

    ################################-----PCA-----################################

    if(ncol(dt_num) > 0){
      pca_do<-dlgMessage(message = "Do you want to perform Principal Component Analysis?",type = "yesno")$res
      if(pca_do == "yes"){
        pca_go<-dlgMessage(message = c("PCA will only take the numerical data as input","Continue?"),type = "yesno")$res
        if(pca_go == "yes"){
          if(ncol(dt_num) < 3){
            pca_force<-dlgMessage(message = c(paste("There are only",ncol(dt_num),"numerical variables in the data",sep = " "),
                                              "Do you still want to perform PCA?"),type = "yesno")$res
          }
          else {
            pca_force<-"yes"
          }
          if(pca_force == "yes"){
            pca_tar<-dlgMessage(message = "Does the data contain a numeric target/response variable?",type = "yesno")$res
            if(pca_tar == "no"){
              pca_dt<-dt_num
            }
            else if(pca_tar == "yes"){
              pca_tv<-dlgList(choices = c(colnames(dt_num),"Not listed"),multiple = F,title = "Select target")$res
              if(pca_tv == "Not listed"){
                pca_dt<-dt_num
              }
              else {
                tar_pca<-dt_num[[which(colnames(dt_num) == pca_tv)]]
                names(tar_pca)<-pca_tv
                pca_dt<-dt_num[,-which(colnames(dt_num) == pca_tv),with = F]
              }
            }
            pca_sc<-dlgMessage(message = c("Do you want to scale or center the data before PCA?",
                                           "It is suggested to both center and scale the data"),type = "yesno")$res
            if(pca_sc == "yes"){
              pca_op<-dlgList(choices = c("Scale","Center","Both"),multiple = F,preselect = "Both",title = "Choose method")$res
              if(pca_op == "Scale"){
                pca<-prcomp(x = pca_dt,center = F,scale. = T)
              }
              else if(pca_op == "Center"){
                pca<-prcomp(x = pca_dt,center = T,scale. = F)
              }
              else if(pca_op == "Both"){
                pca<-prcomp(x = pca_dt,center = T,scale. = T)
              }
            }
            else {
              pca<-prcomp(x = pca_dt,center = F,scale. = F)
            }
            vars<-apply(X = pca$x,MARGIN = 2,FUN = var)
            props<-vars/sum(vars)
            cumvar<-as.data.table(melt(round(x = cumsum(x = props) * 100,digits = 2)))
            cumvar<-cumvar[,.("PC" = names(vars),"Cumulative Variance Explained (%)" = value)]
            propvar<-as.data.table(melt(round(x = props * 100,digits = 2)))
            propvar<-propvar[,.("PC" = factor(x = names(vars),levels = names(vars)),"Variance Expained (%)" = value)]
            cat("Cumulative percentage of variation explained by each Principal Component:\n\n")
            print(cumvar)
            fl_nm<-paste(sv_path,"\\Screeplot.jpeg",sep = "")
            jpeg(filename = fl_nm,quality = 100)
            screeplot<-ggplot(data = propvar) +
              geom_col(mapping = aes(x = PC,y = propvar$`Variance Expained (%)`),fill = "blue") +
              ggtitle(label = "Screeplot",subtitle = "Principal Component Analysis") +
              xlab(label = "Principal Component") + ylab(label = "Variance Explained (%)") +
              theme(legend.position = "none")
            print(screeplot)
            dev.off()
            dlgMessage(message = c("The PCA results have been printed in the console and the screeplot has been generated and saved in your working directory",
                                   "Inspect them before proceeding"),type = "ok")
            sv_pca<-dlgMessage(message = "Do you want to save the PCA scores (transformed data)?",type = "yesno")$res
            if(sv_pca == "yes"){
              sv_form<-dlgList(choices = c(".csv",".xls",".xlsx"),preselect = ".xlsx",multiple = F,
                               title = "Choose output file format")$res
              if(sv_form == ".csv"){
                write.csv2(x = as.data.table(pca$x),row.names = F,file = paste(sv_path,"Auto_EDA-PCA_Scores.csv",sep = "\\"))
              }
              else if(sv_form == ".xls"){
                write.xlsx(x = as.data.table(pca$x),col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-PCA_Scores.xls",sep = "\\"))
              }
              else if(sv_form == ".xlsx"){
                write.xlsx(x = as.data.table(pca$x),col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-PCA_Scores.xlsx",sep = "\\"))
              }
            }
            rep_pca<-dlgMessage(message = "Do you want to replace the numerical variables with Principal Components?",
                                type = "yesno")$res
            if(rep_pca == "yes" && pca_tar == "yes"){
              pca_no<-as.numeric(dlgInput(message = c("Enter the number of Principal Components to be included in the data",
                                                      paste("Maximum value allowed:",ncol(pca$x),sep = " ")),default = 2)$res)
              pca_bind<-as.data.table(pca$x[,1:pca_no])
              colnames(pca_bind)<-colnames(pca$x)[1:pca_no]
              dt<-dt[,which(cls %in% c("numeric","integer") == F),with = F]
              if(ncol(dt) > 0){
                dt<-as.data.table(cbind(dt,pca_bind,tar_pca))
                colnames(dt)[ncol(dt)]<-names(tar_pca)
                target<-ncol(dt)
              }
              else {
                dt<-as.data.table(cbind(pca_bind,tar_pca))
                colnames(dt)[ncol(dt)]<-names(tar_pca)
                target<-ncol(dt)
              }
              sv_newdt<-dlgMessage(message = "Do you want to save the new data with Principal Components?",type = "yesno")$res
              if(sv_newdt == "yes"){
                sv_form<-dlgList(choices = c(".csv",".xls",".xlsx"),preselect = ".xlsx",multiple = F,
                                 title = "Choose output file format")$res
                if(sv_form == ".csv"){
                  write.csv2(x = dt,row.names = T,file = paste(sv_path,"Auto_EDA-PCA_New_Data.csv",sep = "\\"))
                }
                else if(sv_form == ".xls"){
                  write.xlsx(x = dt,col.names = T,row.names = T,file = paste(sv_path,"Auto_EDA-PCA_New_Data.xls",sep = "\\"))
                }
                else if(sv_form == ".xlsx"){
                  write.xlsx(x = dt,col.names = T,row.names = T,file = paste(sv_path,"Auto_EDA-PCA_New_Data.xlsx",sep = "\\"))
                }
              }
            }
            else if(rep_pca == "yes" && pca_tar == "no"){
              pca_no<-as.numeric(dlgInput(message = c("Enter the number of Principal Components to be included in the data",
                                                      paste("Maximum value allowed:",ncol(pca$x),sep = " ")),default = 2)$res)
              pca_bind<-as.data.table(pca$x[,1:pca_no])
              colnames(pca_bind)<-colnames(pca$x)[1:pca_no]
              dt<-dt[,-which(cls %in% c("numeric","integer")),with = F]
              if(ncol(dt) > 0){
                dt<-as.data.table(cbind(dt,pca_bind))
              }
              else {
                dt<-pca_bind
              }
              sv_newdt<-dlgMessage(message = "Do you want to save the new data with Principal Components?",type = "yesno")$res
              if(sv_newdt == "yes"){
                sv_form<-dlgList(choices = c(".csv",".xls",".xlsx"),preselect = ".xlsx",multiple = F,
                                 title = "Choose output file format")$res
                if(sv_form == ".csv"){
                  write.csv2(x = dt,row.names = F,file = paste(sv_path,"Auto_EDA-PCA_New_Data.csv",sep = "\\"))
                }
                else if(sv_form == ".xls"){
                  write.xlsx(x = dt,col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-PCA_New_Data.xls",sep = "\\"))
                }
                else if(sv_form == ".xlsx"){
                  write.xlsx(x = dt,col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-PCA_New_Data.xlsx",sep = "\\"))
                }
              }
            }
            sv_rot<-dlgMessage(message = "Do you want to save the PCA rotations?",type = "yesno")$res
            if(sv_rot == "yes"){
              sv_form<-dlgList(choices = c(".csv",".xls",".xlsx"),preselect = ".xlsx",multiple = F,
                               title = "Choose output file format")$res
              if(sv_form == ".csv"){
                write.csv2(x = pca$rotation,row.names = T,file = paste(sv_path,"Auto_EDA-PCA_Rotation.csv",sep = "\\"))
              }
              else if(sv_form == ".xls"){
                write.xlsx(x = pca$rotation,col.names = T,row.names = T,file = paste(sv_path,"Auto_EDA-PCA_Rotation.xls",sep = "\\"))
              }
              else if(sv_form == ".xlsx"){
                write.xlsx(x = pca$rotation,col.names = T,row.names = T,file = paste(sv_path,"Auto_EDA-PCA_Rotation.xlsx",sep = "\\"))
              }
            }
          }
        }
      }
    }

    ################################-----Best subsets-----################################

    if(length(target) > 0){
      reg<-dlgMessage(message = c("Do you want to check for the best subsets in the data for predictive analysis?",
                                  "NOTE: Categorical variables present in the data will be dummified"),type = "yesno")$res
      if(reg == "yes"){
        nvmax<-as.numeric(dlgInput(message = c("Enter the maximum size of subsets to examine",
                                               paste("Maximum value allowed:",ncol(dt),sep = " ")),
                                   default = round(x = ncol(dt)*0.5,digits = 0))$res)
        reg_form<-as.formula(paste(target_nm,"~ .",sep = " "))
        reg_res<-summary(regsubsets(reg_form,data = dt,nvmax = nvmax))$which
        id<-1:nvmax
        reg_res<-as.data.table(cbind("No of Subsets" = id,reg_res[,-1]))
        print(reg_res)
        sv_reg<-dlgMessage(message = c("The best variables for each no of subsets has been printed in the console",
                                       "Do you want to save the results?"),type = "yesno")$res
        if(sv_reg == "yes"){
          sv_form<-dlgList(choices = c(".csv",".xls",".xlsx"),preselect = ".xlsx",multiple = F,
                           title = "Choose output file format")$res
          if(sv_form == ".csv"){
            write.csv2(x = reg_res,row.names = F,file = paste(sv_path,"Auto_EDA-Best_Subsets.csv",sep = "\\"))
          }
          else if(sv_form == ".xls"){
            write.xlsx(x = reg_res,col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-Best_Subsets.xls",sep = "\\"))
          }
          else if(sv_form == ".xlsx"){
            write.xlsx(x = reg_res,col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-Best_Subsets.xlsx",sep = "\\"))
          }
        }
      }
    }

    ################################-----Partial Least Squares-----################################

    if(length(target) > 0 && pred_type != "Classification"){
      pls_do<-dlgMessage(message = c("Do you want to check components using PLS Regression?",
                                     "NOTE: Categorical variables present in the data will be dummified"),
                         type = "yesno")$res
      if(pls_do == "yes"){
        pls_form<-as.formula(paste(target_nm,"~ .",sep = " "))
        res_pls<-plsr(pls_form,data = dt,scale = T)
        print(summary(res_pls))
        fl_nm<-paste(sv_path,"\\PLS_val_plot.jpeg",sep = "")
        jpeg(filename = fl_nm,quality = 100)
        validationplot(object = res_pls)
        dev.off()
        dlgMessage(message = c("The summary of results from PLS Regression have been printed in the console",
                               "The validation plot has been generated and saved in your working directory",
                               "Inspect them before proceeding"),type = "ok")
        sv_pls<-dlgMessage(message = "Do you want to save the PLSR scores?",type = "yesno")$res
        if(sv_pls == "yes"){
          sv_form<-dlgList(choices = c(".csv",".xls",".xlsx"),preselect = ".xlsx",multiple = F,
                           title = "Choose output file format")$res
          if(sv_form == ".csv"){
            write.csv2(x = as.data.table(unclass(res_pls$scores)),row.names = F,file = paste(sv_path,"Auto_EDA-PLSR_Components.csv",sep = "\\"))
          }
          else if(sv_form == ".xls"){
            write.xlsx(x = as.data.table(unclass(res_pls$scores)),col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-PLSR_Components.xls",sep = "\\"))
          }
          else if(sv_form == ".xlsx"){
            write.xlsx(x = as.data.table(unclass(res_pls$scores)),col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-PLSR_Components.xlsx",sep = "\\"))
          }
        }
        pls_rep<-dlgMessage(message = "Do you want to replace the data with the PLSR components?",type = "yesno")$res
        if(pls_rep == "yes"){
          pls_no<-as.numeric(dlgInput(message = c("Enter the number of PLSR components to use",
                                                  paste("Maximum value allowed:",ncol(res_pls$scores),sep = " ")),default = 2)$res)
          dt_pls<-as.data.table(res_pls$scores[,1:pls_no])
          pls_tar<-dt[[which(colnames(dt) == target_nm)]]
          dt<-as.data.table(cbind(dt_pls,pls_tar))
          colnames(dt)[ncol(dt)]<-target_nm
          target<-ncol(dt)
          sv_plsc<-dlgMessage(message = "Do you want to save the new data containing the PLSR components?",type = "yesno")$res
          if(sv_plsc == "yes"){
            sv_form<-dlgList(choices = c(".csv",".xls",".xlsx"),preselect = ".xlsx",multiple = F,
                             title = "Choose output file format")$res
            if(sv_form == ".csv"){
              write.csv2(x = dt,row.names = F,file = paste(sv_path,"Auto_EDA-PLSR_New_Data.csv",sep = "\\"))
            }
            else if(sv_form == ".xls"){
              write.xlsx(x = dt,col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-PLSR_New_Data.xls",sep = "\\"))
            }
            else if(sv_form == ".xlsx"){
              write.xlsx(x = dt,col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-PLSR_New_Data.xlsx",sep = "\\"))
            }
          }
          dlgMessage(message = "The data with the PLSR components has been successfully saved in your working directory",type = "ok")
        }
      }
    }

    ################################-----VIF and Collinearity Check-----################################

    cls<-character()
    for(i in 1:ncol(dt)){
      int_cls<-class(dt[[i]])
      if(length(int_cls) > 1){
        cls[i]<-paste(int_cls,collapse = " ")
      }
      else {
        cls[i]<-int_cls
      }
    }
    dt_num<-dt[,which(cls %in% c("numeric","integer")),with = F]
    dt_cat<-dt[,which(cls %in% c("numeric","integer") == F),with = F]

    if(ncol(dt_num) > 1 && length(target) > 0 && pred_type != "Classification"){
      mcol<-dlgMessage(message = c("Do you want to perform Multicollinearity Diagnosis on the dataset?",
                                   "NOTE: Only numerical variables will be considered"),type = "yesno")$res
      if(mcol == "yes"){
        tar_num<-which(colnames(dt_num) == target_nm)
        dt_mcm<-imcdiag(x = dt_num[,-tar_num,with = F],y = dt_num[[tar_num]],na.rm = T,all = T)$idiags
        mc_var<-rownames(dt_mcm)
        dt_mcm<-as.data.table(cbind("Variable" = mc_var,dt_mcm))
        cat("Multicollinearity Diagnostic Measures:\n")
        print(dt_mcm)
        sv_mcm<-dlgMessage(message = c("The Multicollinearity Diagnostic Measures have been computed and printed in the console",
                                       "Do you want to save them as a report?"),type = "yesno")$res
        if(sv_mcm == "yes"){
          sv_form<-dlgList(choices = c(".csv",".xls",".xlsx"),preselect = ".xlsx",multiple = F,
                           title = "Choose output file format")$res
          if(sv_form == ".csv"){
            write.csv2(x = dt_mcm,row.names = F,file = paste(sv_path,"Auto_EDA-Multicollinearity_Measures.csv",sep = "\\"))
          }
          else if(sv_form == ".xls"){
            write.xlsx(x = dt_mcm,col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-Multicollinearity_Measures.xls",sep = "\\"))
          }
          else if(sv_form == ".xlsx"){
            write.xlsx(x = dt_mcm,col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-Multicollinearity_Measures.xlsx",sep = "\\"))
          }
        }
        dt_mcd<-imcdiag(x = dt_num[,-tar_num,with = F],y = dt_num[[tar_num]],na.rm = T,all = T)$alldiag
        mc_var<-rownames(dt_mcd)
        dt_mcd<-as.data.table(cbind("Variable" = mc_var,dt_mcd))
        cat("Multicollinearity Detection:\n")
        print(dt_mcd)
        sv_mcd<-dlgMessage(message = c("The results from Multicollinearity detection have been printed on the console",
                                       "Do you want to save the results as a report?",
                                       "NOTE: TRUE indicates the presence of multicollinearity as per the computed diagnostic measure"),type = "yesno")$res
        if(sv_mcd == "yes"){
          sv_form<-dlgList(choices = c(".csv",".xls",".xlsx"),preselect = ".xlsx",multiple = F,
                           title = "Choose output file format")$res
          if(sv_form == ".csv"){
            write.csv2(x = dt_mcd,row.names = F,file = paste(sv_path,"Auto_EDA-Multicollinearity_Detection.csv",sep = "\\"))
          }
          else if(sv_form == ".xls"){
            write.xlsx(x = dt_mcd,col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-Multicollinearity_Detection.xls",sep = "\\"))
          }
          else if(sv_form == ".xlsx"){
            write.xlsx(x = dt_mcd,col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-Multicollinearity_Detection.xlsx",sep = "\\"))
          }
        }
        dlgMessage("Multicollinearity check has been successfully completed")
      }
    }
    else if(length(target) > 0 && pred_type != "Classification"){
      mcol<-dlgMessage(message = "Do you want to perform Multicollinearity Diagnosis on the dataset?",type = "yesno")$res
      if(mcol == "yes"){
        mod_lm<-glm(formula = as.formula(paste(target_nm,".",sep = "~")),data = dt)
        dt_vif<-vif(mod = mod_lm)
        if(class(dt_vif) == "numeric"){
          res_vif<-as.data.table(cbind("Variable" = names(dt_vif),"VIF" = dt_vif))
        }
        else {
          res_vif<-as.data.table(cbind("Variable" = rownames(dt_vif),dt_vif))
        }
        cat("Multicollinearity Diagnostic Measures:\n")
        print(res_vif)
        sv_vif<-dlgMessage(message = c("The VIF measures have been computed and printed in the console",
                                       "Do you want to save them as a report?"),type = "yesno")$res
        if(sv_vif == "yes"){
          sv_form<-dlgList(choices = c(".csv",".xls",".xlsx"),preselect = ".xlsx",multiple = F,
                           title = "Choose output file format")$res
          if(sv_form == ".csv"){
            write.csv2(x = res_vif,row.names = F,file = paste(sv_path,"Auto_EDA-Multicollinearity_VIF.csv",sep = "\\"))
          }
          else if(sv_form == ".xls"){
            write.xlsx(x = res_vif,col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-Multicollinearity_VIF.xls",sep = "\\"))
          }
          else if(sv_form == ".xlsx"){
            write.xlsx(x = res_vif,col.names = T,row.names = F,file = paste(sv_path,"Auto_EDA-Multicollinearity_VIF.xlsx",sep = "\\"))
          }
        }
        dlgMessage("Multicollinearity check has been successfully completed")
      }
    }

    ################################-----Plot generation-----################################

    cls<-character()
    for(i in 1:ncol(dt)){
      int_cls<-class(dt[[i]])
      if(length(int_cls) > 1){
        cls[i]<-paste(int_cls,collapse = " ")
      }
      else {
        cls[i]<-int_cls
      }
    }
    dt_num<-dt[,which(cls %in% c("numeric","integer")),with = F]
    dt_cat<-dt[,which(cls %in% c("numeric","integer") == F),with = F]
    plot_choice<-vector()
    if(ncol(dt_cat) > 0){
      plot_choice<-c(plot_choice,"Frequency plots")
    }
    if(ncol(dt_num) > 0){
      plot_choice<-c(plot_choice,"Correlation plot","Box and Whisker plot (outlier analysis)",
                     "Distribution description plots (Cullen and Frey graph)",
                     "Fitted distribution plots","All")
    }
    if(length(target) > 0 && cls[target] %in% c("numeric","integer")){
      plot_choice<-c("Target vs Predictor plots",plot_choice)
      target_num<-which(colnames(dt_num) == colnames(dt)[target])
    }
    else if(length(target) > 0 && cls[target] %in% c("numeric","integer") == F){
      dlgMessage(message = c("The target variable is categorical",
                             "Target vs Predictor plots will not be generated"),type = "ok")
    }
    else if(length(target) <= 0){
      dlgMessage(message = c("No target variable has been provided",
                             "Target vs Predictor plots will not be generated"),type = "ok")
    }

    gen_plots<-dlgList(choices = plot_choice,preselect = "All",multiple = T,title = "Choose plots to generate")$res
    if(length(gen_plots) > 0){
      dlgMessage(message = "Generating plots. Please wait as this might take a while",type = "ok")
    }

    if("Target vs Predictor plots" %in% gen_plots | "All" %in% gen_plots){
      if(length(target) > 0 && cls[target] %in% c("numeric","integer")){
        feats<-1:ncol(dt_num)
        feats<-feats[-target_num]
        colnm<-gsub(pattern = "\\%",replacement = "pct",x = colnames(dt_num))
        err_tar_pred<-err_tar_pred_var<-vector()
        for(i in feats){
          tryCatch({
            fl_nm<-paste(sv_path,"\\Target_vs_Predictor(",colnm[i],").jpeg",sep = "")
            jpeg(filename = fl_nm,quality = 100)
            sv_plot<-ggplot(data = dt_num) + geom_point(mapping = aes(x = dt_num[[i]],y = dt_num[[target_num]])) +
              geom_line(mapping = aes(x = dt_num[[i]],y = dt_num[[target_num]],colour = "red")) +
              xlab(label = colnames(dt_num)[i]) + ylab(label = colnames(dt_num)[target_num]) +
              ggtitle(label = paste(colnames(dt_num)[i],"vs",colnames(dt_num)[target_num],sep = " ")) +
              theme(legend.position = "none")
            print(sv_plot)
            dev.off()
          },error = function(e){
            write(x = paste(paste(Sys.time(),"Target vs Predictor plot could not be generated for the variable :",
                                  colnames(dt_num)[i],sep = " "),paste("ERROR:",e$message,sep = " "),sep = "\n"),
                  file = paste(sv_path,"\\Auto_EDA-Error_log.txt",sep = ""),append = TRUE)
            dev_off<-dev.off()
            file.remove(fl_nm)
          })
        }

        dlgMessage(message = "Target vs Predictor plot(s) have been successfully generated and saved in the chosen directory",
                   type = "ok")
      }
    }

    if("Correlation plot" %in% gen_plots | "All" %in% gen_plots){
      cor_meth<-dlgList(choices = c("Pearson's correlation coefficient","Kendall's correlation coefficient",
                                    "Spearman's correlation coefficient"),multiple = F,
                        preselect = "Pearson's correlation coefficient",title = "Choose correlation method")$res
      if(cor_meth == "Pearson's correlation coefficient"){
        cor_meth<-"pearson"
      }
      else if(cor_meth == "Kendall's correlation coefficient"){
        cor_meth<-"kendall"
      }
      else if(cor_meth == "Spearman's correlation coefficient"){
        cor_meth<-"spearman"
      }
      tryCatch({
        corr<-cor(x = dt_num,method = cor_meth)
        jpeg(filename = paste(sv_path,"\\Correlation_Plot.jpeg",sep = ""),quality = 100,width = 1080,height = 1080)
        corrplot(title = "Correlation Plot",corr = corr)
        dev.off()
        dlgMessage(message = "Correlation plot has been successfully generated and saved in the chosen directory",
                   type = "ok")
      },error = function(e){
        write(x = paste(paste(Sys.time(),"Correlation plot could not be generated",sep = " "),
                        paste("ERROR:",e$message,sep = " "),sep = "\n"),
              file = paste(sv_path,"\\Auto_EDA-Error_log.txt",sep = ""),append = TRUE)
        dev_off<-dev.off()
        file.remove(paste(sv_path,"\\Correlation_Plot.jpeg",sep = ""))
      })
    }

    if("Box and Whisker plot (outlier analysis)" %in% gen_plots | "All" %in% gen_plots){
      tryCatch({
        box_col<-seq(from = 1,to = ncol(dt_num),by = 1)
        while(length(box_col) >= 5){
          plot_nm<-paste(sv_path,"\\Boxplot(",box_col[1],"-",box_col[5],").jpeg",sep = "")
          jpeg(filename = plot_nm,quality = 100,height = 1080,width = 1080)
          boxplot(x = dt_num[,box_col[1:5],with = F])
          title(main = "Box and Whisker Plot")
          dev.off()
          box_col<-box_col[-c(1:5)]
        }
        if(length(box_col) > 0){
          jpeg(filename = paste(sv_path,"\\Boxplot(",min(box_col),"-",max(box_col),").jpeg",sep = "")
               ,quality = 100,width = 1080,height = 1080)
          boxplot(x = dt_num[,box_col,with = F])
          title(main = "Box and Whisker Plot")
          dev.off()
        }
        dlgMessage(message = "Box and Whisker plot(s) have been successfully generated and saved in the chosen directory",
                   type = "ok")
      },error = function(e){
        write(x = paste(paste(Sys.time(),"One or more Boxplots could not be generated",sep = " "),
                        paste("ERROR:",e$message," "),sep = "\n"),file = paste(sv_path,"\\Auto_EDA-Error_log.txt",sep = ""),
              append = TRUE)
        dev_off<-dev.off()
        file.remove(paste(sv_path,"\\Boxplot(",min(box_col)))
      })
    }

    if("Distribution description plots (Cullen and Frey graph)" %in% gen_plots | "All" %in% gen_plots){
      colnm<-gsub(pattern = "\\%",replacement = "pct",x = colnames(dt_num))
      err_desc<-err_desc_var<-vector()
      for(i in 1:ncol(dt_num)){
        tryCatch({
          fl_nm<-paste(sv_path,"\\Distribution description(",colnm[i],").jpeg",sep = "")
          jpeg(filename = fl_nm,quality = 100)
          desc<-descdist(data = dt_num[[i]],graph = T)
          dev.off()
        },error = function(e){
          write(x = paste(paste(Sys.time(),"Distribution description plot could not be generated for the variable :",
                                colnames(dt_num)[i],sep = " "),paste("ERROR:",e$message,sep = " "),sep = "\n"),
                file = paste(sv_path,"\\Auto_EDA-Error_log.txt",sep = ""),append = TRUE)
          dev_off<-dev.off()
          file.remove(fl_nm)
        })
      }

      dlgMessage(message = "Distribution description plots have been successfully generated and saved in the chosen directory",
                 type = "ok")
    }

    if("Fitted distribution plots" %in% gen_plots | "All" %in% gen_plots){
      fits<-dlgList(choices = c("Normal                                                                                    ",
                                "Log-normal","Uniform","Exponential","Beta","Gamma","Poisson","Geometric","Weibull"),
                    multiple = T,preselect = "Normal",title = "Choose the distribution(s) the variables should be fitted to")$res
      fits_plot<-vector()
      if("Normal                                                                                    " %in% fits){
        fits_plot<-c(fits_plot,"norm")
      }
      if("Log-normal" %in% fits){
        fits_plot<-c(fits_plot,"lnorm")
      }
      if("Uniform" %in% fits){
        fits_plot<-c(fits_plot,"unif")
      }
      if("Exponential" %in% fits){
        fits_plot<-c(fits_plot,"exp")
      }
      if("Beta" %in% fits){
        fits_plot<-c(fits_plot,"beta")
      }
      if("Gamma" %in% fits){
        fits_plot<-c(fits_plot,"gamma")
      }
      if("Poisson" %in% fits){
        fits_plot<-c(fits_plot,"pois")
      }
      if("Geometric" %in% fits){
        fits_plot<-c(fits_plot,"geom")
      }
      if("Weibull" %in% fits){
        fits_plot<-c(fits_plot,"weibull")
      }
      colnm<-gsub(pattern = "\\%",replacement = "pct",x = colnames(dt_num))
      err_fit<-err_fit_var<-vector()
      sink(file = paste(sv_path,"\\Auto_EDA-Error_log.txt",sep = ""),append = T)
      for(i in 1:ncol(dt_num)){
        for(j in 1:length(fits_plot)){
          tryCatch({
            fl_nm<-paste(sv_path,"\\Fit_",fits_plot[j],"(",colnm[i],").jpeg",sep = "")
            jpeg(filename = fl_nm,quality = 100)
            plot(fitdist(data = dt_num[[i]],distr = fits_plot[j]))
            dev.off()
          },error = function(e){
            write(x = paste(paste(Sys.time(),"Fitted distribution (",fits_plot[j],") could not be generated for the variable :",
                                  colnames(dt_num)[i],sep = " "),paste("ERROR:",e$message,sep = " "),sep = "\n"),
                  file = paste(sv_path,"\\Auto_EDA-Error_log.txt",sep = ""),append = TRUE)
            dev_off<-dev.off()
            file.remove(fl_nm)
          })
        }
      }
      sink()

      dlgMessage(message = "Fitted distribution plots have been successfully generated and saved in the chosen directory",
                 type = "ok")
    }

    if("Frequency plots" %in% gen_plots | "All" %in% gen_plots & ncol(dt_cat) > 0){
      colnm<-gsub(pattern = "\\%",replacement = "pct",x = colnames(dt_cat))
      err_hist<-err_hist_var<-vector()
      for(i in 1:ncol(dt_cat)){
        tryCatch({
          fl_nm<-paste(sv_path,"\\Frequency plot(",colnm[i],").jpeg",sep = "")
          jpeg(filename = fl_nm,quality = 100)
          freq_plot<-ggplot(data = dt_cat) + geom_bar(mapping = aes(dt_cat[[i]]),fill = "blue") +
            ggtitle(label = "Frequency Plot",subtitle = colnames(dt_cat)[i]) + xlab(label = colnames(dt_cat)[i]) +
            theme(legend.position = "none")
          print(freq_plot)
          dev.off()
        },error = function(e){
          invisible(e)
          write(x = paste(paste(Sys.time(),"Frequency plot could not be generated for the variable :",colnames(dt_cat)[i],sep = " "),
                          paste("ERROR:",e$message,sep = " "),sep = "\n"),file = paste(sv_path,"\\Auto_EDA-Error_log.txt",sep = ""),
                append = TRUE)
          dev_off<-dev.off()
          file.remove(fl_nm)
        })
      }

      dlgMessage(message = "Frequency plots have been successfully generated and saved in the chosen directory",
                 type = "ok")
    }

    dlgMessage(message = c("EDA has been completed & plots/reports/error logs have been generated and saved",
                           "Please check the following path to view them :",sv_path),type = "ok")
    read<-readLines(con = paste(sv_path,"\\Auto_EDA-Error_log.txt",sep = ""),n = 5)
    if(length(read) <= 0){
      invisible(file.remove(paste(sv_path,"\\Auto_EDA-Error_log.txt",sep = "")))
    }
  }
  else {
    dlgMessage(message = "You can use the Pre_Proc function in AutoAnalyse for data pre-processing",type = "ok")
  })
}
