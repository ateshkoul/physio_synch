############################################### NoOcc Occ study functions #################################################

IBS_load_pupil_intra_data <- function(){
  data_dir <- "Physio\\"
  
  data <-  read.csv(paste0(data_dir,"pupil.csv"))
  
  data <- data[!is.nan(data$nanmean_diameter_3d),]
  data <- data[!data$nanmean_diameter_3d>100,]
  
  data[,"Vision"] <- data$condition
  
  data$Vision[grepl("NoOcc",data$condition)] <- "NoOcc"
  data$Vision[!grepl("NoOcc",data$condition)] <- "Occ"
  
  
  data$nanmean_diameter_3d[abs(scale(data$nanmean_diameter_3d))>2.5] <- NA
  
  split1 <- function(x) as.double(strsplit(x,"Dyad_")[[1]][2])
  data["Dyd_no"] <- ldply(data$Dyad_no_Eye_tracker_pupil_no_correction,split1)

  data_summary <- ddply(data,.(Dyd_no,Vision),summarize,
                        avg_diameter= mean(nanmean_diameter_3d,na.rm=TRUE),
                        se_diameter = sd(nanmean_diameter_3d,na.rm=TRUE)/length(nanmean_diameter_3d))
  
  
  names(data_summary)[1:3] <- c("Dyd_no","Vision","pupil_dia")
  data_summary <- data_summary[,c("Dyd_no","Vision","pupil_dia")]
  data_summary["exp"] <- "INS"
  
  
  return(data_summary)

}


INS_load_pupil_corr_data <- function(){
  root_dir <- "Physio\\"

  INS_pupil <- read.csv(paste0(root_dir,"pupil_corr.csv"))
  
  
  dat_Pupil <- INS_pupil[,grep("Pupil",names(INS_pupil))]
  
  dat_Pupil_long <- reshape(dat_Pupil,direction = "long",varying = list(c(1,2)),
                            times = names(dat_Pupil)[c(1,2)],v.names = "corr_coef",
                            timevar = "condition")
  
  dat_Pupil_long["Vision"] <- ifelse(dat_Pupil_long$condition %in% "Pupil_NoOcc","NoOcc","Occ")
  dat_Pupil_long["exp"] <- "INS"
  dat_Pupil_long["Modality"] <- "Pupil"
  
  return(dat_Pupil_long[,c("corr_coef","id","Vision","exp","Modality")])
  
}

IBS_load_ECG_EDA_Resp_intra_data <- function(){

  data_dir <- "Physio\\"
  
  data <-  read.csv(paste0(data_dir,"results_intra.csv"))
  

  data[,"dist"] <- substr(data$Condition,1,2)
  data[,"Vision"] <- substr(data$Condition,3,7)

  
  data$mean_ECG_rate[abs(scale(data$mean_ECG_rate))>2.5] <- NA
  data$mean_EDA_Tonic[abs(scale(data$mean_EDA_Tonic))>2.5] <- NA
  data$mean_RSP_Rate[abs(scale(data$mean_RSP_Rate))>2.5] <- NA

  
  
  data <- ddply(data,.(Dyd_no,Vision),summarize,
                bpm_avg=mean(mean_ECG_rate,na.rm=TRUE),
                EDA_avg=mean(mean_EDA_Tonic,na.rm=TRUE),
                rsp_avg = mean(mean_RSP_Rate,na.rm=TRUE))
  
  data["exp"] <- "INS"

  return(data)
}


INS_load_ECG_EDA_Resp_corr_data <- function(data_type){
  
  
  INS_root_dir <- "Physio\\"

  INS_physio_data <- read.csv(paste0(INS_root_dir,"corr_data.csv"))
  
  dat_ECG <- INS_physio_data[,grep("ECG",names(INS_physio_data))]
  dat_EDA <- INS_physio_data[,grep("EDA",names(INS_physio_data))]
  dat_Resp <- INS_physio_data[,grep("Resp",names(INS_physio_data))]
  

  dat_ECG_long <- reshape(dat_ECG,direction = "long",varying = list(c(1,2)),
                          times = names(dat_ECG)[c(1,2)],v.names = "corr_coef",
                          timevar = "condition")
  
  dat_ECG_long["Vision"] <- ifelse(dat_ECG_long$condition %in% "ECG_NoOcc","NoOcc","Occ")
  dat_ECG_long["exp"] <- "INS"
  dat_ECG_long["Modality"] <- "ECG"
  
  
  dat_EDA_long <- reshape(dat_EDA,direction = "long",varying = list(c(1,2)),
                          times = names(dat_EDA)[c(1,2)],v.names = "corr_coef",
                          timevar = "condition")
  
  dat_EDA_long["Vision"] <- ifelse(dat_EDA_long$condition %in% "EDA_NoOcc","NoOcc","Occ")
  
  
  dat_EDA_long["exp"] <- "INS"
  dat_EDA_long["Modality"] <- "EDA"
  
  
  dat_Resp_long <- reshape(dat_Resp,direction = "long",varying = list(c(1,2)),
                           times = names(dat_Resp)[c(1,2)],v.names = "corr_coef",
                           timevar = "condition")
  
  dat_Resp_long["Vision"] <- ifelse(dat_Resp_long$condition %in% "Resp_NoOcc","NoOcc","Occ")
  
  dat_Resp_long["exp"] <- "INS"
  
  
  dat_Resp_long["Modality"] <- "Resp"
  
  
  
  switch(data_type,
         ECG=dat_ECG_long[,c("corr_coef","id","Vision","exp","Modality")],
         EDA=dat_EDA_long[,c("corr_coef","id","Vision","exp","Modality")],
         Resp=dat_Resp_long[,c("corr_coef","id","Vision","exp","Modality")])
  
}
INS_load_surrogate_ECG_EDA_Resp_pupil_corr_data <- function(data_type){
  
  
  INS_root_dir <- "Physio\\"
  
  INS_physio_data <- read.csv(paste0(INS_root_dir,"corr_data_surrogate.csv"))
  
  
  dat_ECG <- INS_physio_data[,grep("ECG",names(INS_physio_data))]
  dat_EDA <- INS_physio_data[,grep("EDA",names(INS_physio_data))]
  dat_Resp <- INS_physio_data[,grep("Resp",names(INS_physio_data))]
  dat_pupil <- INS_physio_data[,grep("Pupil",names(INS_physio_data))]
  

  
  dat_ECG_long <- reshape(dat_ECG,direction = "long",varying = list(c(1,2)),
                          times = names(dat_ECG)[c(1,2)],v.names = "corr_coef",
                          timevar = "condition")
  
  dat_ECG_long["Vision"] <- ifelse(dat_ECG_long$condition %in% "ori_pseudo_ECG_NoOcc","NoOcc","Occ")
  dat_ECG_long["exp"] <- "INS"
  dat_ECG_long["Modality"] <- "ECG"
  
  
  dat_EDA_long <- reshape(dat_EDA,direction = "long",varying = list(c(1,2)),
                          times = names(dat_EDA)[c(1,2)],v.names = "corr_coef",
                          timevar = "condition")
  
  dat_EDA_long["Vision"] <- ifelse(dat_EDA_long$condition %in% "ori_pseudo_EDA_NoOcc","NoOcc","Occ")
  
  
  dat_EDA_long["exp"] <- "INS"
  dat_EDA_long["Modality"] <- "EDA"
  
  
  dat_Resp_long <- reshape(dat_Resp,direction = "long",varying = list(c(1,2)),
                           times = names(dat_Resp)[c(1,2)],v.names = "corr_coef",
                           timevar = "condition")
  
  dat_Resp_long["Vision"] <- ifelse(dat_Resp_long$condition %in% "ori_pseudo_Resp_NoOcc","NoOcc","Occ")
  dat_Resp_long["exp"] <- "INS"
  dat_Resp_long["Modality"] <- "Resp"
  
  
  dat_pupil_long <- reshape(dat_pupil,direction = "long",varying = list(c(1,2)),
                            times = names(dat_pupil)[c(1,2)],v.names = "corr_coef",
                            timevar = "condition")
  
  dat_pupil_long["Vision"] <- ifelse(dat_pupil_long$condition %in% "ori_pseudo_Pupil_NoOcc","NoOcc","Occ")
  dat_pupil_long["exp"] <- "INS"
  dat_pupil_long["Modality"] <- "Pupil"
  
  
  switch(data_type,
         ECG=dat_ECG_long[,c("corr_coef","id","Vision","exp","Modality")],
         EDA=dat_EDA_long[,c("corr_coef","id","Vision","exp","Modality")],
         Resp=dat_Resp_long[,c("corr_coef","id","Vision","exp","Modality")],
         Pupil=dat_pupil_long[,c("corr_coef","id","Vision","exp","Modality")])
  
}
INS_new_load_surrogate_ECG_EDA_Resp_pupil_corr_data <- function(data_type){
  
  
  INS_root_dir <- "Physio\\"
  
  
  INS_physio_pseudo_data <- read.csv(paste0(INS_root_dir,"pseudo_data.csv"))
  
  INS_physio_ori_data <- read.csv(paste0(INS_root_dir,"ori_data.csv"))
  
  
  
  
  INS_physio_data <- cbind(INS_physio_ori_data,INS_physio_pseudo_data)
  
  dat_ECG <- INS_physio_data[,grep("ECG",names(INS_physio_data))]
  dat_EDA <- INS_physio_data[,grep("EDA",names(INS_physio_data))]
  dat_Resp <- INS_physio_data[,grep("Resp",names(INS_physio_data))]
  dat_pupil <- INS_physio_data[,grep("Pupil",names(INS_physio_data))]
  

  
  dat_ECG_long <- reshape(dat_ECG,direction = "long",varying = list(c(1:4)),
                          times = names(dat_ECG)[c(1:4)],v.names = "corr_coef",
                          timevar = "condition")
  
  dat_ECG_long["surrogate"] <-ifelse(grepl("pseudo",dat_ECG_long$condition),"pseudo","ori")
  dat_ECG_long["Vision"] <-ifelse(grepl("NoOcc",dat_ECG_long$condition),"NoOcc","Occ")
  

  dat_ECG_long["exp"] <- "INS"
  dat_ECG_long["Modality"] <- "ECG"
  
  
  dat_EDA_long <- reshape(dat_EDA,direction = "long",varying = list(c(1:4)),
                          times = names(dat_EDA)[c(1:4)],v.names = "corr_coef",
                          timevar = "condition")
  
  
  dat_EDA_long["surrogate"] <-ifelse(grepl("pseudo",dat_EDA_long$condition),"pseudo","ori")
  dat_EDA_long["Vision"] <-ifelse(grepl("NoOcc",dat_EDA_long$condition),"NoOcc","Occ")
  
  
  dat_EDA_long["exp"] <- "INS"
  dat_EDA_long["Modality"] <- "EDA"
  
  
  dat_Resp_long <- reshape(dat_Resp,direction = "long",varying = list(c(1:4)),
                           times = names(dat_Resp)[c(1:4)],v.names = "corr_coef",
                           timevar = "condition")
  

  dat_Resp_long["surrogate"] <-ifelse(grepl("pseudo",dat_Resp_long$condition),"pseudo","ori")
  dat_Resp_long["Vision"] <-ifelse(grepl("NoOcc",dat_Resp_long$condition),"NoOcc","Occ")
  
  
  dat_Resp_long["exp"] <- "INS"
  dat_Resp_long["Modality"] <- "Resp"
  
  
  dat_pupil_long <- reshape(dat_pupil,direction = "long",varying = list(c(1:4)),
                          times = names(dat_pupil)[c(1:4)],v.names = "corr_coef",
                          timevar = "condition")
  
  dat_pupil_long["surrogate"] <-ifelse(grepl("pseudo",dat_pupil_long$condition),"pseudo","ori")
  dat_pupil_long["Vision"] <-ifelse(grepl("NoOcc",dat_pupil_long$condition),"NoOcc","Occ")
  
  dat_pupil_long["exp"] <- "INS"
  dat_pupil_long["Modality"] <- "Pupil"
  
  
  switch(data_type,
         ECG=dat_ECG_long[,c("corr_coef","id","Vision","exp","Modality","surrogate")],
         EDA=dat_EDA_long[,c("corr_coef","id","Vision","exp","Modality","surrogate")],
         Resp=dat_Resp_long[,c("corr_coef","id","Vision","exp","Modality","surrogate")],
         Pupil=dat_pupil_long[,c("corr_coef","id","Vision","exp","Modality","surrogate")])
  
}
############################################### end functions #################################################


