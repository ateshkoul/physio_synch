MBS_load_ECG_EDA_intra_data <- function(){
  
  data_dir <- "Physio/"
  data <-  read.csv(paste0(data_dir,"intra.csv"))
  

  data["Vision"] <- ifelse(grepl("NoOcc",data$Condition),"NoOcc","Occ")
 
  
  data$mean_ECG_rate[abs(scale(data$mean_ECG_rate))>2.5] <- NA
  data$mean_EDA_Tonic[abs(scale(data$mean_EDA_Tonic))>2.5] <- NA
  data$mean_RSP_Rate[abs(scale(data$mean_RSP_Rate))>2.5] <- NA
  # 
  
  data_out <- ddply(data,.(Dyd_no,Vision),summarise,
                    bpm_avg = mean(mean_ECG_rate,na.rm=TRUE),
                    EDA_avg = mean(mean_EDA_Tonic,na.rm=TRUE),
                    rsp_avg = mean(mean_RSP_Rate,na.rm=TRUE))##,
  
  
  data_out["exp"] <- "MBS"

  return(data_out)
}




MBS_load_ECG_EDA_Resp_pupil_corr_data <- function(data_type,data_process='ori'){
  
  
  root_MBS_dir <- "Physio/"
  

  physio_MBS_data <- read.csv(paste0(root_MBS_dir,'corr_data_MBS.csv'))
  # from matlab
  Dyad_no <- c(203,204,205,206,207,208,209,211,212,213,214,216,217,218,219)
  
  dat_ECG <- physio_MBS_data[,grep("ECG",names(physio_MBS_data))]
  dat_EDA <- physio_MBS_data[,grep("EDA",names(physio_MBS_data))]
  dat_Resp <- physio_MBS_data[,grep("Resp",names(physio_MBS_data))]
  dat_Pupil <- physio_MBS_data[,grep("pupil",names(physio_MBS_data))]
  
  
  
  dat_ECG_long_avg <- reshape(dat_ECG,direction = "long",
                          varying = list(1:2),
                          times=names(dat_ECG)[1:2],
                          v.names = "corr_coef",
                          timevar = "condition",ids=Dyad_no)
  
  dat_ECG_long_avg["Vision"] <- ifelse(grepl("NoOcc",dat_ECG_long_avg$condition),"NoOcc","Occ")

  dat_ECG_long_avg["exp"] <- "MBS"
  dat_ECG_long_avg["Modality"] <- "ECG"
  
  
  dat_EDA_long_avg <- reshape(dat_EDA,direction = "long",
                          varying = list(1:2),
                          times=names(dat_EDA)[1:2],
                          v.names = "corr_coef",timevar = "condition",ids=Dyad_no)
  
  dat_EDA_long_avg["Vision"] <- ifelse(grepl("NoOcc",dat_EDA_long_avg$condition),"NoOcc","Occ")
  dat_EDA_long_avg["exp"] <- "MBS"
  dat_EDA_long_avg["Modality"] <- "EDA"
  
  
  
  dat_Resp_long_avg <- reshape(dat_Resp,direction = "long",
                           varying = list(1:2),
                           times=names(dat_Resp)[1:2],
                           v.names = "corr_coef",timevar = "condition",ids=Dyad_no)
  
  dat_Resp_long_avg["Vision"] <- ifelse(grepl("NoOcc",dat_Resp_long_avg$condition),"NoOcc","Occ")
  dat_Resp_long_avg["exp"] <- "MBS" 
  dat_Resp_long_avg["Modality"] <- "Resp"
  
  dat_Pupil_long_avg <- reshape(dat_Pupil,direction = "long",
                               varying = list(1:2),
                               times=names(dat_Resp)[1:2],
                               v.names = "corr_coef",timevar = "condition",ids=Dyad_no)
  
  dat_Pupil_long_avg["Vision"] <- ifelse(grepl("NoOcc",dat_Pupil_long_avg$condition),"NoOcc","Occ")
  dat_Pupil_long_avg["exp"] <- "MBS" 
  dat_Pupil_long_avg["Modality"] <- "Pupil"
  
  
  switch(data_type,
         ECG=dat_ECG_long_avg[,c("corr_coef","id","Vision","exp","Modality")],
         EDA=dat_EDA_long_avg[,c("corr_coef","id","Vision","exp","Modality")],
         Resp=dat_Resp_long_avg[,c("corr_coef","id","Vision","exp","Modality")],
         Pupil=dat_Pupil_long_avg[,c("corr_coef","id","Vision","exp","Modality")])
  
}

MBS_new_load_surrogate_ECG_EDA_corr_data <- function(data_type){
  
  
  root_MBS_dir <- "Physio/"
  

  physio_MBS_data <- read.csv(paste0(root_MBS_dir,'MBS_ori_pseudo.csv'))
  # from matlab
  Dyad_no <- c(203,204,205,206,207,208,209,211,212,213,214,216,217,218,219)
  
  
  
  physio_MBS_data <- physio_MBS_data[,!(grepl("ori_pseudo",names(physio_MBS_data)))] 
  
  dat_ECG <- physio_MBS_data[,grep("ECG",names(physio_MBS_data))]
  dat_EDA <- physio_MBS_data[,grep("EDA_Tonic",names(physio_MBS_data))]
  dat_Resp <- physio_MBS_data[,grep("Resp",names(physio_MBS_data))]
  
  dat_ECG <- dat_ECG[,grep("NoOcc",names(dat_ECG))]
  dat_EDA <- dat_EDA[,grep("NoOcc",names(dat_EDA))]
  dat_Resp <- dat_Resp[,grep("NoOcc",names(dat_Resp))]


  
  dat_ECG_long <- reshape(dat_ECG,direction = "long",
                          varying = list(1:6),
                          times=names(dat_ECG)[1:6],
                          v.names = "corr_coef",
                          timevar = "condition",ids=Dyad_no)
  
  dat_ECG_long["Vision"] <- ifelse(grepl("pseudo",dat_ECG_long$condition),"pseudo","ori")

  
  dat_ECG_long_avg <- ddply(dat_ECG_long,.(id,Vision),summarize,
                            corr_coef= mean(corr_coef,na.rm=TRUE))
  dat_ECG_long_avg["exp"] <- "MBS"
  dat_ECG_long_avg["Modality"] <- "ECG"
  

  
  dat_EDA_long <- reshape(dat_EDA,direction = "long",
                          varying = list(1:6),
                          times=names(dat_EDA)[1:6],
                          v.names = "corr_coef",timevar = "condition",ids=Dyad_no)
  
  dat_EDA_long["Vision"] <- ifelse(grepl("pseudo",dat_EDA_long$condition),"pseudo","ori")
  
  
  dat_EDA_long_avg <- ddply(dat_EDA_long,.(id,Vision),summarize,
                            corr_coef= mean(corr_coef,na.rm=TRUE))
  
  dat_EDA_long_avg["exp"] <- "MBS"
  dat_EDA_long_avg["Modality"] <- "EDA"
  
  
  
  dat_Resp_long <- reshape(dat_Resp,direction = "long",
                           varying = list(1:6),
                           times=names(dat_Resp)[1:6],
                           v.names = "corr_coef",timevar = "condition",ids=Dyad_no)
  
  dat_Resp_long["Vision"] <- ifelse(grepl("pseudo",dat_Resp_long$condition),"pseudo","ori")

  
  
  dat_Resp_long_avg <- ddply(dat_Resp_long,.(id,Vision),summarize,
                             corr_coef= mean(corr_coef,na.rm=TRUE))
  
  dat_Resp_long_avg["exp"] <- "MBS" 
  
  dat_Resp_long_avg["Modality"] <- "Resp"
  
  switch(data_type,
         ECG=dat_ECG_long_avg[,c("corr_coef","id","Vision","exp","Modality")],
         EDA=dat_EDA_long_avg[,c("corr_coef","id","Vision","exp","Modality")],
         Resp=dat_Resp_long_avg[,c("corr_coef","id","Vision","exp","Modality")])
  
  
}



MBS_load_pupil_intra_data <- function(){
  
  data_dir <- "Physio/"


  data <-  read.csv(paste0(data_dir,"MBS_pupil_corr.csv"))
  
  
  dia_cols <- grep("dia",names(data))
  corr_cols <- grep("pupil_corr",names(data))
  
  
  Dyad_no <- c(203,204,205,206,207,208,209,211,212,213,214,216,217,218,219);
  
  
  data_dia <- reshape(data,direction = "long",
                      varying = dia_cols,
                      idvar = "Dyd_no",
                      v.names="pupil_dia",
                      timevar = "Condition",
                      times=names(data)[dia_cols],
                      drop = corr_cols,ids=Dyad_no)
  
  data_dia["Vision"] <- ifelse(grepl("NoOcc",data_dia$Condition),"NoOcc","Occ")
  
  split <- function(x) strsplit(x,"_pupil")[[1]][1]
  data_dia["cond_name"] <- ldply(data_dia$Condition,split)
  
  split2 <- function(x) strsplit(strsplit(x,"_pupil_")[[1]][2],"_0_dia")[[1]][1]
  data_dia["Sub_no"] <- ldply(data_dia$Condition,split2)  

  
  data_dia$pupil_dia[abs(scale(data_dia$pupil_dia))>2.5] <- NA
  
  
  data_out <- ddply(data_dia,.(Dyd_no,Vision),summarise,
                    pupil_dia = mean(pupil_dia,na.rm=TRUE))##,
  
  data_out["exp"] <- "MBS"
  
  
  return(data_out)
}


MBS_load_pupil_corr_data <- function(){
  
  MBS_root_result_dir <- "Physio/"
  
   MBS_pupil_data <- read.csv(paste0(MBS_root_result_dir,"MBS_pupil_corr.csv"))
  
  
  Dyad_no <- c(203,204,205,206,207,208,209,211,212,213,214,216,217,218,219);
  
  
  dat_MBS_pupil <- MBS_pupil_data[,grep("pupil_corr",names(MBS_pupil_data))]
  
  dat_MBS_pupil_long <- reshape(dat_MBS_pupil,direction = "long",
                                varying = list(1:6),times=names(dat_MBS_pupil)[1:6],
                                v.names = "corr_coef",timevar = "condition",ids=Dyad_no)
  
  
  
  dat_MBS_pupil_long["Vision"] <- ifelse(grepl("NoOcc",dat_MBS_pupil_long$condition),"NoOcc","Occ")
  


  dat_MBS_pupil_long_avg <- ddply(dat_MBS_pupil_long,.(id,Vision),summarise,corr_coef= mean(corr_coef,na.rm=TRUE))
  

  dat_MBS_pupil_long_avg["exp"] <- "MBS"
  dat_MBS_pupil_long_avg["Modality"] <- "Pupil"
  
  return(dat_MBS_pupil_long_avg[,c("corr_coef","id","Vision","exp","Modality")])
  
 
  
}



MBS_new_load_surrogate_pupil_corr_data <- function(){
  
  MBS_root_result_dir <- "Physio/"

  
  
  MBS_pupil_data <- read.csv(paste0(MBS_root_result_dir,"MBS_pupil_corr_surrogate.csv"))

  Dyad_no <- c(203,204,205,206,207,208,209,211,212,213,214,216,217,218,219);
  
  

  dat_MBS_pupil <- MBS_pupil_data[,!(grepl("ori_pseudo",names(MBS_pupil_data)))] 
  dat_MBS_pupil <- dat_MBS_pupil[,grep("NoOcc",names(dat_MBS_pupil))]
  
  
  dat_MBS_pupil_long <- reshape(dat_MBS_pupil,direction = "long",
                                varying = list(1:6),times=names(dat_MBS_pupil)[1:6],
                                v.names = "corr_coef",timevar = "condition",ids=Dyad_no)
  
  
  
  dat_MBS_pupil_long["Vision"] <- ifelse(grepl("pseudo",dat_MBS_pupil_long$condition),"pseudo","ori")
  
  
 
  
  dat_MBS_pupil_long_avg <- ddply(dat_MBS_pupil_long,.(id,Vision),summarise,corr_coef= mean(corr_coef,na.rm=TRUE))
  

  dat_MBS_pupil_long_avg["exp"] <- "MBS"
  dat_MBS_pupil_long_avg["Modality"] <- "Pupil"
  
  return(dat_MBS_pupil_long_avg[,c("corr_coef","id","Vision","exp","Modality")])
  
  
}


MBS_load_surrogate_ECG_EDA_corr_data <- function(data_type){
  
  
  root_MBS_dir <- "Physio/"
  

  physio_MBS_data <- read.csv(paste0(root_MBS_dir,'MBS_ori_pseudo_pupil_corr.csv'))
  # from matlab
  Dyad_no <- c(203,204,205,206,207,208,209,211,212,213,214,216,217,218,219)
  
  dat_ECG <- physio_MBS_data[,grep("ECG",names(physio_MBS_data))]
  dat_EDA <- physio_MBS_data[,grep("EDA",names(physio_MBS_data))]
  dat_Resp <- physio_MBS_data[,grep("Resp",names(physio_MBS_data))]
  
  
  
  dat_ECG_long <- reshape(dat_ECG,direction = "long",
                          varying = list(1:2),
                          times=names(dat_ECG)[1:2],
                          v.names = "corr_coef",
                          timevar = "condition",ids=Dyad_no)
  
  dat_ECG_long["Vision"] <- ifelse(grepl("NoOcc",dat_ECG_long$condition),"NoOcc","Occ")

  
  dat_ECG_long_avg <- ddply(dat_ECG_long,.(id,Vision),summarize,
                            corr_coef= mean(corr_coef,na.rm=TRUE))
  dat_ECG_long_avg["exp"] <- "MBS"
  dat_ECG_long_avg["Modality"] <- "ECG"
  

  
  dat_EDA_long <- reshape(dat_EDA,direction = "long",
                          varying = list(1:2),
                          times=names(dat_EDA)[1:2],
                          v.names = "corr_coef",timevar = "condition",ids=Dyad_no)
  
  dat_EDA_long["Vision"] <- ifelse(grepl("NoOcc",dat_EDA_long$condition),"NoOcc","Occ")
  
  
  dat_EDA_long_avg <- ddply(dat_EDA_long,.(id,Vision),summarize,
                            corr_coef= mean(corr_coef,na.rm=TRUE))
  
  dat_EDA_long_avg["exp"] <- "MBS"
  dat_EDA_long_avg["Modality"] <- "EDA"
  
  
  
  dat_Resp_long <- reshape(dat_Resp,direction = "long",
                           varying = list(1:2),
                           times=names(dat_Resp)[1:2],
                           v.names = "corr_coef",timevar = "condition",ids=Dyad_no)
  
  dat_Resp_long["Vision"] <- ifelse(grepl("NoOcc",dat_Resp_long$condition),"NoOcc","Occ")

  
  
  dat_Resp_long_avg <- ddply(dat_Resp_long,.(id,Vision),summarize,
                             corr_coef= mean(corr_coef,na.rm=TRUE))
  
  dat_Resp_long_avg["exp"] <- "MBS" 
  
  dat_Resp_long_avg["Modality"] <- "Resp"
  
  switch(data_type,
         ECG=dat_ECG_long_avg[,c("corr_coef","id","Vision","exp","Modality")],
         EDA=dat_EDA_long_avg[,c("corr_coef","id","Vision","exp","Modality")],
         Resp=dat_Resp_long_avg[,c("corr_coef","id","Vision","exp","Modality")])
  
  
}

MBS_load_surrogate_pupil_corr_data <- function(){
  
  MBS_root_result_dir <- "Physio/"
  

  MBS_pupil_data <- read.csv(paste0(MBS_root_result_dir,"MBS_ori_pseudo_pupil_corr.csv"))
  
  Dyad_no <- c(203,204,205,206,207,208,209,211,212,213,214,216,217,218,219);
  
  
  dat_MBS_pupil <- MBS_pupil_data[,grep("pupil",names(MBS_pupil_data))]
  
  dat_MBS_pupil_long_avg <- reshape(dat_MBS_pupil,direction = "long",
                                varying = list(1:2),times=names(dat_MBS_pupil)[1:2],
                                v.names = "corr_coef",timevar = "condition",ids=Dyad_no)
  
  
  
  dat_MBS_pupil_long_avg["Vision"] <- ifelse(grepl("NoOcc",dat_MBS_pupil_long_avg$condition),"NoOcc","Occ")
  

  
  dat_MBS_pupil_long_avg["exp"] <- "MBS"
  dat_MBS_pupil_long_avg["Modality"] <- "Pupil"
  
  return(dat_MBS_pupil_long_avg[,c("corr_coef","id","Vision","exp","Modality")])
  
  
  
}

MBS_new_load_surrogate_ECG_EDA_Resp_pupil_corr_data <- function(data_type){
  
  
  MBS_root_result_dir <- "Physio/"
  

  MBS_physio_pseudo_data <- read.csv(paste0(MBS_root_result_dir,"MBS_pseudo_pupil_corr.csv"))
  
  MBS_physio_ori_data <- read.csv(paste0(MBS_root_result_dir,"MBS_ori_pupil_corr.csv"))
  
   
  MBS_physio_data <- cbind(MBS_physio_ori_data,MBS_physio_pseudo_data)
  
  dat_ECG <- MBS_physio_data[,grep("ECG",names(MBS_physio_data))]
  dat_EDA <- MBS_physio_data[,grep("EDA",names(MBS_physio_data))]
  dat_Resp <- MBS_physio_data[,grep("Resp",names(MBS_physio_data))]
  dat_pupil <- MBS_physio_data[,grep("pupil",names(MBS_physio_data))]
  
  

  
  
  Dyad_no <- c(203,204,205,206,207,208,209,211,212,213,214,216,217,218,219)
  
  dat_ECG_long <- reshape(dat_ECG,direction = "long",varying = list(c(1:4)),
                          times = names(dat_ECG)[c(1:4)],v.names = "corr_coef",
                          timevar = "condition",ids = Dyad_no)
  
  dat_ECG_long["surrogate"] <-ifelse(grepl("pseudo",dat_ECG_long$condition),"pseudo","ori")
  dat_ECG_long["Vision"] <-ifelse(grepl("NoOcc",dat_ECG_long$condition),"NoOcc","Occ")
  
  dat_ECG_long["exp"] <- "MBS"
  dat_ECG_long["Modality"] <- "ECG"
  
  
  dat_EDA_long <- reshape(dat_EDA,direction = "long",varying = list(c(1:4)),
                          times = names(dat_EDA)[c(1:4)],v.names = "corr_coef",
                          timevar = "condition",ids = Dyad_no)
  

  dat_EDA_long["surrogate"] <-ifelse(grepl("pseudo",dat_EDA_long$condition),"pseudo","ori")
  dat_EDA_long["Vision"] <-ifelse(grepl("NoOcc",dat_EDA_long$condition),"NoOcc","Occ")
  
  dat_EDA_long["exp"] <- "MBS"
  dat_EDA_long["Modality"] <- "EDA"
  
  
  dat_Resp_long <- reshape(dat_Resp,direction = "long",varying = list(c(1:4)),
                           times = names(dat_Resp)[c(1:4)],v.names = "corr_coef",
                           timevar = "condition",ids = Dyad_no)
  
 
  
  dat_Resp_long["surrogate"] <-ifelse(grepl("pseudo",dat_Resp_long$condition),"pseudo","ori")
  dat_Resp_long["Vision"] <-ifelse(grepl("NoOcc",dat_Resp_long$condition),"NoOcc","Occ")
  
  dat_Resp_long["exp"] <- "MBS"
  dat_Resp_long["Modality"] <- "Resp"
  
  
  dat_pupil_long <- reshape(dat_pupil,direction = "long",varying = list(c(1:4)),
                            times = names(dat_pupil)[c(1:4)],v.names = "corr_coef",
                            timevar = "condition",ids = Dyad_no)

  
  dat_pupil_long["surrogate"] <-ifelse(grepl("pseudo",dat_pupil_long$condition),"pseudo","ori")
  dat_pupil_long["Vision"] <-ifelse(grepl("NoOcc",dat_pupil_long$condition),"NoOcc","Occ")
  
  dat_pupil_long["exp"] <- "MBS"
  dat_pupil_long["Modality"] <- "Pupil"
  
  
  switch(data_type,
         ECG=dat_ECG_long[,c("corr_coef","id","Vision","exp","Modality","surrogate")],
         EDA=dat_EDA_long[,c("corr_coef","id","Vision","exp","Modality","surrogate")],
         Resp=dat_Resp_long[,c("corr_coef","id","Vision","exp","Modality","surrogate")],
         Pupil=dat_pupil_long[,c("corr_coef","id","Vision","exp","Modality","surrogate")])
  
}



##################### end functions

