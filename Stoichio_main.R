rm(list=ls())

source("Stoichio_functions.R")

#******************************************************************************#

# ----------------------Step 1: Simulations for main text ------------------

#******************************************************************************#
## >> 1) Mapping ecosystem production in the rP and rB space for delta_X = 1 ----



n_point=50;p_coup=1
param_space=expand.grid(rP=seq(1/10,1/40,length.out=n_point),rB=seq(0.12,0.25,length.out=n_point))

#tibble for all the scenarios explored
scenario_space=tibble(Name_scena=c("C-limited",'N-limited',"Colimitation"),type_ode=c(rep("full",3)),
                      param_name=c("C-limited","N-limited","Colimitation"),N_time=rep(10000,3),
                      colim=c(F,F,T))

for (rowspace in 1:nrow(scenario_space)){ 
  
  d=d2=d3=d_recy=tibble()
  colim=scenario_space$colim[rowspace]
  
  
  
  for (nr in 1:nrow(param_space)){
    
    #defining parameters
    param=Get_classical_param(scena = scenario_space$param_name[rowspace],coupling = F)
    param$rP=param_space$rP[nr];param$rB=param_space$rB[nr]
    
    #getting initial states
    state=Get_initial_values(param)
    
    #Running the model
    data_save=Compute_ode(state,param,n_time = scenario_space$N_time[rowspace],type_ode = scenario_space$type_ode[rowspace])
    Eq=Extract_equilibrium_from_dynamics(data_save,param) #Equilibrium
    limit=Get_limitation(Eq$Eq,param)
    
    #productivity with recycling
    P1_recy=Primary_productivity(Eq$Eq,param,colim)
    P2_recy=Secondary_productivity(Eq$Eq,param)
    
    #production with recycling
    PP1_recy=Primary_production(Eq$Eq,param,colim)
    PP2_recy=Secondary_production(Eq$Eq,param)
    
    #aggregating
    d_recy=rbind(d_recy,Eq$Eq%>%add_column(PP1_T=PP1_recy$Terrestrial,PP1_A=PP1_recy$Aquatic,P1_T=P1_recy$Terrestrial,P1_A=P1_recy$Aquatic,
                                           PP2_T=PP2_recy$Terrestrial,PP2_A=PP2_recy$Aquatic,P2_T=P2_recy$Terrestrial,P2_A=P2_recy$Aquatic,
                                           rP=param$rP,rB=param$rB,Phi=param$pB))
    
    #running the model with spatial flows
    param[c("pH",'pC',"pP","pB")]=p_coup #we set delta_X=1 (what I also called phi)
    
    data_save=Compute_ode(state,param,n_time = scenario_space$N_time[rowspace],type_ode = scenario_space$type_ode[rowspace])
    Eq=Extract_equilibrium_from_dynamics(data_save,param) #Equilibrium
    limit=Get_limitation(Eq$Eq,param)
    
    
    # Getting ecosystem productions and meta-ecosystem functioning metrics
    
    P1=Primary_productivity(Eq$Eq,param,colim);P2=Secondary_productivity(Eq$Eq,param) #productivity
    PP1=Primary_production(Eq$Eq,param,colim);PP2=Secondary_production(Eq$Eq,param) #production
    Metric_metaecosyst=LRR_meta_ecosystem(P1,P2,P1_recy,P2_recy,PP1,PP2,PP1_recy,PP2_recy)
    
    
    #net flow of nitrogen and carbon
    net_flow=Net_flow_nitrogen(Eq$Eq,param)
    
    
    #merging dataframes
    d2=rbind(d2,Eq$Eq %>% add_column(Phi=p_coup,Ratio=Get_limitation(Eq$Eq,param)$Ratio,
                                     P1_T=P1$Terrestrial,P1_A=P1$Aquatic,P2_T=P2$Terrestrial,P2_A=P2$Aquatic,P1_A_tot=P1$Aqua_tot,
                                     PP1_T=PP1$Terrestrial,PP1_A=PP1$Aquatic,PP2_T=PP2$Terrestrial,PP2_A=PP2$Aquatic,PP1_A_tot=PP1$Aqua_tot,
                                     

                                     #LRR productivity
                                     LRR_P1_T=log(P1$Terrestrial/P1_recy$Terrestrial),
                                     LRR_P1_A=log(P1$Aquatic/P1_recy$Aquatic),
                                     LRR_P2_T=log(P2$Terrestrial/P2_recy$Terrestrial),
                                     LRR_P2_A=log(P2$Aquatic/P2_recy$Aquatic),
                                     
                                     #LRR production
                                     LRR_PP1_T=log(PP1$Terrestrial/PP1_recy$Terrestrial),
                                     LRR_PP1_A=log(PP1$Aquatic/PP1_recy$Aquatic),
                                     LRR_PP2_T=log(PP2$Terrestrial/PP2_recy$Terrestrial),
                                     LRR_PP2_A=log(PP2$Aquatic/PP2_recy$Aquatic),
                                     
                                     #Limitation of the system
                                     Limitation=limit$Limitation,
                                     
                                     #Metrics at the meta-ecosystem scale
                                     
                                     ## productivity
                                     P_log_sum=Metric_metaecosyst$Metric_log_sum_P,
                                     P_log_sum_1=Metric_metaecosyst$Metric_log_sum_P_1,
                                     P_log_sum_2=Metric_metaecosyst$Metric_log_sum_P_2,
                                     
                                     ## production
                                     PP_log_sum=Metric_metaecosyst$Metric_log_sum_PP,
                                     PP_log_sum_1=Metric_metaecosyst$Metric_log_sum_PP_1,
                                     PP_log_sum_2=Metric_metaecosyst$Metric_log_sum_PP_2,
                                     
                                     #netflow of carbon and nitrogen (expressed for aquatic ecosystem), meaning that a negative value corresponds to a 
                                     #net emission of nitrogen while a positive one means that aquatic ecosystem receives N
                                     
                                     N_receive_A=net_flow$N_receive_A,
                                     C_receive_A=net_flow$C_receive_A,
                                     Flow_C_T_to_A=net_flow$flow_C_T_to_A,
                                     Flow_C_A_to_T=net_flow$flow_C_A_to_T,
                                     Flow_N_T_to_A=net_flow$flow_N_T_to_A,
                                     Flow_N_A_to_T=net_flow$flow_N_A_to_T
    ))
    
    if (nr%%n_point==0) print(nr)
    
  }
  write.table(d2,paste0("./Table/Space_rB_rP_",scenario_space$Name_scena[rowspace],"_phi_",p_coup,".csv"),sep=";")
  write.table(d_recy,paste0("./Table/Space_rB_rP_",scenario_space$Name_scena[rowspace],"_phi_",p_coup,"_recy.csv"),sep=";")
  
}#end scenario limitation







## >> 2) Meta-ecosystem scale metrics ----

n_point=30;p_coup=1;  
param_space=expand.grid(rP=c(1/10,1/40),rB=c(0.12,0.25));p_seq=seq(0,1,length.out=n_point)

for (s in c("C-limited",'N-limited')){ 
  d=d2=d3=tibble()

  for (nr in 1:nrow(param_space)){
      for (p in p_seq){
        
        param=Get_classical_param(scena = s,coupling = F)
        param$rP=param_space$rP[nr];param$rB=param_space$rB[nr]
        param[c("pH",'pC',"pP","pB")]=p
        state=Get_initial_values(param)
        
        
        data_save=Compute_ode(state,param,n_time = 10000)
        Eq=Extract_equilibrium_from_dynamics(data_save,param) #Equilibrium
        
        limit_ratio=Get_limitation(Eq$Eq,param)
        
        PP1_bidirectional=Primary_production(Eq$Eq,param);        P1_bidirectional=Primary_productivity(Eq$Eq,param)
        PP2_bidirectional=Secondary_production(Eq$Eq,param);        P2_bidirectional=Secondary_productivity(Eq$Eq,param)
        
        
        param[c("pH",'pC',"pP","pB")]=0
        state=Get_initial_values(param)
        
        
        data_save=Compute_ode(state,param,n_time = 10000)
        Eq=Extract_equilibrium_from_dynamics(data_save,param) #Equilibrium
        
        PP1_recy=Primary_production(Eq$Eq,param);        P1_recy=Primary_productivity(Eq$Eq,param)
        PP2_recy=Secondary_production(Eq$Eq,param);        P2_recy=Secondary_productivity(Eq$Eq,param)
        
        
        Metric_metaecosyst=LRR_meta_ecosystem(P1_bidirectional,P2_bidirectional,P1_recy,P2_recy,PP1_bidirectional,PP2_bidirectional,PP1_recy,PP2_recy)
        

        d2=rbind(d2,Eq$Eq %>% add_column(Phi=p, Scenario=s,
                                         Basal=Metric_metaecosyst$Metric_log_sum_PP_1,Secondary=Metric_metaecosyst$Metric_log_sum_PP_2,
                                         Ratio=limit_ratio$Ratio,Limitation=limit_ratio$Limitation,
                                         ))

    }
  }
  write.table(d2,paste0("./Table/Meta_ecosystem_",s,".csv"),sep=";")
  
}









#******************************************************************************#

## >> 3) Simulations on feedback----


n_point=100;p_coup=1;  
param_space=expand.grid(rP=c(1/10,1/40),rB=c(0.12,0.25));p_seq=seq(0,1,length.out=n_point)
Extract_equilibrium_from_dynamics=function(data,param,consumers=F){
  
  n_begin=ifelse(consumers,19,15)
  
  #here we do the mean on the last 30000 values to have very precise values.
  #for the feedback measure it is important to have smooth lines
  data_mean=as_tibble(t(colMeans(data[(nrow(data)-30000):nrow(data),-1])))
  data_with_param=cbind(data_mean,matrix(unlist(param),ncol = length(param),nrow=1))
  colnames(data_with_param)[n_begin:ncol(data_with_param)]=names(param)
  
  return(list(Eq=data_with_param))
  
}
for (s in c("C-limited",'N-limited',"Colimitation")){ 
  d=d2=d3=tibble()
  
  for (nr in 1:nrow(param_space)){
    for (p in p_seq){
      
      param=Get_classical_param(scena = s,coupling = F)
      param$rP=param_space$rP[nr];param$rB=param_space$rB[nr]
      param[c("pH",'pC',"pP","pB")]=p
      state=Get_initial_values(param)
      
      
      data_save=Compute_ode(state,param,n_time = 60000)
      Eq=Extract_equilibrium_from_dynamics(data_save,param) #Equilibrium
      
      limit_ratio=Get_limitation(Eq$Eq,param)
      
      #EXtract ecosystems functioning metrics
      P1_bidirectional=Primary_production(Eq$Eq,param)
      P2_bidirectional=Secondary_production(Eq$Eq,param)
      
      
      #Computing the feedbacks on each ecosystem see the function for details
      feedback=Compute_feedbacks(Eq$Eq,param,type_prod = "Production",n_time=60000)
      
      
      #Aggregating the dataframes
      d2=rbind(d2,Eq$Eq %>% add_column(Phi=p, Feedback_T_1=feedback$Net_T_1,Feedback_A_1=feedback$Net_A_1,
                                       Feedback_T_2=feedback$Net_T_2,Feedback_A_2=feedback$Net_A_2,Scenario=s,
                                       P1_bidirec_T=P1_bidirectional$Terrestrial,P1_bidirec_A=P1_bidirectional$Aquatic,
                                       P2_bidirec_T=P2_bidirectional$Terrestrial,P2_bidirec_A=P2_bidirectional$Aquatic,
                                       P1_uni_T=feedback$P2_no_T_to_A,P1_uni_A=feedback$P1_no_A_to_T,P2_uni_T=feedback$P2_no_T_to_A,
                                       P2_uni_A=feedback$P2_no_A_to_T,Ratio=limit_ratio$Ratio,Limitation=limit_ratio$Limitation))
      d=rbind(d,feedback$Eq_uni_A%>%add_column(type_feedback="uni_A",Ratio=Get_limitation(feedback$Eq_uni_A,param)$Ratio,Limitation=Get_limitation(feedback$Eq_uni_A,param)$Limitation),
              feedback$Eq_uni_T%>%add_column(type_feedback="uni_T",Ratio=Get_limitation(feedback$Eq_uni_T,param)$Ratio,Limitation=Get_limitation(feedback$Eq_uni_T,param)$Limitation))
      
    }
    
  }
  write.table(d2,paste0("./Table/Feedback_",s,".csv"),sep=";")
  write.table(d ,paste0("./Table/Feedback_all_eq_",s,".csv"),sep=";")
  
}




# --------------------- Step 2: Simulation for SI ------------

#******************************************************************************#

## >> 4) Mechanisms N-limitation ----

# effect of rB
phi_seq=c(1);scena=c("no delta_C","all");d=tibble()
param_space=expand.grid(rB=seq(.12,.25,length.out=10),rP=c(.025))
for (s in scena){
  param=Get_classical_param(scena = "N-limited")
  for (nr in 1:nrow(param_space)){
    param$rB=param_space$rB[nr];param$rP=param_space$rP[nr]
    for (p in phi_seq){
      if (s=="all") { param[c("pH","pP","pC","pB")]=p
      } else {param[c("pH","pP","pB")]=p;param[c("pC")]=0}
      
      state=Get_initial_values(param)
      data=Compute_ode(state,param,n_time = 10000,type_ode = "full")
      Eq=Extract_equilibrium_from_dynamics(data,param)$Eq
      
      PII=Secondary_production(Eq,param)
      PI=Primary_production(Eq,param)
      net=Net_flow_nitrogen(Eq,param)
      d=rbind(d,Eq %>% add_column(Phi=p,scena=s,PI_A=PI$Aquatic,PI_T=PI$Terrestrial,
                                  PII_A=PII$Aquatic,PII_T=PII$Terrestrial,
                                  Net_N_A_to_T=-net$N_receive_A,Net_C_A_to_T=-net$C_receive_A, # negative because we are not getting the net flow to aquatic ecosystems
                                  C_T_to_A=net$flow_C_T_to_A,C_A_to_T=net$flow_C_A_to_T,
                                  N_T_to_A=net$flow_N_T_to_A,N_A_to_T=net$flow_N_A_to_T,
                                  C_T_to_T=net$flow_C_T_to_T,C_A_to_A=net$flow_C_A_to_A,
                                  N_T_to_T=net$flow_N_T_to_T,N_A_to_A=net$flow_N_A_to_A ))
    }
  }
}
write.table(d,"./Table/Mecanism_consumers_rB.csv",sep=";")



# effect of rP

phi_seq=c(1);scena=c("no delta_H","all");d=tibble()
param_space=expand.grid(rP=seq(.025,.1,length.out=10),rB=c(.25))
for (s in scena){
  param=Get_classical_param(scena = "N-limited")
  for (nr in 1:nrow(param_space)){
    param$rB=param_space$rB[nr];param$rP=param_space$rP[nr]
    for (p in phi_seq){
      if (s=="all") { param[c("pH","pP","pC","pB")]=p
      } else {param[c("pC","pP","pB")]=p;param[c("pH")]=0}
      
      state=Get_initial_values(param)
      data=Compute_ode(state,param,n_time = 10000,type_ode = "full")
      Eq=Extract_equilibrium_from_dynamics(data,param)$Eq
      
      PII=Secondary_production(Eq,param)
      PI=Primary_production(Eq,param)
      
      net=Net_flow_nitrogen(Eq,param)
      d=rbind(d,Eq %>% add_column(Phi=p,scena=s,PI_A=PI$Aquatic,PI_T=PI$Terrestrial,
                                  PII_A=PII$Aquatic,PII_T=PII$Terrestrial,
                                  Net_N_A_to_T=net$N_receive_A,Net_C_A_to_T=net$C_receive_A,
                                  C_T_to_A=net$flow_C_T_to_A,C_A_to_T=net$flow_C_A_to_T,
                                  N_T_to_A=net$flow_N_T_to_A,N_A_to_T=net$flow_N_A_to_T,
                                  C_T_to_T=net$flow_C_T_to_T,C_A_to_A=net$flow_C_A_to_A,
                                  N_T_to_T=net$flow_N_T_to_T,N_A_to_A=net$flow_N_A_to_A ))
    }
    
  }
}
write.table(d,"./Table/Mecanism_herbivores_rP.csv",sep=";")





## >> 5) Mechanisms C-limitation ----

#effect of rB
phi_seq=c(1);scena=c("no delta_C","all");d=tibble()
param_space=expand.grid(rB=seq(.12,.25,length.out=10),rP=c(.025))
for (s in scena){
  param=Get_classical_param(scena = "C-limited")
  for (nr in 1:nrow(param_space)){
    param$rB=param_space$rB[nr];param$rP=param_space$rP[nr]
    for (p in phi_seq){
      for (rep in 1:1){
        if (s=="all") { param[c("pH","pP","pC","pB")]=p
        } else {param[c("pH","pP","pB")]=p;param[c("pC")]=0}
        
        state=Get_initial_values(param)
        data=Compute_ode(state,param,n_time = 10000,type_ode = "full")
        Eq=Extract_equilibrium_from_dynamics(data,param)$Eq
        
        PII=Secondary_production(Eq,param)
        PI=Primary_production(Eq,param)
        net=Net_flow_nitrogen(Eq,param)
        d=rbind(d,Eq %>% add_column(Phi=p,scena=s,PI_A=PI$Aquatic,PI_T=PI$Terrestrial,
                                    PII_A=PII$Aquatic,PII_T=PII$Terrestrial,
                                    Net_N_A_to_T=-net$N_receive_A,Net_C_A_to_T=-net$C_receive_A,
                                    C_T_to_A=net$flow_C_T_to_A,C_A_to_T=net$flow_C_A_to_T,
                                    N_T_to_A=net$flow_N_T_to_A,N_A_to_T=net$flow_N_A_to_T,
                                    C_T_to_T=net$flow_C_T_to_T,C_A_to_A=net$flow_C_A_to_A,
                                    N_T_to_T=net$flow_N_T_to_T,N_A_to_A=net$flow_N_A_to_A ))
      }
    }
  }
}
write.table(d,"./Table/Mecanism_consumers_rB_C-limited.csv",sep=";")

#effect of rP

phi_seq=c(1);scena=c("no delta_H","all");d=tibble()
param_space=expand.grid(rP=seq(.025,.1,length.out=10),rB=c(.25))
for (s in scena){
  param=Get_classical_param(scena = "C-limited")
  for (nr in 1:nrow(param_space)){
    param$rB=param_space$rB[nr];param$rP=param_space$rP[nr]
    for (p in phi_seq){
      
      if (s=="all") { param[c("pH","pP","pC","pB")]=p
      } else {param[c("pC","pP","pB")]=p;param[c("pH")]=0}
      
      state=Get_initial_values(param)
      data=Compute_ode(state,param,n_time = 10000,type_ode = "full")
      Eq=Extract_equilibrium_from_dynamics(data,param)$Eq
      
      PII=Secondary_production(Eq,param)
      PI=Primary_production(Eq,param)
      
      net=Net_flow_nitrogen(Eq,param)
      d=rbind(d,Eq %>% add_column(Phi=p,scena=s,PI_A=PI$Aquatic,PI_T=PI$Terrestrial,
                                  PII_A=PII$Aquatic,PII_T=PII$Terrestrial,
                                  Net_N_A_to_T=net$N_receive_A,Net_C_A_to_T=net$C_receive_A,
                                  C_T_to_A=net$flow_C_T_to_A,C_A_to_T=net$flow_C_A_to_T,
                                  N_T_to_A=net$flow_N_T_to_A,N_A_to_T=net$flow_N_A_to_T,
                                  C_T_to_T=net$flow_C_T_to_T,C_A_to_A=net$flow_C_A_to_A,
                                  N_T_to_T=net$flow_N_T_to_T,N_A_to_A=net$flow_N_A_to_A ))
    }
    
  }
}
write.table(d,"./Table/Mecanism_herbivores_rP_C-limited.csv",sep=";")




## 6) Sensitivity on the food-webs structure: adding a top predator ----
### >> Stoichiometry space----
n_point=20;p_coup=1
param_space=expand.grid(rP=seq(1/10,1/40,length.out=n_point),rB=seq(0.12,0.25,length.out=n_point))
scenario_space=tibble(Name_scena=c("C-limited",'N-limited'),type_ode=c(rep("topconsum",2)),
                      param_name=c("C-limited","N-limited"),N_time=rep(10000,2),
                      colim=c(F,F))

for (rowspace in 1:nrow(scenario_space)){ 
  
  d=d2=d3=d_recy=tibble()
  colim=scenario_space$colim[rowspace]
  
  
  
  for (nr in 1:nrow(param_space)){
    
    
    param=Get_classical_param(scena = scenario_space$param_name[rowspace],coupling = F);param=Topconsum_param(param)
    param$rP=param_space$rP[nr];param$rB=param_space$rB[nr]
    
    if (scenario_space$param_name[rowspace]=="C-limited"){
      param$aTC=.1;param$eTC=1;param$dTC=.05;param$dTH=.15;param$aP=.1;param$aH=.4;param$dP=.1;param$aTH=.5;param$aC=.5
    }else{
      param$aTC=.1;param$eTC=1;param$dTC=.05;param$dTH=.15;param$aP=.1;param$aH=.4;param$dP=.1;param$aTH=.5;param$aC=.5
      param$INa=5;param$lNa=2;param$IDa=12;param$aBN=.1;param$lDa=1
    }
    
    state=Get_initial_values(param);state=State_topconsum(state,param)
    
    data_save=Compute_ode(state,param,n_time = scenario_space$N_time[rowspace],type_ode = scenario_space$type_ode[rowspace])
    Eq=Extract_equilibrium_from_dynamics(data_save,param,consumers = T) #Equilibrium
    limit=Get_limitation(Eq$Eq,param)
    
    #productivity with recycling
    P1_recy=Primary_productivity(Eq$Eq,param,colim,consumers = T)
    P2_recy=Secondary_productivity(Eq$Eq,param,consumers = T)
    P3_recy=Secondary_productivity_topconsum(Eq$Eq,param)
    
    #production with recycling
    PP1_recy=Primary_production(Eq$Eq,param,colim,consumers = T)
    PP2_recy=Secondary_production(Eq$Eq,param,consumers = T)
    PP3_recy=Secondary_production_topconsum(Eq$Eq,param)
    
    d_recy=rbind(d_recy,Eq$Eq %>% add_column(PP1_T=PP1_recy$Terrestrial,PP1_A=PP1_recy$Aquatic,P1_T=P1_recy$Terrestrial,P1_A=P1_recy$Aquatic,
                                             PP2_T=PP2_recy$Terrestrial,PP2_A=PP2_recy$Aquatic,P2_T=P2_recy$Terrestrial,P2_A=P2_recy$Aquatic,
                                             rP=param$rP,rB=param$rB,Phi=param$pB))
    
    param[c("pH",'pC',"pP","pB","pTC","pTH")]=p_coup #we set delta_X=1 (what I also called phi)
    
    data_save=Compute_ode(state,param,n_time = scenario_space$N_time[rowspace],type_ode = scenario_space$type_ode[rowspace])
    Eq=Extract_equilibrium_from_dynamics(data_save,param,consumers = T) #Equilibrium
    limit=Get_limitation(Eq$Eq,param)
    
    #productivity coupling
    P1=Primary_productivity(Eq$Eq,param,colim,consumers = T)
    P2=Secondary_productivity(Eq$Eq,param,consumers = T)
    P3=Secondary_productivity_topconsum(Eq$Eq,param)
    
    #production coupling
    PP1=Primary_production(Eq$Eq,param,colim,consumers = T)
    PP2=Secondary_production(Eq$Eq,param,consumers = T)
    PP3=Secondary_production_topconsum(Eq$Eq,param)
    
    #dataframe addition
    
    d2=rbind(d2,Eq$Eq %>% add_column(Phi=p_coup,Ratio=Get_limitation(Eq$Eq,param)$Ratio,
                                     P1_T=P1$Terrestrial,P1_A=P1$Aquatic,P2_T=P2$Terrestrial,P2_A=P2$Aquatic,P1_A_tot=P1$Aqua_tot,
                                     PP1_T=PP1$Terrestrial,PP1_A=PP1$Aquatic,PP2_T=PP2$Terrestrial,PP2_A=PP2$Aquatic,PP1_A_tot=PP1$Aqua_tot,
                                     
                                     
                                     #LRR productivity
                                     LRR_P1_T=log(P1$Terrestrial/P1_recy$Terrestrial),
                                     LRR_P1_A=log(P1$Aquatic/P1_recy$Aquatic),
                                     LRR_P2_T=log(P2$Terrestrial/P2_recy$Terrestrial),
                                     LRR_P2_A=log(P2$Aquatic/P2_recy$Aquatic),
                                     LRR_P3_T=log(P3$Terrestrial/P3_recy$Terrestrial),
                                     LRR_P3_A=log(P3$Aquatic/P3_recy$Aquatic),
                                     
                                     #LRR production
                                     LRR_PP1_T=log(PP1$Terrestrial/PP1_recy$Terrestrial),
                                     LRR_PP1_A=log(PP1$Aquatic/PP1_recy$Aquatic),
                                     LRR_PP2_T=log(PP2$Terrestrial/PP2_recy$Terrestrial),
                                     LRR_PP2_A=log(PP2$Aquatic/PP2_recy$Aquatic),
                                     LRR_PP3_T=log(PP3$Terrestrial/PP3_recy$Terrestrial),
                                     LRR_PP3_A=log(PP3$Aquatic/PP3_recy$Aquatic),
                                     
                                     #Limitation of the system
                                     Limitation=limit$Limitation
                                     
    ))
    
    if (nr%%n_point==0) print(nr)
    
  }
  write.table(d2,paste0("./Table/Space_rB_rP_",scenario_space$Name_scena[rowspace],"_phi_Top_pred_",p_coup,".csv"),sep=";")
  write.table(d_recy,paste0("./Table/Space_rB_rP_",scenario_space$Name_scena[rowspace],"_phi_Top_pred_",p_coup,"_recy.csv"),sep=";")
  
  
  
}#end scenario limitation




### >> Feedback ----

n_point=50;p_coup=1;  
param_space=expand.grid(rP=c(1/10,1/40),rB=c(.12,0.25));p_seq=seq(0,1,length.out=n_point)

for (s in c('C-limited',"N-limited")){ 
  d=d2=d3=tibble()
  N_rep=ifelse(s!="N-limited",1,1)
  
  for (nr in 1:nrow(param_space)){
    
    for (p in p_seq){
      
      param=Get_classical_param(scena = s,coupling = F);param=Topconsum_param(param)
      param$rP=param_space$rP[nr];param$rB=param_space$rB[nr]
      param[c("pH",'pC',"pP","pB","pTC","pTH")]=p
      state=Get_initial_values(param);state=State_topconsum(state,param)
      
      
      if (s=="C-limited"){
        param$aTC=.1;param$eTC=1;param$dTC=.05;param$dTH=.15;param$aP=.1;param$aH=.4;param$dP=.1;param$aTH=.5;param$aC=.5
      }else{
        param$aTC=.1;param$eTC=1;param$dTC=.05;param$dTH=.15;param$aP=.1;param$aH=.4;param$dP=.1;param$aTH=.5;param$aC=.5
        param$INa=5;param$lNa=2;param$IDa=12;param$aBN=.1;param$lDa=1
      }
      
      
      data_save=Compute_ode(state,param,n_time = 20000,type_ode = "topconsum")
      Eq=Extract_equilibrium_from_dynamics(data_save,param,consumers = T) #Equilibrium
      
      limit_ratio=Get_limitation(Eq$Eq,param)
      
      P1_bidirectional=Primary_production(Eq$Eq,param)
      P2_bidirectional=Secondary_production(Eq$Eq,param)
      P3_bidirectional=Secondary_production_topconsum(Eq$Eq,param)
      
      feedback=Compute_feedbacks(Eq$Eq,param,type_prod = "Production",n_time=20000,DC = F,plot = F,top_consumers = T)
      
      
      d2=rbind(d2,Eq$Eq %>% add_column(Phi=p, Feedback_T_1=feedback$Net_T_1,Feedback_A_1=feedback$Net_A_1,
                                       Feedback_T_2=feedback$Net_T_2,Feedback_A_2=feedback$Net_A_2,
                                       Feedback_T_3=feedback$Net_T_3,Feedback_A_3=feedback$Net_A_3,
                                       Scenario=s,
                                       P1_bidirec_T=P1_bidirectional$Terrestrial,P1_bidirec_A=P1_bidirectional$Aquatic,
                                       P2_bidirec_T=P2_bidirectional$Terrestrial,P2_bidirec_A=P2_bidirectional$Aquatic,
                                       P1_uni_T=feedback$P1_no_T_to_A,P1_uni_A=feedback$P1_no_A_to_T,
                                       P2_uni_T=feedback$P2_no_T_to_A,
                                       P2_uni_A=feedback$P2_no_A_to_T,
                                       Ratio=limit_ratio$Ratio,Limitation=limit_ratio$Limitation))
      d=rbind(d,feedback$Eq_uni_A%>%add_column(type_feedback="uni_A",Ratio=Get_limitation(feedback$Eq_uni_A,param)$Ratio,Limitation=Get_limitation(feedback$Eq_uni_A,param)$Limitation),
              feedback$Eq_uni_T%>%add_column(type_feedback="uni_T",Ratio=Get_limitation(feedback$Eq_uni_T,param)$Ratio,Limitation=Get_limitation(feedback$Eq_uni_T,param)$Limitation))
      
    }
    
  }
  write.table(d2,paste0("./Table/Feedback_Top_pred_",s,".csv"),sep=";")
  write.table(d ,paste0("./Table/Feedback_Top_pred_all_eq_",s,".csv"),sep=";")
  
}

## 7) Donnor-control scenario ----
### >> Stoichiometry space ----
n_point=20;p_coup=1
param_space=expand.grid(rP=seq(1/10,1/40,length.out=n_point),rB=seq(0.12,0.25,length.out=n_point))
scenario_space=tibble(Name_scena=c("C-limited",'N-limited'),type_ode=c(rep("DC",2)),
                      param_name=c("C-limited",'N-limited'),N_time=c(rep(10000,2)),
                      colim=c(F,F))

for (rowspace in 1:nrow(scenario_space)){ 
  
  d=d2=d3=d_recy=tibble()
  colim=scenario_space$colim[rowspace]
  
  for (nr in 1:nrow(param_space)){
    
    param=Get_classical_param(scena = scenario_space$param_name[rowspace],coupling = F)
    param$rP=param_space$rP[nr];param$rB=param_space$rB[nr]
    
    if (scenario_space$Name_scena[rowspace]=="N-limited"){
      param$lNa=2
    }
    
    
    state=Get_initial_values(param)
    
    data_save=Compute_ode(state,param,n_time = scenario_space$N_time[rowspace],type_ode = scenario_space$type_ode[rowspace])
    Eq=Extract_equilibrium_from_dynamics(data_save,param) #Equilibrium
    limit=Get_limitation(Eq$Eq,param) # if the system is N limited when isolated, it will necessary be N -limited when coupled to the terrestrial ecosystem
    
    #productivity with recycling
    P1_recy=Primary_productivity(Eq$Eq,param,colim)
    P2_recy=Secondary_productivity(Eq$Eq,param)
    
    #production with recycling
    PP1_recy=Primary_production(Eq$Eq,param,colim,DC = T)
    PP2_recy=Secondary_production(Eq$Eq,param,DC = T)
    
    d_recy=rbind(d_recy,Eq$Eq%>% add_column(PP1_T=PP1_recy$Terrestrial,PP1_A=PP1_recy$Aquatic,P1_T=P1_recy$Terrestrial,P1_A=P1_recy$Aquatic,
                                            PP2_T=PP2_recy$Terrestrial,PP2_A=PP2_recy$Aquatic,P2_T=P2_recy$Terrestrial,P2_A=P2_recy$Aquatic,
                                            rP=param$rP,rB=param$rB,Phi=param$pB,Limitation=limit$Limitation))
    
    param[c("pH",'pC',"pP","pB")]=p_coup #we set delta_X=1 (what I also called phi)
    
    data_save=Compute_ode(state,param,n_time = scenario_space$N_time[rowspace],type_ode = scenario_space$type_ode[rowspace])
    Eq=Extract_equilibrium_from_dynamics(data_save,param) #Equilibrium
    limit=Get_limitation(Eq$Eq,param)
    
    P1=Primary_productivity(Eq$Eq,param,colim);P2=Secondary_productivity(Eq$Eq,param) #productivity
    PP1=Primary_production(Eq$Eq,param,colim,DC = T);PP2=Secondary_production(Eq$Eq,param,DC = T) #production
    
    Metric_metaecosyst=LRR_meta_ecosystem(P1,P2,P1_recy,P2_recy,PP1,PP2,PP1_recy,PP2_recy)
    
    
    #net flow of nitrogen and carbon
    net_flow=Net_flow_nitrogen(Eq$Eq,param)
    
    state=State_at_equilibrium(Eq$Eq)
    
    #dataframe addition
    d2=rbind(d2,Eq$Eq %>% add_column(Phi=p_coup,Ratio=Get_limitation(Eq$Eq,param)$Ratio,
                                     P1_T=P1$Terrestrial,P1_A=P1$Aquatic,P2_T=P2$Terrestrial,P2_A=P2$Aquatic,P1_A_tot=P1$Aqua_tot,
                                     PP1_T=PP1$Terrestrial,PP1_A=PP1$Aquatic,PP2_T=PP2$Terrestrial,PP2_A=PP2$Aquatic,PP1_A_tot=PP1$Aqua_tot,
                                     
                                     #LRR productivity
                                     LRR_P1_T=log(P1$Terrestrial/P1_recy$Terrestrial),
                                     LRR_P1_A=log(P1$Aquatic/P1_recy$Aquatic),
                                     LRR_P2_T=log(P2$Terrestrial/P2_recy$Terrestrial),
                                     LRR_P2_A=log(P2$Aquatic/P2_recy$Aquatic),
                                     
                                     #LRR production
                                     LRR_PP1_T=log(PP1$Terrestrial/PP1_recy$Terrestrial),
                                     LRR_PP1_A=log(PP1$Aquatic/PP1_recy$Aquatic),
                                     LRR_PP2_T=log(PP2$Terrestrial/PP2_recy$Terrestrial),
                                     LRR_PP2_A=log(PP2$Aquatic/PP2_recy$Aquatic),
                                     
                                     #Limitation of the system
                                     Limitation=limit$Limitation,
                                     
                                     state=state,
                                     
                                     #netflow
                                     N_receive_A=net_flow$N_receive_A,
                                     C_receive_A=net_flow$C_receive_A,
                                     Flow_C_T_to_A=net_flow$flow_C_T_to_A,
                                     Flow_C_A_to_T=net_flow$flow_C_A_to_T,
                                     Flow_N_T_to_A=net_flow$flow_N_T_to_A,
                                     Flow_N_A_to_T=net_flow$flow_N_A_to_T
                                     
    ))
    
    if (nr%%n_point==0) print(nr)
    
  }
  write.table(d2,paste0("./Table/Space_rB_rP_",scenario_space$Name_scena[rowspace],"_phi_",p_coup,"_DC_.csv"),sep=";")
  write.table(d_recy,paste0("./Table/Space_rB_rP_recy",scenario_space$Name_scena[rowspace],"_phi_",p_coup,"_DC_.csv"),sep=";")
  
  
}#end scenario limitation


### >> Feedback ----

n_point=100;p_coup=1;  
param_space=expand.grid(rP=c(1/10,1/40),rB=c(0.12,0.25));p_seq=seq(0,1,length.out=n_point)

for (s in c('C-limited','N-limited')){ 
  d=d2=d3=tibble()
  N_rep=ifelse(s!="N-limited",1,1)
  
  for (nr in 1:nrow(param_space)){
    for (p in p_seq){
      
      param=Get_classical_param(scena = s,coupling = F)
      param$rP=param_space$rP[nr];param$rB=param_space$rB[nr]
      param[c("pH",'pC',"pP","pB")]=p
      state=Get_initial_values(param)
      
      
      if (s=="N-limited"){
        param$lNa=2
      }
      
      data_save=Compute_ode(state,param,n_time = (10000),type_ode = "DC")
      Eq=Extract_equilibrium_from_dynamics(data_save,param) #Equilibrium
      
      limit_ratio=Get_limitation(Eq$Eq,param)
      
      P1_bidirectional=Primary_production(Eq$Eq,param,DC=T)
      P2_bidirectional=Secondary_production(Eq$Eq,param,DC=T)
      
      feedback=Compute_feedbacks(Eq$Eq,param,type_prod = "Production",n_time=(10000),DC = T,plot = F)
      
      
      d2=rbind(d2,Eq$Eq %>% add_column(Phi=p, Feedback_T_1=feedback$Net_T_1,Feedback_A_1=feedback$Net_A_1,
                                       Feedback_T_2=feedback$Net_T_2,Feedback_A_2=feedback$Net_A_2,Scenario=s,
                                       P1_bidirec_T=P1_bidirectional$Terrestrial,P1_bidirec_A=P1_bidirectional$Aquatic,
                                       P2_bidirec_T=P2_bidirectional$Terrestrial,P2_bidirec_A=P2_bidirectional$Aquatic,
                                       P1_uni_T=feedback$P1_no_T_to_A,P1_uni_A=feedback$P1_no_A_to_T,P2_uni_T=feedback$P2_no_T_to_A,
                                       P2_uni_A=feedback$P2_no_A_to_T,Ratio=limit_ratio$Ratio,Limitation=limit_ratio$Limitation))
      d=rbind(d,feedback$Eq_uni_A%>%add_column(type_feedback="uni_A",Ratio=Get_limitation(feedback$Eq_uni_A,param)$Ratio,Limitation=Get_limitation(feedback$Eq_uni_A,param)$Limitation),
              feedback$Eq_uni_T%>%add_column(type_feedback="uni_T",Ratio=Get_limitation(feedback$Eq_uni_T,param)$Ratio,Limitation=Get_limitation(feedback$Eq_uni_T,param)$Limitation))
      
    }
    
  }
  write.table(d2,paste0("./Table/Feedback_",s,"_DC.csv"),sep=";")
  write.table(d ,paste0("./Table/Feedback_all_eq_",s,"_DC.csv"),sep=";")
  
}


## 8) Asymmetry of flows ----
### >> Asymmetry space ----
n_point=25;p_coup=1;  
param_space=expand.grid(rP=c(1/10,1/40),rB=c(0.12,0.25));p_seq=seq(0,1,length.out=n_point)

d2=tibble()
for (s in c("C-limited",'N-limited')){ 
  
  
  for (nr in 1:nrow(param_space)){
    for (p_T in p_seq){
      for (p_A in p_seq){
        param=Get_classical_param(scena = s,coupling = F)
        param$rP=param_space$rP[nr];param$rB=param_space$rB[nr]
        param[c('pC',"pB")]=p_A
        param[c("pH","pP")]=p_T
        state=Get_initial_values(param)
        
        
        data_save=Compute_ode(state,param,n_time = 10000)
        Eq=Extract_equilibrium_from_dynamics(data_save,param) #Equilibrium
        
        limit_ratio=Get_limitation(Eq$Eq,param)
        
        P1_bidirectional=Primary_production(Eq$Eq,param)
        P2_bidirectional=Secondary_production(Eq$Eq,param)
        
  
        
        d2=rbind(d2,Eq$Eq %>% add_column(Phi_T=p_T,Phi_A=p_A,Scenario=s,
                                         P1_bidirec_T=P1_bidirectional$Terrestrial,P1_bidirec_A=P1_bidirectional$Aquatic,
                                         P2_bidirec_T=P2_bidirectional$Terrestrial,P2_bidirec_A=P2_bidirectional$Aquatic,
                                         Ratio=limit_ratio$Ratio,Limitation=limit_ratio$Limitation))
      }
    }
  }
}


write.table(d2,"./Table/Asymmetry_flows_prod.csv",sep=";")

### >> Feedbacks ----

Extract_equilibrium_from_dynamics=function(data,param,consumers=F){
  
  n_begin=ifelse(consumers,19,15)
  
  data_mean=as_tibble(t(colMeans(data[(nrow(data)-round((nrow(data)/2))):nrow(data),-1])))
  data_with_param=cbind(data_mean,matrix(unlist(param),ncol = length(param),nrow=1))
  colnames(data_with_param)[n_begin:ncol(data_with_param)]=names(param)
  
  return(list(Eq=data_with_param))
  
}

n_point=15;p_coup=1;  
param_space=rbind(expand.grid(rP=c(1/10),rB=c(0.25),
                              p_A=c(0,.25,.5,.75,1),p_T=seq(0,1,length.out=n_point),Varying_flow="Terr")%>%
                    add_column(., Asymmetry=rep(paste0(seq(0,1,length.out=5)),n_point)),
                  tibble(rP=c(1/10),rB=c(0.25),
                              p_A=seq(0,1,length.out=n_point),p_T=seq(0,1,length.out=n_point),Varying_flow="Terr")%>%
                    add_column(., Asymmetry="Symmetry"),
                  expand.grid(rP=c(1/10),rB=c(0.25),
                              p_A=seq(0,1,length.out=n_point),p_T=c(0,.25,.5,.75,1),Varying_flow="Aqu")%>%
                    add_column(., Asymmetry=rep(paste0(seq(0,1,length.out=5)),each=n_point)),
                  tibble(rP=c(1/10),rB=c(0.25),
                              p_A=seq(0,1,length.out=n_point),p_T=seq(0,1,length.out=n_point),Varying_flow="Aqu")%>%
                    add_column(., Asymmetry="Symmetry"))


d2=tibble()
for (s in c("C-limited",'N-limited')){ 
  for (nr in 1:nrow(param_space)){
    
    
    param=Get_classical_param(scena = s,coupling = F)
    param$rP=param_space$rP[nr];param$rB=param_space$rB[nr]
    param[c("pH","pP")]=param_space$p_T[nr]
    param[c("pB","pC")]=param_space$p_A[nr]
    state=Get_initial_values(param)
    
    
    data_save=Compute_ode(state,param,n_time = 100000)
    Eq=Extract_equilibrium_from_dynamics(data_save,param) #Equilibrium
    
    limit_ratio=Get_limitation(Eq$Eq,param)
    
    P1_bidirectional=Primary_production(Eq$Eq,param)
    P2_bidirectional=Secondary_production(Eq$Eq,param)
    
    feedback=Compute_feedbacks(Eq$Eq,param,type_prod = "Production",n_time=100000)
    
    d2=rbind(d2,Eq$Eq %>% add_column(p_A=param_space$p_A[nr],p_T=param_space$p_T[nr], Feedback_T_1=feedback$Net_T_1,Feedback_A_1=feedback$Net_A_1,
                                     Feedback_T_2=feedback$Net_T_2,Feedback_A_2=feedback$Net_A_2,Scenario=s,
                                     P1_bidirec_T=P1_bidirectional$Terrestrial,P1_bidirec_A=P1_bidirectional$Aquatic,
                                     P2_bidirec_T=P2_bidirectional$Terrestrial,P2_bidirec_A=P2_bidirectional$Aquatic,
                                     P1_uni_T=feedback$P2_no_T_to_A,P1_uni_A=feedback$P1_no_A_to_T,P2_uni_T=feedback$P2_no_T_to_A,
                                     P2_uni_A=feedback$P2_no_A_to_T,Ratio=limit_ratio$Ratio,Limitation=limit_ratio$Limitation))
    
  }
}
write.table(d2%>%
              add_column(., Varying=rep(param_space$Varying,2))%>%
              add_column(., Asymmetry=rep(param_space$Asymmetry,2)),"./Table/Asymmetry_flows_feedback.csv",sep=";")


## 9) Varying the parameters ----
stoichio_seq=expand.grid(rB=seq(0.12,.25,length.out=2),rP=seq(0.025,.1,length.out=2))
delta_X=1;n_point=15
Sensitivity_tab=list(
  #Terrestrial
  
  INt=seq(1,15,length.out=n_point),IDt=seq(1,15,length.out=n_point),
  lNt=seq(0.5,5,length.out=n_point),lDt=seq(0.5,5,length.out=n_point),
  mt=seq(0,1,length.out=n_point),eH=seq(0.2,1,length.out=n_point),
  aP=seq(.1,.7,length.out=n_point),aH=seq(.1,2,length.out=n_point),
  dP=seq(0.01,1,length.out=n_point),dH=seq(0.05,1,length.out=n_point),
  
  #Aquatic
  
  lNa=seq(1,2,length.out=n_point),lDa=seq(.5,1,length.out=n_point),
  ma=seq(0,1,length.out=n_point),eC=seq(0.3,1,length.out=n_point),
  eB=seq(.5,1,length.out=n_point),aBD=seq(.83,2,length.out=n_point),
  aC=seq(0.3,1,length.out=n_point),dB=seq(0.01,.5,length.out=n_point),
  dC=seq(0.01,.15,length.out=n_point))


names_list=c("INt" ,  "IDt" ,  "lNt" ,  "lDt"  , "mt"   , "eH"    ,  "aP" , "aH" ,  "dP" , "dH"   , 
             "lNa"  , "lDa" ,  "ma"  ,  "eC"  ,  "eB"   ,  "aBD", "aC"  ,  "dB"  ,  "dC","INa" ,  "IDa"  )

time_ode=5000;colim=F


Extract_equilibrium_from_dynamics=function(data,param,consumers=F){
  
  n_begin=ifelse(consumers,19,15)
  
  data_mean=as_tibble(t(colMeans(data[(nrow(data)-4000):nrow(data),-1])))
  data_with_param=cbind(data_mean,matrix(unlist(param),ncol = length(param),nrow=1))
  colnames(data_with_param)[n_begin:ncol(data_with_param)]=names(param)
  
  return(list(Eq=data_with_param))
  
}

for (Scena in c("N-limited","C-limited")){ #for each scenario of limitation
  
  if (Scena=="N-limited"){
    Sensitivity_tab$INa=seq(1,2,length.out=n_point)
    Sensitivity_tab$IDa=seq(12,18,length.out=n_point)
    
  } else {
    
    Sensitivity_tab$INa=seq(2,20,length.out=n_point)
    Sensitivity_tab$IDa=seq(2,20,length.out=n_point)
  }
  
  which_param=1
  
  for (range_param in Sensitivity_tab){ #for each parameter
    print(names_list[which_param])
    d2=d_recy=tibble()
    
    for (nr in 1:nrow(stoichio_seq)){ #for the 4 values in the rP-rB space
      
      param=Get_classical_param(scena = Scena,coupling = T)
      
      param$rB=stoichio_seq$rB[nr];param$rP=stoichio_seq$rP[nr]
      
      
      for (val in range_param){ #along the gradient of each parameter
        
        param[names_list[which_param]]=val #updating the value
        
        param[c("pH",'pC',"pP","pB")]=0 
        
        state=Get_initial_values(param)
        
        data_save=Compute_ode(state,param,n_time = time_ode,type_ode = "full")
        Eq=Extract_equilibrium_from_dynamics(data_save,param ) #Equilibrium
        limit=Get_limitation(Eq$Eq,param)
        
        #productivity with recycling
        P1_recy=Primary_productivity(Eq$Eq,param,colim)
        P2_recy=Secondary_productivity(Eq$Eq,param)
        
        #production with recycling
        PP1_recy=Primary_production(Eq$Eq,param,colim)
        PP2_recy=Secondary_production(Eq$Eq,param)
        
        d_recy=rbind(d_recy,Eq$Eq%>%add_column(PP1_T=PP1_recy$Terrestrial,PP1_A=PP1_recy$Aquatic,P1_T=P1_recy$Terrestrial,P1_A=P1_recy$Aquatic,
                                               PP2_T=PP2_recy$Terrestrial,PP2_A=PP2_recy$Aquatic,P2_T=P2_recy$Terrestrial,P2_A=P2_recy$Aquatic,
                                               rP=param$rP,rB=param$rB,Phi=param$pB))
        
        param[c("pH",'pC',"pP","pB")]=delta_X 
        
        data_save=Compute_ode(state,param,n_time = time_ode,type_ode = "full")
        Eq=Extract_equilibrium_from_dynamics(data_save,param) #Equilibrium
        limit=Get_limitation(Eq$Eq,param)
        
        P1=Primary_productivity(Eq$Eq,param,colim);P2=Secondary_productivity(Eq$Eq,param) #productivity
        PP1=Primary_production(Eq$Eq,param,colim);PP2=Secondary_production(Eq$Eq,param) #production
        
        d2=rbind(d2,Eq$Eq %>% add_column(Phi=delta_X,Ratio=Get_limitation(Eq$Eq,param)$Ratio,Scenario=Scena,
                                         P1_T=P1$Terrestrial,P1_A=P1$Aquatic,P2_T=P2$Terrestrial,P2_A=P2$Aquatic,P1_A_tot=P1$Aqua_tot,
                                         PP1_T=PP1$Terrestrial,PP1_A=PP1$Aquatic,PP2_T=PP2$Terrestrial,PP2_A=PP2$Aquatic,PP1_A_tot=PP1$Aqua_tot,
                                         
                                         #LRR production
                                         LRR_PP1_T=log(PP1$Terrestrial/PP1_recy$Terrestrial),
                                         LRR_PP1_A=log(PP1$Aquatic/PP1_recy$Aquatic),
                                         LRR_PP2_T=log(PP2$Terrestrial/PP2_recy$Terrestrial),
                                         LRR_PP2_A=log(PP2$Aquatic/PP2_recy$Aquatic),
                                         
                                         #Limitation of the system
                                         Limitation=limit$Limitation))
      }
    }# end param list
    
    write.table(d2,paste0("./Table/Sensitivity_indiv_param_",Scena,"_",names_list[which_param],".csv"),sep=";")
    write.table(d_recy,paste0("./Table/Sensitivity_indiv_param_",Scena,"_",names_list[which_param],"_recy.csv"),sep=";")
    
    which_param=which_param+1
    
    param=Get_classical_param(scena = Scena,coupling = T)
    
  }# end main loop in different parameters
}

## 10) Growth efficiency ----

n_point=30;p_coup=1;  
param_space=expand.grid(rP=c(1/10),rB=c(0.25),eB=c(.1,.75,1));p_seq=seq(0,1,length.out=n_point)
Extract_equilibrium_from_dynamics=function(data,param,consumers=F){
  
  n_begin=ifelse(consumers,19,15)
  
  data_mean=as_tibble(t(colMeans(data[(nrow(data)-round(nrow(data)/2)):nrow(data),-1])))
  data_with_param=cbind(data_mean,matrix(unlist(param),ncol = length(param),nrow=1))
  colnames(data_with_param)[n_begin:ncol(data_with_param)]=names(param)
  
  return(list(Eq=data_with_param))
  
}


d2=tibble()

for (nr in 1:nrow(param_space)){
  for (p in p_seq){
    
    param=Get_classical_param(scena = "C-limited",coupling = F)
    param$rP=param_space$rP[nr];param$rB=param_space$rB[nr]
    param[c("pH",'pC',"pP","pB")]=p
    param$eB=param_space$eB[nr]
    state=Get_initial_values(param)
    
    
    data_save=Compute_ode(state,param,n_time = 40000)
    Eq=Extract_equilibrium_from_dynamics(data_save,param) #Equilibrium
    
    limit_ratio=Get_limitation(Eq$Eq,param)
    
    P1_bidirectional=Primary_production(Eq$Eq,param)
    P2_bidirectional=Secondary_production(Eq$Eq,param)
    
    feedback=Compute_feedbacks(Eq$Eq,param,type_prod = "Production",n_time=40000)
    
    
    
    d2=rbind(d2,Eq$Eq %>% add_column(Phi=p, Feedback_T_1=feedback$Net_T_1,Feedback_A_1=feedback$Net_A_1,
                                     Feedback_T_2=feedback$Net_T_2,Feedback_A_2=feedback$Net_A_2,Scenario=s,
                                     P1_bidirec_T=P1_bidirectional$Terrestrial,P1_bidirec_A=P1_bidirectional$Aquatic,
                                     P2_bidirec_T=P2_bidirectional$Terrestrial,P2_bidirec_A=P2_bidirectional$Aquatic,
                                     P1_uni_T=feedback$P2_no_T_to_A,P1_uni_A=feedback$P1_no_A_to_T,P2_uni_T=feedback$P2_no_T_to_A,
                                     P2_uni_A=feedback$P2_no_A_to_T,Ratio=limit_ratio$Ratio,Limitation=limit_ratio$Limitation))

  }
  
}
write.table(d2,paste0("./Table/Feedback_growth_efficiency.csv"),sep=";")



