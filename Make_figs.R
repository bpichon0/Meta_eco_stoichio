rm(list=ls())
library(tidyverse)
library(reshape2)
library(ggtext)
library(ggpubr)
library(latex2exp)
library(RColorBrewer)
library(viridis)

source("Stoichio_functions.R")
dir.create("./Figures/",showWarnings = F)
dir.create("./Figures/SI",showWarnings = F)

the_theme=theme_classic()+theme(legend.position = "bottom",
                                strip.background = element_rect(fill = "#CCE8D8"),
                                strip.text.y = element_text(size = 10, angle = -90),
                                strip.text.x = element_text(size = 8),axis.text = element_text(size=11),axis.title = element_text(size=13),
                                legend.text = element_text(size = 10),text = element_text(family = "NewCenturySchoolbook"))


pal=colorRampPalette(c("#5C7ECC","#32C4E2","#7DCE9A","#B4CE7D","#EFDD35","#FFB700"))
pal_aqua=colorRampPalette((c("white","#CFDCDE","#A4DEE6","#49CADC","#3B7FE0","#193C82","#060D61")))
pal_terr=colorRampPalette((c("white","#D8ECCD","#BBE0A7","#86D45C","#3BA23B","#066F06","#033E03")))


the_theme=theme_classic()+theme(legend.position = "bottom",
                                strip.background = element_rect(fill = "#CCE8D8"),
                                strip.text.y = element_text(size = 10, angle = -90),
                                strip.text.x = element_text(size = 8),axis.text = element_text(size=11),axis.title = element_text(size=13),
                                legend.text = element_text(size = 10),text = element_text(family = "NewCenturySchoolbook"))



#******************************************************************************#

# Step 1: Main figures ----

#******************************************************************************#
## >> Fig : Empirical data ----

color_ac_ter=c('Aquatic'='#90D0E0','Terrestrial'="#66C562")
color_resource=c('N'='#90D0E0','C'="#66C562")
col_matt_type=c("Primary producers"="springgreen4","Invertebrates"="chocolate3","Vertebrates"="red3","POC-DOC"="royalblue")
col_ecosyst=c("Forest"="springgreen4","Grassland"="#66C562","Lake"="#6AA9DA","Stream"="#22429A")
shape_type=c("Aqu. insects"=4,"Zooplankton"=10,"Macroalgae"=24,
             "Carcass"=11,"Mammal faeces"=16,
             "Small amphib."=22,"Terr. plants"=21,"Terr. inverteb."=3,"Phytoplankton"=8,"POC"=23,"DOC"=25)
color_stoichio=c("Forest"="#94D488","Grassland"="#3C8237","Lake"="#5BBED8","Stream"="#4466B7")

the_theme=theme_classic()+theme(legend.position = "bottom",
                                strip.background = element_rect(fill = "#CCE8D8"),
                                strip.text.y = element_text(size = 10, angle = -90, face = "italic"),
                                strip.text.x = element_text(size = 10, face = "italic"),
                                legend.text = element_text(size = 10),text = element_text(family = "NewCenturySchoolbook"))


for (r in c("N","C")){
  assign(paste0("d_cross_",r),read.table(paste0("./Empirical_data/",r,"_flows_Aq_Terr.csv"),sep=";"))
}

d_cross=rbind(d_cross_C%>% add_column(Resource='C'),
              d_cross_N%>%
                add_column(Resource="N" )) %>%
  filter(.,Coupling %in% c("Fresh. to terr.","Terr. to fresh."))%>%
  
  mutate(.,From_ecosys=recode_factor(From_ecosys,"Agroecosystem"="Grassland","Wetland"="Lake"))%>%
  
  mutate(.,To_ecosys=recode_factor(To_ecosys,"Agroecosystem"="Grassland","Wetland"="Lake"))%>%
  
  filter(., To_ecosys!="Desert",From_ecosys!="Desert")


d_cross$Flow_class=sapply(1:nrow(d_cross),function(x){
  if (d_cross$Flow[x]<1){ return("[0,1[")
  }else if (d_cross$Flow[x]>=1 & d_cross$Flow[x]<5){
    return("[1,5[")
  }else if (d_cross$Flow[x]>=5 & d_cross$Flow[x]<20){
    return("[5,20[")
  }else if (d_cross$Flow[x]>=20 & d_cross$Flow[x]<100){
    return("[20,100[")
  }else {
    return(">100")
  }
})

d_cross$Mattyp1[which(d_cross$Coupling=="Terr. to fresh." & d_cross$Mattyp1=="Aqu. insects")]="Terr. inverteb."
d_cross$Mattyp1[which(d_cross$Coupling=="Fresh. to terr." & d_cross$Mattyp1=="Small amphib.")]="Amphib."
d_cross$Mattyp1[which(d_cross$Coupling=="Terr. to fresh." & d_cross$Mattyp1=="Small amphib.")]="Amphib."


p_save3= ggplot(d_cross) +
  geom_bar(aes(x = Flow_class, fill = interaction(Mattyp1,Coupling),color=interaction(Mattyp1,Coupling)),position = "stack",width = .7) +
  the_theme+labs(fill="",x=TeX("Class of flow magnitude, in gDW.$m^{-2}.yr^{-1}"),y="Number of observations",
                 color="")+
  scale_fill_manual(values=c("#74BEEF","#6474DE","#03116B","#000000","#CA3A3A","#CA813A","#CA8686","gray",
                             "#66A46A","#87D45F"),
                    labels=c("Fresh. amphib. (n = 26)","Fresh. insects (n = 156)","Fresh. carcass (n = 28)",
                             "Terr. amphib. (n = 24)","Terr. carcass (n = 2)","Terr. DOC (n = 1)",
                             "Terr. mammal faeces (n = 4)","Terr. POC (n = 3)",
                             "Terr. inverteb. (n = 123)","Terr. plants (n = 161)"))+
  scale_color_manual(values=c("#74BEEF","#6474DE","#03116B","#000000","#CA3A3A","#CA813A","#CA8686","gray",
                              "#66A46A","#87D45F"),
                     labels=c("Fresh. amphib. (n = 26)","Fresh. insects (n = 156)","Fresh. carcass (n = 28)",
                              "Terr. amphib. (n = 24)","Terr. carcass (n = 2)","Terr. DOC (n = 1)",
                              "Terr. mammal faeces (n = 4)","Terr. POC (n = 3)",
                              "Terr. inverteb. (n = 123)","Terr. plants (n = 161)"))


rectangle=tibble(xmin=c(.5,2.7),xmax=c(2.3,4.5),ymin=c(0,0),ymax=rep(max(d_cross$Flow),2),name=c("N","C"))

# p_save1=ggplot(NULL)+
#   
#   geom_boxplot(data=d_cross,aes(x=interaction(Coupling,Resource),y=Flow,group=interaction(Coupling,Resource)),width=0.4,outlier.shape = NA,color="white",fill="gray50",
#                position = position_dodge(width = 1),alpha=.4)+ 
#   
#   geom_point(data=d_cross,aes(x=interaction(Coupling,Resource),y=Flow,color=Mattyp2,group=interaction(Mattyp2,Coupling,Resource)),
#              position=position_jitterdodge(dodge.width=.4,jitter.width=.05),alpha=.8)+ 
#   
#   geom_rect(data=NULL,aes(xmin=.5,xmax=2.35,ymin=0,ymax=max(d_cross$Flow)),alpha=.1,fill="#D2B96F")+
#   geom_rect(data=NULL,aes(xmin=2.65,xmax=4.5,ymin=0,ymax=max(d_cross$Flow)),alpha=.1,fill="#AC86DA")+
#   
#   scale_y_continuous(trans = "log10",breaks = c(0.01,0.1,1,10,100))+the_theme+
#   theme(legend.position = "bottom")+
#   scale_color_manual(values=col_matt_type)+
#   scale_x_discrete(labels=c("Fresh. to terr.", "Terr. to fresh." ,"Fresh. to terr.", "Terr. to fresh."))+
#   labs(x="",fill="",y=expression(paste("Cross-ecosystem flows of resources (g.",m^{"-2"},".", yr^{"-1"},")")),shape="",color="")+
#   guides(shape=F)+theme(axis.title.y = element_text(size=10),axis.text.x = element_text(size=10))+
#   annotate("text",x=1:4,y=5e3,label=paste0(round((d_cross%>% group_by(Resource,Coupling) %>% summarise(Mean_flow=mean(Flow),.groups = "keep"))$Mean_flow,2)),
#            size=3)+
#   annotate("text",x=1:4,y=0.0001,label=paste0("n = ",as.numeric(table(d_cross$Coupling,d_cross$Resource))),size=3)+
#   the_theme+theme(axis.title.x = element_blank())


p_save1=ggplot(NULL)+
  
  geom_boxplot(data=d_cross,aes(x=interaction(Coupling,Resource),y=Flow,
                                group=interaction(Coupling,Resource)),width=0.4,outlier.shape = NA,color="white",fill="gray50",
               position = position_dodge(width = 1),alpha=.4)+ 
  
  geom_point(data=d_cross,aes(x=interaction(Coupling,Resource),y=Flow,shape=Mattyp1,color=From_ecosys,
                              group=interaction(Mattyp1,Coupling,Resource,From_ecosys)),
             position=position_jitterdodge(dodge.width=.4,jitter.width=.05))+ 
  
  geom_rect(data=NULL,aes(xmin=.5,xmax=2.35,ymin=0,ymax=max(d_cross$Flow)),alpha=.1,fill="#D2B96F")+
  geom_rect(data=NULL,aes(xmin=2.65,xmax=4.5,ymin=0,ymax=max(d_cross$Flow)),alpha=.1,fill="#AC86DA")+
  
  scale_y_continuous(trans = "log10",breaks = c(0.01,0.1,1,10,100))+the_theme+
  theme(legend.position = "bottom")+
  scale_color_manual(values=color_stoichio)+
  scale_shape_manual(values=shape_type)+
  scale_x_discrete(labels=c("Fresh. to terr.", "Terr. to fresh." ,"Fresh. to terr.", "Terr. to fresh."))+
  labs(x="",fill="",y=expression(paste("Cross-ecosystem flows of resources (g.",m^{"-2"},".", yr^{"-1"},")")),shape="",color="")+
  theme(axis.title.y = element_text(size=10),axis.text.x = element_text(size=10),legend.box = "vertical",axis.title.x = element_blank())+
  annotate("text",x=1:4,y=5e3,label=paste0(round((d_cross%>% group_by(Resource,Coupling) %>% summarise(Mean_flow=mean(Flow),.groups = "keep"))$Mean_flow,2)),
           size=3.5)+
  annotate("text",x=1:4,y=0.0001,label=paste0("n = ",as.numeric(table(d_cross$Coupling,d_cross$Resource))),size=3.5)+
  geom_text(data=tibble(text=c("Carbon","Nitrogen"),x=c(1.5,3.5),y=c(3e2,3e2)),aes(x=x,y=y,label=text),fontface = "bold")+
  guides(color = guide_legend(override.aes = list(size = 4)))

# p1 = ggplot(d_cross%>%group_by(., Mattyp1,Coupling)%>%summarise(.groups = "keep",mean_flow=mean(Flow)), 
#             aes(x = "", y = mean_flow, fill = interaction(Mattyp1,Coupling),color=interaction(Mattyp1,Coupling))) +
#   geom_col() +
#   coord_polar(theta = "y")+theme_transparent()+labs(fill="Distribution of aquatic-terrestrial resource fluxes",
#                                                     color="Distribution of aquatic-terrestrial resource fluxes")+
#   scale_fill_manual(values=c("#74BEEF","#6474DE","#03116B","#CA813A","#000000","#CA8686","gray","#CA3A3A",
#                              "#66A46A","#87D45F"),
#                     labels=c("Aqu. insects (n = 156)","Aqu. carcass (n = 28)","Aqu. amphib. (n = 26)","Terr. carcass (n = 2)","Terr. DOC (n = 1)","Terr. mammal faeces (n = 4)",
#                              "Terr. POC (n = 3)","Terr. amphib. (n = 24)","Terr. inverteb. (n = 123)","Terr. plants (n = 161)","Terr. "))+
#   scale_color_manual(values=c("#74BEEF","#6474DE","#03116B","#CA813A","#000000","#CA8686","gray","#CA3A3A",
#                               "#66A46A","#87D45F"),
#                      labels=c("Aqu. insects (n = 156)","Aqu. carcass (n = 28)","Aqu. amphib. (n = 26)","Terr. carcass (n = 2)","Terr. DOC (n = 1)","Terr. mammal faeces (n = 4)",
#                               "Terr. POC (n = 3)","Terr. amphib. (n = 24)","Terr. inverteb. (n = 123)","Terr. plants (n = 161)","Terr. "))

d_stoichio=read.table("./Empirical_data/Stoichio_NC.csv",sep=";")

p_save2=ggplot(d_stoichio,
               aes(x=Exporter_ecosyst,y=Ratio,shape=Mattyp1))+
  geom_boxplot(aes(fill=Exporter_ecosyst,group=Exporter_ecosyst),width=0.4,outlier.shape = NA,color="white",
               position = position_dodge(width = 1),alpha=.4)+ 
  geom_point(aes(color=Exporter_ecosyst),position=position_jitterdodge(dodge.width=.4,jitter.width=.1))+
  annotate("text",x=1:4,y=0.005,label=paste0("n = ",table(d_stoichio$Exporter_ecosyst)),size=3.5)+
  labs(x="",fill="",y="N:C",shape="",color="",shape="")+scale_y_log10()+
  scale_shape_manual(values=shape_type)+
  scale_fill_manual(values=color_stoichio)+
  scale_color_manual(values=color_stoichio)+
  the_theme+theme(axis.title.x = element_blank())+
  guides(color=F,fill=F)


p_tot=ggarrange(p_save3+
                  theme(legend.text = element_text(size=10),legend.position = c(.6, .8),axis.text.x = element_text(size=12),
                        axis.text.y = element_text(size=12), axis.title = element_text(size=12))+
                  guides(fill=guide_legend(ncol=2),color=F),
                ggarrange(p_save2+
                            theme(legend.position="none",axis.text.x = element_text(size=12),
                                  axis.text.y = element_text(size=12), axis.title = element_text(size=12)),
                          p_save1+
                            theme(axis.title.y = element_text(size=12),axis.text.x = element_text(size=12),
                                  axis.text.y = element_text(size=12), axis.title = element_text(size=12)),
                          nrow = 2,hjust = -3.3,labels = LETTERS[2:3],
                          font.label = list(size=20),heights = c(1,1.7)),
                labels=c(LETTERS[1],"",""),nrow = 2,heights = c(1,2),hjust =-4,font.label = list(size=20))
ggsave("./Figures/Fig1.pdf",p_tot,width = 6,height = 12)




## >> Fig : N-limited & C-limited , explanation patterns ----

for (scena in c("C-limited","N-limited")){
  type_prod="PP"
  d2=read.table(paste0("./Table/Space_rB_rP_",scena,"_phi_1.csv"),sep=";")
  
  d2_melt=melt(d2 , measure.vars = c(paste0(type_prod,"1_A"),
                                     paste0(type_prod,"1_T")))
  
  name_plot=c("Aquatic basal production","Terrestrial basal production")
  
  u=1
  for (pl in unique(d2_melt$variable)){
    
    if (pl %in% c("PP2_T","PP1_T")) pal_col=pal_terr(100)
    if (pl %in% c("PP2_A","PP1_A")) pal_col=pal_aqua(100)
    
    assign(paste0("p_",u),ggplot(filter(d2_melt,variable==pl))+geom_tile(aes(x=rP,y=rB,fill=value),width=1/200,height=1/200)+the_theme+
             labs(y=expression(paste("N:C decomposers (r"[B],")")),x=expression(paste("N:C plants (r"[P],")")),fill='')+ggtitle(name_plot[u])+
             scale_fill_gradientn(colors = pal_col)+geom_contour(aes(x=rP,y=rB,z=value),breaks = c(0),linetype=1,lwd=.01,color="black")+
             theme(plot.title = element_text(size=12)))
    
    
    u=u+1
  }
  if (scena=="C-limited"){
    col_annotate=c("white",rep("black",3))
  } else{
    col_annotate=c("white","black","white","black")
    
    
  }
  p_2=p_2+annotate("text",x=c(.027,.1,.027,.1),y=c(.2475,.2475,.125,.125),label=c("(i)",'(ii)','(iii)','(iv)'),color=col_annotate)
  
  col_terr=pal_terr(100)
  col_aq=pal_aqua(100)
  
  
  if (scena=="C-limited"){
    p_1=p_1+scale_fill_gradientn(colours=col_aq,breaks=c(2.42,2.44,2.46))
  }
  if (scena=="N-limited"){
    p_2=p_2+scale_fill_gradientn(colours=col_terr,breaks=c(4.60,4.70))

  }
  
  
  p_1=p_1+labs(y="")
  p_tile=ggarrange(p_2+theme(axis.title = element_text(size=13),axis.text.x = element_text(size=12),
                             plot.title = element_text(size=12),axis.text.y = element_text(size=12)),
                   p_1+theme(axis.title = element_text(size=13),axis.text.x = element_text(size=12),
                             plot.title = element_text(size=12),axis.text.y = element_text(size=12)),
                   ncol=2,nrow=1)
  
  p_space=expand.grid(rB=c(.12,.25),rP=c(.025,.1))
  param=Get_classical_param(scena = scena,coupling=T)
  
  for (i in 1:nrow(p_space)){
    
    param$rB=p_space$rB[i]; param$rP=p_space$rP[i]
    assign(paste0("param_",i),param)
    
    state=Get_initial_values(param)
    data=Compute_ode(state,param,optim_time = F)
    assign(paste0("Eq_",i),Extract_equilibrium_from_dynamics(data,param)$Eq)
    
  }
  Param_all=rbind(param_1,param_2,param_3,param_4)
  
  if (scena=="C-limited"){ 
    All_eq=rbind(Eq_1,Eq_2,Eq_3,Eq_4)[,c(1,3,5,7,9,10,11,12:14)]
    min_col=apply(All_eq,2,min)-.1
    multiplicative=c(10,45,22.6,8.33,50,50,50,50,17,12)
    
    Relative_eq=as_tibble(sapply(1:ncol(All_eq),function(x){
      return(multiplicative[x]*(All_eq[,x]-min_col[x]))
    }));colnames(Relative_eq)=colnames(All_eq)
    
  }
  
  if (scena=="N-limited"){
    All_eq=rbind(Eq_1,Eq_2,Eq_3,Eq_4)[,c(1,5,7,10,11,12:14)]
    min_col=apply(All_eq,2,min)-.1;All_eq$Detritus_A_C=All_eq$Detritus_A_C+2
    multiplicative=c(17,22.6,8.33,30,2,22,20,17)
    
    Relative_eq=as_tibble(sapply(1:ncol(All_eq),function(x){
      return(multiplicative[x]*(All_eq[,x]-min_col[x]))
    }));colnames(Relative_eq)=colnames(All_eq)
    
    Relative_eq=Relative_eq%>%
      mutate(.,Consumers_C=rbind(Eq_1,Eq_2,Eq_3,Eq_4)$Consumers_C/1.2,
             Detritus_T_C=rbind(Eq_1,Eq_2,Eq_3,Eq_4)$Detritus_T_C/1.2)
    All_eq=All_eq%>%
      mutate(.,Consumers_C=rbind(Eq_1,Eq_2,Eq_3,Eq_4)$Consumers_C,
             Detritus_T_C=rbind(Eq_1,Eq_2,Eq_3,Eq_4)$Detritus_T_C)
  }
  
  
  for (i in 1:4) assign(paste0("pp_",i),Plot_meta_ecosystem_graph(data_save=All_eq[i,] ,
                                                                  data =  Relative_eq[i,],Param_all[i,]))
  
  P_structure=ggarrange(
    pp_2+theme( plot.margin=margin(t = 0, r = 0,b = 1,l = 0,unit = "cm")),
    pp_4+theme( plot.margin=margin(t = 0, r = 0,b = 1,l = 0,unit = "cm")),
    pp_1+theme( plot.margin=margin(t = 0, r = 0,b = 1,l = 0,unit = "cm")),
    pp_3+theme( plot.margin=margin(t = 0, r = 0,b = 1,l = 0,unit = "cm")),
    ncol=4,nrow=1,labels = c("(i)", "(ii)","(iii)", "(iv)"),font.label = list(size=12))
  assign(paste0("p_tot_",gsub("-limited","",scena)),ggarrange(p_tile,P_structure,nrow=2,heights =  c(2.5,1)))
}

p_tot=ggarrange(p_tot_C,p_tot_N,
                nrow=2,labels = LETTERS[1:2],font.label = list(size=20),hjust=-1.5)
ggsave(paste0("./Figures/Fig3.pdf"),p_tot,width = 7,height = 10)


## >> Fig : LRR at the meta-ecosystem scale ----

for (scena in c("C-limited","N-limited")){
  type_prod="PP"
  d2=read.table(paste0("./Table/Space_rB_rP_",scena,"_phi_1.csv"),sep=";")
  
  d2_melt=melt(d2 , measure.vars = c("PP_log_sum_1","PP_log_sum_2","PP_log_sum"))
  
  
  name_plot=c("Basal prod. meta-ecosystem","Secondary prod. meta-ecosystem")
  
  u=1
  for (pl in unique(d2_melt$variable)){
    pal_col=pal(100)
    assign(paste0("p_",u),ggplot(filter(d2_melt,variable==pl))+geom_tile(aes(x=rP,y=rB,fill=value),width=1/200,height=1/200)+the_theme+
             labs(y=expression(paste("N:C decomposers (r"[B],")")),x=expression(paste("N:C plants (r"[P],")")),fill='')+ggtitle(name_plot[u])+
             scale_fill_gradient2(low = "red",mid = "white",high = "blue")+#geom_contour(aes(x=rP,y=rB,z=value),breaks = c(-.0001,0.0001),linetype=1,lwd=.1,color="gray50")+
             theme(plot.title = element_text(size=13)))
    u=u+1
  }
  p_1=p_1;p_2=p_2+labs(y="");p_3=p_3+labs(y="")
  
  if (scena=="N-limited"){
    p_1=p_1+ggtitle("");    p_2=p_2+ggtitle("");    p_3=p_3+ggtitle("")
  }
  
  if (scena=="C-limited"){
    p_1=p_1+scale_fill_gradient2(low = "red",mid = "white",high = "blue",breaks=c(-0.01,0.01,0.03))
    p_2=p_2+scale_fill_gradient2(low = "red",mid = "white",high = "blue",breaks=c(0.01,0.03,0.05))
  } else {
    p_1=p_1+scale_fill_gradient2(low = "red",mid = "white",high = "blue",breaks=c(-0.015,-0.005,0.005))
    
  }
  
  assign(paste0("p_tile_",gsub("-limited","",scena)),
         ggarrange(p_1+theme(axis.title = element_text(size=14),axis.text.x = element_text(size=13),
                             plot.title = element_text(size=14),axis.text.y = element_text(size=13)),
                   p_2+theme(axis.title = element_text(size=14),axis.text.x = element_text(size=13),
                             plot.title = element_text(size=14),axis.text.y = element_text(size=13)),
                   ncol=2))
}

p_tot=ggarrange(p_tile_C,
                p_tile_N,
                nrow=2,labels = LETTERS[1:2],font.label = list(size=20))

ggsave(paste0("./Figures/Fig4.pdf"),p_tot,width = 8,height = 9)


## >> Fig : Feedbacks in N and C limited scenarios ----



for (s in c("C-limited","N-limited")){
  
  d2=read.table(paste0("./Table/Feedback_",s,".csv"),sep=";")
  
  d2_mean_rep=d2 %>% filter(.,Scenario==s) %>%
    group_by(.,Scenario,rB,rP,Phi)%>%
    summarise(.,mean_feedbackT1=mean(Feedback_T_1),mean_feedbackT2=mean(Feedback_T_2),
              mean_feedbackA1=mean(Feedback_A_1),mean_feedbackA2=mean(Feedback_A_2),
              sd_feedbackT1=sd(Feedback_T_1),sd_feedbackT2=sd(Feedback_T_2),
              sd_feedbackA1=sd(Feedback_A_1),sd_feedbackA2=sd(Feedback_A_2),.groups='keep')
  d2_mean_rep[,c(5,ncol(d2_mean_rep))]=round(d2_mean_rep[,c(5,ncol(d2_mean_rep))],10)
  
  
  phi_keep=seq(0,1,length.out=100)[round(seq(1,100,length.out=12))]
  
  
  assign(paste0("p_",1),
         ggplot(NULL)+
           geom_point(data=filter(d2_mean_rep,round(Phi,6) %in% round(phi_keep,6)),aes(x=Phi,y=mean_feedbackT1,shape=interaction(rP,rB)),color="#3FB500",size=3)+
           geom_line(data=d2_mean_rep,aes(x=Phi,y=mean_feedbackT1,group=interaction(rP,rB)),color="#3FB500")+
           
           ggtitle("Terr. basal prod.")+
           scale_shape_manual(values=c(1,2,8,11),labels=c(TeX("$r_P = 0.025, r_B = 0.12$"),TeX("$r_P = 0.1, r_B = 0.12$"),
                                                         TeX("$r_P = 0.025, r_B = 0.25$"),TeX("$r_P = 0.1, r_B = 0.25$")))+
           the_theme+ geom_hline(yintercept=0)+labs(x=TeX("$\\Delta$"),y="Spatial feedback",
                                                    shape=""))
  
  
  
  assign(paste0("p_",2),
         ggplot(NULL)+
           geom_point(data=filter(d2_mean_rep,round(Phi,6) %in% round(phi_keep,6)),aes(x=Phi,y=mean_feedbackT2,shape=interaction(rP,rB)),color="#3D8814",size=3)+
           geom_line(data=d2_mean_rep,aes(x=Phi,y=mean_feedbackT2,group=interaction(rP,rB)),color="#3D8814")+
           
           ggtitle("Terr. secondary prod.")+
           scale_shape_manual(values=c(1,2,8,11),labels=c(TeX("$r_P = 0.025, r_B = 0.12$"),TeX("$r_P = 0.1, r_B = 0.12$"),
                                                         TeX("$r_P = 0.025, r_B = 0.25$"),TeX("$r_P = 0.1, r_B = 0.25$")))+
           the_theme+ geom_hline(yintercept=0)+labs(x=TeX("$\\Delta$"),y="",
                                                    shape=""))
  
  assign(paste0("p_",3),
         ggplot(NULL)+
           geom_point(data=filter(d2_mean_rep,round(Phi,6) %in% round(phi_keep,6)),aes(x=Phi,y=mean_feedbackA1,shape=interaction(rP,rB)),color="#2EAEBF",size=3)+
           geom_line(data=d2_mean_rep,aes(x=Phi,y=mean_feedbackA1,group=interaction(rP,rB)),color="#2EAEBF")+
           
           ggtitle("Aqu. basal prod.")+
           scale_shape_manual(values=c(1,2,8,11),labels=c(TeX("$r_P = 0.025, r_B = 0.12$"),TeX("$r_P = 0.1, r_B = 0.12$"),
                                                         TeX("$r_P = 0.025, r_B = 0.25$"),TeX("$r_P = 0.1, r_B = 0.25$")))+
           the_theme+ geom_hline(yintercept=0)+labs(x=TeX("$\\Delta$"),y="",
                                                    shape=""))
  
  assign(paste0("p_",4),
         ggplot(NULL)+
           geom_point(data=filter(d2_mean_rep,round(Phi,6) %in% round(phi_keep,6)),aes(x=Phi,y=mean_feedbackA2,shape=interaction(rP,rB)),color="#2B50A7",size=3)+
           geom_line(data=d2_mean_rep,aes(x=Phi,y=mean_feedbackA2,group=interaction(rP,rB)),color="#2B50A7")+
           
           ggtitle("Aqu. secondary prod.")+
           scale_shape_manual(values=c(1,2,8,11),labels=c(TeX("$r_P = 0.025, r_B = 0.12$"),TeX("$r_P = 0.1, r_B = 0.12$"),
                                                         TeX("$r_P = 0.025, r_B = 0.25$"),TeX("$r_P = 0.1, r_B = 0.25$")))+
           the_theme+ geom_hline(yintercept=0)+labs(x=TeX("$\\Delta$"),y="",
                                                    shape=""))
  
  p_legend=ggplot(NULL)+
    geom_point(data=filter(d2_mean_rep,round(Phi,6) %in% round(phi_keep,6)),aes(x=Phi,y=mean_feedbackA1,shape=interaction(rP,rB)),color="black",size=3)+
    theme_classic()+
    scale_shape_manual(values=c(1,2,8,11),labels=c(TeX("$ \ r_P = 0.025, r_B = 0.12  $"),TeX("$ \  r_P = 0.1, r_B = 0.12  $"),
                                                  TeX("$ \ r_P = 0.025, r_B = 0.25  $"),TeX("$ \  r_P = 0.1, r_B = 0.25  $")))
  
  if (s=="N-limited"){
    
    # Small insert to see when it goes negative
    p_small3=ggplot(NULL)+the_theme+
      geom_point(data=filter(d2_mean_rep,rP==.025,round(Phi,5) %in% round(phi_keep,5) ),
                 aes(x=Phi,y=mean_feedbackA1,shape=interaction(rP,rB)),color="#2EAEBF",size=1.5)+
      geom_line(data=filter(d2_mean_rep,rP==.025),aes(x=Phi,y=mean_feedbackA1,group=interaction(rP,rB)),color="#2EAEBF")+
      
      ggtitle("")+scale_x_continuous(breaks = c(0,1))+
      scale_shape_manual(values=c(8,1))+
      geom_hline(yintercept=0)+labs(x=TeX(""),y="",
                                    shape="",color="")+theme(legend.position = "none",text=element_text(size=6),axis.text = element_text(size=10))
    p_3=p_3 +
      annotation_custom(grob=ggplotGrob(p_small3),
                        ymin = 0.0013, ymax=.003, xmin=-.1, xmax=.6)
    
    
    
    p_small4=ggplot(NULL)+the_theme+
      geom_point(data=filter(d2_mean_rep,rB==.25,round(Phi,5) %in% round(phi_keep,5))%>%filter(., Phi<0.63637),
                 aes(x=Phi,y=mean_feedbackA2,shape=interaction(rP,rB)),color="#2B50A7",size=1.5)+
      geom_line(data=filter(d2_mean_rep,rB==.25)%>%filter(., Phi<0.63637),
                aes(x=Phi,y=mean_feedbackA2,group=interaction(rP,rB)),color="#2B50A7")+
      
      ggtitle("")+scale_x_continuous(breaks = c(0,0.6))+
      scale_shape_manual(values=c(8,11))+
      geom_hline(yintercept=0)+labs(x=TeX(""),y="",
                                    shape="",color="")+theme(legend.position = "none",text=element_text(size=6),axis.text = element_text(size=10))
    p_4=p_4 +
      annotation_custom(grob=ggplotGrob(p_small4),
                        ymin = 0.004, ymax=.009, xmin=-.1, xmax=.6)
  }
  
  if (gsub("-limited","",s)=="C"){
    assign(paste0("p_",gsub("-limited","",s)),ggarrange(p_1+theme(plot.title = element_text(size=16),axis.text = element_text(size=13),
                                                                axis.title.y = element_text(size=17),
                                                                axis.title.x = element_text(size=16),
                                                                legend.text = element_text(size=17)),
                                                        p_2+theme(plot.title = element_text(size=16),axis.text = element_text(size=13),
                                                                  axis.title.x = element_text(size=16),
                                                                  legend.text = element_text(size=17)),
                                                        p_3+theme(plot.title = element_text(size=16),axis.text = element_text(size=13),
                                                                  axis.title.x = element_text(size=16),
                                                                  legend.text = element_text(size=17)),
                                                        p_4+theme(plot.title = element_text(size=16),axis.text = element_text(size=13),
                                                                  axis.title.x = element_text(size=16),
                                                                  legend.text = element_text(size=17)),
                                                        ncol=4,common.legend = F,legend = "none"))
  } else{
    assign(paste0("p_",gsub("-limited","",s)),
           ggarrange(p_1+theme(plot.title=element_blank(),axis.text = element_text(size=13),
                               axis.title.y = element_text(size=17),
                               axis.title.x = element_text(size=20),
                               legend.text = element_text(size=17)),
                     p_2+theme(plot.title=element_blank(),axis.text = element_text(size=13),
                               axis.title.x = element_text(size=20),
                               legend.text = element_text(size=17)),
                     p_3+theme(plot.title=element_blank(),axis.text = element_text(size=13),
                               axis.title.x = element_text(size=20),
                               legend.text = element_text(size=17)),
                     p_4+theme(plot.title=element_blank(),axis.text = element_text(size=13),
                               axis.title.x = element_text(size=20),
                               legend.text = element_text(size=17)),ncol=4,legend = "none"))
  }
  
}

p_feedback=ggarrange(p_C,p_N,ggarrange(ggplot()+theme_void(),
                                       get_legend(p_legend+labs(shape="")+guides(shape=guide_legend(nrow=2),shape=F)+
                                                    theme(legend.text = element_text(size=16))),
                                       ggplot()+theme_void(),ncol=3,widths = c(.01,3,.01)),
                     nrow = 3,common.legend = T,labels = c(LETTERS[1:2],""),heights = c(1,1,.3),font.label = list(size=20),vjust = c(2,0))


ggsave(paste0("./Figures/Fig_5.pdf"),p_feedback,width = 14,height = 7)


























#******************************************************************************#

# Step 2: SI figures ----

#******************************************************************************#

## >> Fig N & C limited, secondary production ----
for (scena in c("C-limited","N-limited")){
  type_prod="PP"
  d2=read.table(paste0("./Table/Space_rB_rP_",scena,"_phi_1.csv"),sep=";")
  

  d2_melt=melt(d2 , measure.vars = c(paste0(type_prod,"2_A"),paste0(type_prod,"2_T")
  ))
  
  name_plot=c("Aquatic secondary production","Terrestrial secondary production")
  
  u=1
  for (pl in unique(d2_melt$variable)){
    
    if (pl %in% c("PP2_T","PP1_T")) pal_col=pal_terr(100)
    if (pl %in% c("PP2_A","PP1_A")) pal_col=pal_aqua(100)

    assign(paste0("p_",u),ggplot(filter(d2_melt,variable==pl))+geom_tile(aes(x=rP,y=rB,fill=value),width=1/200,height=1/200)+the_theme+
             labs(y=expression(paste("N:C decomposers (r"[B],")")),x=expression(paste("N:C plants (r"[P],")")),fill='')+ggtitle(name_plot[u])+
             scale_fill_gradientn(colors = pal_col)+geom_contour(aes(x=rP,y=rB,z=value),breaks = c(0),linetype=1,lwd=.01,color="black")+
             theme(plot.title = element_text(size=12)))
    
    
    u=u+1
  }
  
  col_terr=pal_terr(100)
  col_aq=pal_aqua(100)
  
  if (scena=="C-limited"){
    p_1=p_1+scale_fill_gradientn(colours=col_aq,breaks=c(1.02,1.025,1.03))
    p_2=p_2+scale_fill_gradientn(colours=col_terr,breaks=c(1.1,1.15,1.2))
    
  }
  if (scena=="N-limited"){
    p_2=p_2+scale_fill_gradientn(colours=col_terr,breaks=c(1.1,1.12,1.14))
  }
  
  assign(paste0("p_tile",gsub("-limited","",scena)),ggarrange(p_2,p_1,ncol=2,nrow=1))
  
}
p=ggarrange(p_tileC,p_tileN,ncol=1,nrow=2,labels=LETTERS[1:2],hjust=-2)
ggsave("./Figures/SI/Secondary_production.pdf",p,width = 7,height = 7)




## >> Exported flows ----

#C-limited

d=read.table("./Table/Mecanism_consumers_rB_C-limited.csv",sep=";")
p1_C=ggplot(d%>%melt(., measure.vars=c("N_A_to_T","C_A_to_T"))%>%
              mutate(., variable=recode_factor(variable,"N_A_to_T"=" ","C_A_to_T"="  ")))+geom_point(aes(x=rB,y=value,shape=scena,color=variable),size=3)+
  geom_line(aes(x=rB,y=value,color=variable,group=interaction(scena,variable)))+
  the_theme+facet_wrap(.~variable,scales = "free",ncol=2)+theme(strip.background=element_rect(colour="transparent",fill="transparent"),
                                                                axis.title = element_text(size=9))+
  labs(x=expression(paste("N:C decomposers (r"[B],")")),y="Resource flow to the terrestrial ecosystem",color="",shape="")+
  scale_color_manual(values=(c("#AC86DA","#D2B96F")),labels=c("Nitrogen","Carbon"))+
  scale_shape_manual(values=c(21,0),labels=c(TeX("$\\Delta_C=1$"),TeX("$\\Delta_C=0$")))


d=read.table("./Table/Mecanism_herbivores_rP_C-limited.csv",sep=";")
p2_C=ggplot(d%>%melt(., measure.vars=c("N_T_to_A","C_T_to_A"))%>%
              mutate(., variable=recode_factor(variable,"N_T_to_A"=" ","C_T_to_A"="  ")))+geom_point(aes(x=rP,y=value,shape=scena,color=variable),size=3)+
  geom_line(aes(x=rP,y=value,color=variable,group=interaction(scena,variable)))+
  the_theme+facet_wrap(.~variable,scales = "free",ncol=2)+theme(strip.background=element_rect(colour="transparent",fill="transparent"),
                                                                axis.title = element_text(size=9))+
  labs(x=expression(paste("N:C plants (r"[P],")")),y="Resource flow to the aquatic ecosystem",color="",shape="")+
  scale_color_manual(values=(c("#AC86DA","#D2B96F")),labels=c("Nitrogen","Carbon"))+
  scale_shape_manual(values=c(21,0),labels=c(TeX("$\\Delta_G=1$"),TeX("$\\Delta_G=0$")))


#N-limited

d=read.table("./Table/Mecanism_consumers_rB.csv",sep=";")
p1_N=ggplot(d%>%melt(., measure.vars=c("N_A_to_T","C_A_to_T"))%>%
              mutate(., variable=recode_factor(variable,"N_A_to_T"=" ","C_A_to_T"="  ")))+geom_point(aes(x=rB,y=value,shape=scena,color=variable),size=3)+
  geom_line(aes(x=rB,y=value,color=variable,group=interaction(scena,variable)))+
  the_theme+facet_wrap(.~variable,scales = "free",ncol=2)+theme(strip.background=element_rect(colour="transparent",fill="transparent"),
                                                                axis.title = element_text(size=9))+
  labs(x=expression(paste("N:C decomposers (r"[B],")")),y="Resource flow to the terrestrial ecosystem",color="",shape="")+
  scale_color_manual(values=(c("#AC86DA","#D2B96F")),labels=c("Nitrogen","Carbon"))+
  scale_shape_manual(values=c(21,0),labels=c(TeX("$\\Delta_C=1$"),TeX("$\\Delta_C=0$")))


d=read.table("./Table/Mecanism_herbivores_rP.csv",sep=";")
p2_N=ggplot(d%>%melt(., measure.vars=c("N_T_to_A","C_T_to_A"))%>%
              mutate(., variable=recode_factor(variable,"N_T_to_A"="  ","C_T_to_A"=" ")))+geom_point(aes(x=rP,y=value,shape=scena,color=variable),size=3)+
  geom_line(aes(x=rP,y=value,color=variable,group=interaction(scena,variable)))+
  the_theme+facet_wrap(.~variable,scales = "free",ncol=2)+theme(strip.background=element_rect(colour="transparent",fill="transparent"),
                                                                axis.title = element_text(size=9))+
  labs(x=expression(paste("N:C plants (r"[P],")")),y="Resource flow to the aquatic ecosystem",color="",shape="")+
  scale_color_manual(values=(c("#AC86DA","#D2B96F")),labels=c("Nitrogen","Carbon"))+
  scale_shape_manual(values=c(21,0),labels=c(TeX("$\\Delta_G=1$"),TeX("$\\Delta_G=0$")))


p_rB=ggarrange(p1_C+ggtitle("C-limited")+ theme(plot.title = element_text(hjust = 0.5)),
               p1_N+labs(y="")+ggtitle("N-limited")+ theme(plot.title = element_text(hjust = 0.5)),ncol = 2,heights = c(1,1),common.legend = T,
               legend = 'bottom',hjust=-5,labels = letters[1:2])

p_rP=ggarrange(p2_C,p2_N+labs(y=""),ncol=2,heights = c(1,1),common.legend = T,
               legend = 'bottom',hjust=-5,labels = letters[1:2])


p_tot=ggarrange(p_rB,p_rP,nrow=2,labels = LETTERS[1:2],hjust=-2)
ggsave(paste0("./Figures/SI/Flows_exported.pdf"),p_tot,width = 9,height = 6)

## >> Consumers density ----
d=read.table("./Table/Mecanism_consumers_rB_C-limited.csv",sep=";")
p_consum1_C=ggplot(d%>%melt(., measure.vars=c("Herbivores_C","Consumers_C"))%>%
                     mutate(., variable=recode_factor(variable,"Herbivores_C"=" ","Consumers_C"="  ")))+
  geom_point(aes(x=rB,y=value,shape=scena,color=variable),size=2)+
  geom_line(aes(x=rB,y=value,color=variable,group=interaction(scena,variable)))+
  the_theme+facet_wrap(.~variable,scales = "free",ncol=2)+theme(strip.background=element_rect(colour="transparent",fill="transparent"),
                                                                axis.title = element_text(size=9))+
  labs(x=expression(paste("N:C decomposers (r"[B],")")),y="Carbon content",color="",shape="")+
  scale_color_manual(values=(c(col.alpha("red",.3),col.alpha("red",.9))),labels=c("Grazers","Consumers"))+
  scale_shape_manual(values=c(21,0),labels=c(TeX("$\\Delta_C=1$"),TeX("$\\Delta_C=0$")))


d=read.table("./Table/Mecanism_herbivores_rP_C-limited.csv",sep=";")
p_consum2_C=ggplot(d%>%melt(., measure.vars=c("Herbivores_C","Consumers_C"))%>%
                     mutate(., variable=recode_factor(variable,"Herbivores_C"=" ","Consumers_C"="  ")))+
  geom_point(aes(x=rP,y=value,shape=scena,color=variable),size=2)+
  geom_line(aes(x=rP,y=value,color=variable,group=interaction(scena,variable)))+
  the_theme+facet_wrap(.~variable,scales = "free",ncol=2)+theme(strip.background=element_rect(colour="transparent",fill="transparent"),
                                                                axis.title = element_text(size=9))+
  labs(x=expression(paste("N:C plants (r"[P],")")),y="Carbon content",color="",shape="")+
  scale_color_manual(values=(c(col.alpha("red",.3),col.alpha("red",.9))),labels=c("Grazers","Consumers"))+
  scale_shape_manual(values=c(21,0),labels=c(TeX("$\\Delta_G=1$"),TeX("$\\Delta_G=0$")))


#N-limited

d=read.table("./Table/Mecanism_consumers_rB.csv",sep=";")
p_consum1_N=ggplot(d%>%melt(., measure.vars=c("Herbivores_C","Consumers_C"))%>%
                     mutate(., variable=recode_factor(variable,"Herbivores_C"=" ","Consumers_C"="  ")))+
  geom_point(aes(x=rB,y=value,shape=scena,color=variable),size=2)+
  geom_line(aes(x=rB,y=value,color=variable,group=interaction(scena,variable)))+
  the_theme+facet_wrap(.~variable,scales = "free",ncol=2)+theme(strip.background=element_rect(colour="transparent",fill="transparent"),
                                                                axis.title = element_text(size=9))+
  labs(x=expression(paste("N:C decomposers (r"[B],")")),y="Carbon content",color="",shape="")+
  scale_color_manual(values=(c(col.alpha("red",.3),col.alpha("red",.9))),labels=c("Grazers","Consumers"))+
  scale_shape_manual(values=c(21,0),labels=c(TeX("$\\Delta_C=1$"),TeX("$\\Delta_C=0$")))




d=read.table("./Table/Mecanism_herbivores_rP.csv",sep=";")
p_consum2_N=ggplot(d%>%melt(., measure.vars=c("Herbivores_C","Consumers_C"))%>%
                     mutate(., variable=recode_factor(variable,"Herbivores_C"=" ","Consumers_C"="  ")))+
  geom_point(aes(x=rP,y=value,shape=scena,color=variable),size=2)+
  geom_line(aes(x=rP,y=value,color=variable,group=interaction(scena,variable)))+
  the_theme+facet_wrap(.~variable,scales = "free",ncol=2)+theme(strip.background=element_rect(colour="transparent",fill="transparent"),
                                                                axis.title = element_text(size=9))+
  labs(x=expression(paste("N:C plants (r"[P],")")),y="Carbon content",color="",shape="")+
  scale_color_manual(values=(c(col.alpha("red",.3),col.alpha("red",.9))),labels=c("Grazers","Consumers"))+
  scale_shape_manual(values=c(21,0),labels=c(TeX("$\\Delta_G=1$"),TeX("$\\Delta_G=0$")))


p_rB=ggarrange(p_consum1_C+ggtitle("C-limited")+ theme(plot.title = element_text(hjust = 0.5)),
               p_consum1_N+labs(y="")+ggtitle("N-limited")+ theme(plot.title = element_text(hjust = 0.5)),ncol = 2,heights = c(1,1),common.legend = T,
               legend = 'bottom',hjust=-3.5,labels = letters[1:2])

p_rP=ggarrange(p_consum2_C,p_consum2_N+labs(y=""),ncol=2,heights = c(1,1),common.legend = T,
               legend = 'bottom',hjust=-3.5,labels = letters[1:2])


p_tot=ggarrange(p_rB,p_rP,nrow=2,labels = LETTERS[1:2])

ggsave(paste0("./Figures/SI/Consumers_density.pdf"),p_tot,width = 9,height = 6)


## >> Feedback N limited, biotic abiotic pools ----

scena="N-limited"
phi_keep=seq(0,1,length.out=100)[round(seq(1,100,length.out=12))]


pdf(paste0("./Figures/SI/Explanation_feedback_",scena,".pdf"),width = 8,height = 6)

d2=read.table(paste0("./Table/Feedback_",scena,".csv"),sep=";")%>%
  
  mutate(.,
         Flow_N_A_to_T=unlist(sapply(1:nrow(.),function(x){(.$rB[x]-.$eC[x]*.$rC[x])*.$aC[x]*.$Decomposers_C[x]*.$Consumers_C[x]*.$pC[x]  +  #excretion consumers
             .$rC[x]*.$dC[x]*.$Consumers_C[x]*.$pC[x]   +  .$rB[x]*.$dB[x]*.$Decomposers_C[x]*.$pB[x]})),
         Flow_N_T_to_A=unlist(sapply(1:nrow(.),function(x){(.$rP[x]-.$eH[x]*.$rH[x])*.$aH[x]*.$Herbivores_C[x]*.$Plants_C[x]*.$pH[x]  +  #excretion herbivores
             .$rH[x]*.$dH[x]*.$Herbivores_C[x]*.$pH[x]  +  .$rP[x]*.$dP[x]*.$Plants_C[x]*.$pP[x]})),
         Flow_C_A_to_T=unlist(sapply(1:nrow(.),function(x){.$dC[x]*.$Consumers_C[x]*.$pC[x] + .$dB[x]*.$Decomposers_C[x]*.$pB[x]})),
         Flow_C_T_to_A=unlist(sapply(1:nrow(.),function(x){.$dH[x]*.$Herbivores_C[x]*.$pH[x] + .$dP[x]*.$Plants_C[x]*.$pP[x]})))

d_all=read.table(paste0("./Table/Feedback_all_eq_",scena,".csv"),sep=";")%>%
  
  mutate(.,
         Flow_N_A_to_T=sapply(1:nrow(.),function(x){(.$rB-.$eC*.$rC)*.$aC*.$Decomposers_C*.$Consumers_C*.$pC  +  #excretion consumers
             .$rC*.$dC*.$Consumers_C*.$pC   +  .$rB*.$dB*.$Decomposers_C*.$pB}),
         Flow_N_T_to_A=sapply(1:nrow(.),function(x){(.$rP-.$eH*.$rH)*.$aH*.$Herbivores_C*.$Plants_C*.$pH  +  #excretion herbivores
             .$rH*.$dH*.$Herbivores_C*.$pH  +  .$rP*.$dP*.$Plants_C*.$pP}),
         Flow_C_A_to_T=sapply(1:nrow(.),function(x){.$dC*.$Consumers_C*.$pC + .$dB*.$Decomposers_C*.$pB}),
         Flow_C_T_to_A=sapply(1:nrow(.),function(x){.$dH*.$Herbivores_C*.$pH + .$dP*.$Plants_C*.$pP}))

d_all$Scenario=rep(d2$Scenario,each=2)


#Evaluating the feedback for each trophic level + abiotic pool


d_feedback=d2
d_feedback[,c("Herbivores_C","Nitrogen_T_N","Detritus_T_C","Detritus_T_N","Flow_N_T_to_A","Flow_C_T_to_A")]=
  d_feedback[,c("Herbivores_C","Nitrogen_T_N","Detritus_T_C","Detritus_T_N","Flow_N_T_to_A","Flow_C_T_to_A")]-
  d_all[which(d_all$type_feedback=="uni_T"),c("Herbivores_C","Nitrogen_T_N","Detritus_T_C","Detritus_T_N","Flow_N_T_to_A","Flow_C_T_to_A")]
d_feedback[,c("Consumers_C","Nitrogen_A_N","Detritus_A_C","Detritus_A_N","Flow_N_A_to_T","Flow_C_A_to_T")]=
  d_feedback[,c("Consumers_C","Nitrogen_A_N","Detritus_A_C","Detritus_A_N","Flow_N_A_to_T","Flow_C_A_to_T")]-
  d_all[which(d_all$type_feedback=="uni_A"),c("Consumers_C","Nitrogen_A_N","Detritus_A_C","Detritus_A_N","Flow_N_A_to_T","Flow_C_A_to_T")]


d2_mean_rep=d_feedback %>%
  group_by(.,Scenario,rB,rP,Phi)%>%
  summarise(.,
            mean_H=mean(Herbivores_C),
            mean_C=mean(Consumers_C),
            mean_DCa=mean(Detritus_A_C),
            mean_DNa=mean(Detritus_A_N),
            mean_DCt=mean(Detritus_T_C),
            mean_DNt=mean(Detritus_T_N),
            mean_Nt=mean(Nitrogen_T_N),
            mean_Na=mean(Nitrogen_A_N),
            mean_N_A_to_T=mean(Flow_N_A_to_T),
            mean_N_T_to_A=mean(Flow_N_T_to_A),
            mean_C_A_to_T=mean(Flow_C_A_to_T),
            mean_C_T_to_A=mean(Flow_C_T_to_A),.groups='keep')



d=d2_mean_rep %>%melt(., id.vars=c("Scenario","rB","rP","Phi")) %>%
  mutate(.,stat=rep(c("mean"),each=nrow(.)), pool=rep(rep(c("H","C","DCa","DNa","DCt","DNt","Nt","Na","N_A_to_T","N_T_to_A","C_A_to_T","C_T_to_A"),each=nrow(.)/12),1))%>%
  mutate(., value=as.numeric(value))%>%
  
  pivot_wider(.,names_from = stat, values_from = value)%>%
  mutate(., mean=as.numeric(mean))%>%
  
  mutate(., pool=recode_factor(pool,"N_T_to_A" = "Nitrogen flow to Aq. ecos.",
                               "N_A_to_T" = "Nitrogen flow to Ter. ecos.",
                               "C_T_to_A" = "Carbon flow to Aq. ecos.",
                               "C_A_to_T" = "Carbon flow to Ter. ecos."))

print(ggplot(NULL)+               
        geom_point(data=d%>%filter(.,round(Phi,6) %in% round(phi_keep,6)),aes(x=Phi,y=mean,shape=interaction(rP,rB)),size=1.5,color="gray50")+
        geom_line(data=d,aes(x=Phi,y=mean,group=interaction(rP,rB,pool)),color="gray50")+
        scale_shape_manual(values=c(1,2,0,5))+
        the_theme+ geom_hline(yintercept=0)+labs(x=TeX("$\\Delta_X$"),y="Feedback on pools",
                                                 shape=expression(paste("N:C plants (r"[P],")",", N:C decomposers (r"[B],")")))+
        facet_wrap(.~pool,scales='free_y')+theme(panel.border = element_rect(fill="transparent"),strip.background=element_rect(colour="transparent",
                                                                                                                               fill="transparent"),
                                                 strip.text = element_text(size=7),axis.text = element_text(size=7)))

dev.off()




## >> Change in N:C of detritus ----

d_tot=tibble()
for (s in c("C-limited","N-limited","Colimitation")){
  
  d2=read.table(paste0("./Table/Feedback_",s,".csv"),sep=";")
  phi_keep=seq(0,1,length.out=100)[round(seq(1,100,length.out=12))]
  
  d2_mean_rep=d2 %>% filter(.,Scenario==s,round(Phi,6) %in% round(phi_keep,6)) %>%
    group_by(.,Scenario,rB,rP,Phi)%>%
    summarise(.,mean_N_C_Dt=mean(Detritus_T_N/Detritus_T_C),mean_N_C_Da=mean(Detritus_A_N/Detritus_A_C),
              sd_N_C_Dt=sd(Detritus_T_N/Detritus_T_C),sd_N_C_Da=sd(Detritus_A_N/Detritus_A_C),mean_R=mean(Ratio),.groups = "keep")
  
  d_tot=rbind(d_tot,d2_mean_rep)
}

p_ter=ggplot(d_tot%>% filter(., rP==.025))+
  geom_point(aes(x=Phi,y=mean_N_C_Dt,shape=interaction(rB)),color="#3D8814",size=3)+
  geom_errorbar(aes(x=Phi,ymin=mean_N_C_Dt-sd_N_C_Dt,
                    ymax=mean_N_C_Dt+sd_N_C_Dt,y=mean_N_C_Dt,
                    fill=interaction(rP,rB)),color="#3FB500",width=.01)+
  geom_line(aes(x=Phi,y=mean_N_C_Dt,group=interaction(rP,rB)),color="#3D8814")+
  
  facet_wrap(Scenario~.,ncol=3)+
  
  scale_shape_manual(values=c(1,0))+
  the_theme+labs(x=TeX("$ \\Delta_X$"),y="N:C terr. detritus",
                 shape=expression(paste("N:C decomposers (r"[B],")   ")))+
  theme(plot.title = element_text(size=11),axis.text = element_text(size=10),
        axis.title.y = element_text(size=12),legend.position = "bottom",
        axis.title.x = element_text(size=14),strip.background=element_rect(colour="transparent",
                                                                           fill="transparent"),
        strip.text.x = element_text(size=13),
        legend.title = element_text(size=10))


p_aq=ggplot(d_tot%>% filter(., rB==.12))+
  geom_point(aes(x=Phi,y=mean_N_C_Da,shape=interaction(rP)),color="#2B50A7",size=3)+
  geom_errorbar(aes(x=Phi,ymin=mean_N_C_Da-sd_N_C_Da,
                    ymax=mean_N_C_Da+sd_N_C_Da,y=mean_N_C_Da,
                    fill=interaction(rP,rB)),color="#2B50A7",width=.01)+
  geom_line(aes(x=Phi,y=mean_N_C_Da,group=interaction(rP,rB)),color="#2B50A7")+
  
  facet_wrap(Scenario~.,ncol=3)+
  
  scale_shape_manual(values=c(1,2))+
  the_theme+labs(x=TeX("$ \\Delta_X$"),y="N:C aq. detritus",
                 shape=expression(paste("N:C plants (r"[P],")")))+
  theme(plot.title = element_text(size=11),axis.text = element_text(size=10),
        axis.title.y = element_text(size=12),legend.position = "bottom",
        axis.title.x = element_text(size=14),strip.background=element_rect(colour="white",
                                                                           fill="white"),
        
        strip.text = element_text(color="white"),legend.title = element_text(size=10))

p_slim=ggplot(d_tot%>% filter(.))+
  geom_point(aes(x=Phi,y=mean_R,shape=interaction(rP,rB)),color="black",size=2)+
  geom_line(aes(x=Phi,y=mean_R,group=interaction(rP,rB)),color="black")+
  facet_wrap(Scenario~.,ncol=3,scales = "free")+
  
  scale_shape_manual(values=c(1,2,5,0))+
  the_theme+labs(x=TeX("$ \\Delta_X$"),y=expression(paste("S"[lim])),
                 shape=expression(paste("N:C plants (r"[P],"), N:C decomposers (r"[B],")   ")))+
  theme(plot.title = element_text(size=11),axis.text = element_text(size=10),
        axis.title.y = element_text(size=12),legend.position = "bottom",
        axis.title.x = element_text(size=14),strip.background=element_rect(colour="white",
                                                                           fill="white"),
        
        strip.text = element_text(color="white"),legend.title = element_text(size=10),axis.title.y.left = element_text(size=15))


p=ggarrange(p_ter,p_aq,p_slim,nrow=3,labels = LETTERS[1:3], font.label = list(size = 18))

ggsave("./Figures/SI/Detritus_N_C.pdf",p,width = 8,height = 8)


## >> Adding top predators ----
for (scena in c("C-limited","N-limited")){
  type_prod="PP"
  d2=read.table(paste0("./Table/Space_rB_rP_",scena,"_phi_Top_pred_1.csv"),sep=";")
  d2=d2%>%
    mutate(., 
           PP3_A=eTC*aTC*Consumers_C*Top_cons_C_C,
           PP3_T=eTH*aTH*Herbivores_C*Top_cons_H_C)
  d2_melt=melt(d2 , measure.vars = c(paste0(type_prod,"1_A"),paste0(type_prod,"1_T"),paste0(type_prod,"2_A"),paste0(type_prod,"2_T"),
                                     paste0(type_prod,"3_A"),paste0(type_prod,"3_T")
  ))
  
  name_plot=c("Aquatic basal production","Terrestrial basal production","Aquatic secondary production","Terrestrial secondary production",
              "Aquatic top consumers production","Terrestrial top consumers production")
  
  if (scena=="N-limited")d2_melt=filter(d2_melt,rB>.12)
  
  u=1
  for (pl in unique(d2_melt$variable)){
    
    if (pl %in% c("PP2_T","PP1_T","PP3_T")) pal_col=pal_terr(100)
    if (pl %in% c("PP2_A","PP1_A","PP3_A")) pal_col=pal_aqua(100)
    
    
    assign(paste0("p_",u),ggplot(filter(d2_melt,variable==pl))+geom_tile(aes(x=rP,y=rB,fill=value))+the_theme+
             labs(y=expression(paste("N:C decomposers (r"[B],")")),x=expression(paste("N:C plants (r"[P],")")),fill='')+ggtitle(name_plot[u])+
             scale_fill_gradientn(colors = pal_col)+geom_contour(aes(x=rP,y=rB,z=value),breaks = c(0),linetype=1,lwd=.01,color="gray50")+
             theme(plot.title = element_text(size=14)))
    
    u=u+1
  }
  
  col_terr=pal_terr(100)
  col_aq=pal_aqua(100)
  
  
  
  if (scena=="N-limited"){
    p_1=p_1+scale_fill_gradientn(colours=col_aq,breaks=c(0.1,0.13,0.16))
    p_2=p_2+scale_fill_gradientn(colours=col_terr,breaks=c(-1.5,-1))
    p_4=p_4+scale_fill_gradientn(colours=col_terr,breaks=c(-1.5,-1))
    p_6=p_6+scale_fill_gradientn(colours=col_terr,breaks=c(-1.5,-1))
    p_5=p_5+scale_fill_gradientn(colours=col_aq,breaks=c(-3,-2,-1))
  }
  
  p_2=p_2+labs(x="");p_1=p_1+labs(x='',y="");p_3=p_3+labs(y="",x="");p_4=p_4+labs(x='');p_5=p_5+labs(y='')
  p_tile=ggarrange(p_2,p_1,p_4,p_3,p_6,p_5,ncol=2,nrow=3)
  
  ggsave(paste0("./Figures/SI/Space_rP_rB_LRR_",scena,"_phi_1_Top_predator_Production.pdf"),p_tile,width = 8,height = 11)
  
}

for (s in c("C-limited","N-limited")){
  
  d2=read.table(paste0("./Table/Feedback_Top_pred_",s,".csv"),sep=";")
  
  d2_mean_rep=d2 %>% filter(.,Scenario==s) %>%
    group_by(.,Scenario,rB,rP,Phi)%>%
    summarise(.,mean_feedbackT1=mean(Feedback_T_1),mean_feedbackT2=mean(Feedback_T_2),mean_feedbackT3=mean(Feedback_T_3),
              mean_feedbackA1=mean(Feedback_A_1),mean_feedbackA2=mean(Feedback_A_2),mean_feedbackA3=mean(Feedback_A_3),
              sd_feedbackT1=sd(Feedback_T_1),sd_feedbackT2=sd(Feedback_T_2),
              sd_feedbackA1=sd(Feedback_A_1),sd_feedbackA2=sd(Feedback_A_2),.groups='keep')
  d2_mean_rep[,c(5,ncol(d2_mean_rep))]=round(d2_mean_rep[,c(5,ncol(d2_mean_rep))],10)
  
  
  phi_keep=seq(0,1,length.out=50)[round(seq(1,50,length.out=12))]
  
  
  assign(paste0("p_",1),
         ggplot(NULL)+
           geom_point(data=filter(d2_mean_rep,round(Phi,6) %in% round(phi_keep,6)),aes(x=Phi,y=mean_feedbackT1,shape=interaction(rP,rB)),color="#3FB500",size=3)+
           geom_line(data=d2_mean_rep,aes(x=Phi,y=mean_feedbackT1,group=interaction(rP,rB)),color="#3FB500")+
           
           ggtitle("Terrestrial basal production")+
           scale_shape_manual(values=c(1,2,0,5))+
           the_theme+ geom_hline(yintercept=0)+labs(x=TeX("$\\Delta_X$"),y="Feedback on production",
                                                    shape=expression(paste("N:C plants (r"[P],")",", N:C decomposers (r"[B],")   ")))+
           theme(plot.title = element_text(size=9),axis.text = element_text(size=10),
                 axis.title.y = element_text(size=12),
                 axis.title.x = element_text(size=14),
                 legend.text = element_text(size=12),legend.title = element_text(size=11)))
  
  
  
  assign(paste0("p_",2),
         ggplot(NULL)+
           geom_point(data=filter(d2_mean_rep,round(Phi,6) %in% round(phi_keep,6)),aes(x=Phi,y=mean_feedbackT2,shape=interaction(rP,rB)),color="#3D8814",size=3)+
           geom_line(data=d2_mean_rep,aes(x=Phi,y=mean_feedbackT2,group=interaction(rP,rB)),color="#3D8814")+
           
           ggtitle("Terrestrial secondary production")+
           scale_shape_manual(values=c(1,2,0,5))+
           the_theme+ geom_hline(yintercept=0)+labs(x=TeX("$\\Delta_X$"),y="",
                                                    shape=expression(paste("N:C plants (r"[P],")",", N:C decomposers (r"[B],")   ")))+
           theme(plot.title = element_text(size=9),axis.text = element_text(size=10),
                 axis.title.y = element_text(size=12),
                 axis.title.x = element_text(size=14),
                 legend.text = element_text(size=12),legend.title = element_text(size=11)))
  
  
  assign(paste0("p_",3),
         ggplot(NULL)+
           geom_point(data=filter(d2_mean_rep,round(Phi,6) %in% round(phi_keep,6)),aes(x=Phi,y=mean_feedbackT3,shape=interaction(rP,rB)),color="#004E21",size=3)+
           geom_line(data=d2_mean_rep,aes(x=Phi,y=mean_feedbackT3,group=interaction(rP,rB)),color="#004E21")+
           
           ggtitle("Terrestrial top consumers production")+
           scale_shape_manual(values=c(1,2,0,5))+
           the_theme+ geom_hline(yintercept=0)+labs(x=TeX("$\\Delta_X$"),y="",
                                                    shape=expression(paste("N:C plants (r"[P],")",", N:C decomposers (r"[B],")   ")))+
           theme(plot.title = element_text(size=9),axis.text = element_text(size=10),
                 axis.title.y = element_text(size=12),
                 axis.title.x = element_text(size=14),
                 legend.text = element_text(size=12),legend.title = element_text(size=11)))
  
  
  assign(paste0("p_",4),
         ggplot(NULL)+
           geom_point(data=filter(d2_mean_rep,round(Phi,6) %in% round(phi_keep,6)),aes(x=Phi,y=mean_feedbackA1,shape=interaction(rP,rB)),color="#2EAEBF",size=3)+
           geom_line(data=d2_mean_rep,aes(x=Phi,y=mean_feedbackA1,group=interaction(rP,rB)),color="#2EAEBF")+
           
           ggtitle("Aquatic basal production")+
           scale_shape_manual(values=c(1,2,0,5))+
           the_theme+ geom_hline(yintercept=0)+labs(x=TeX("$\\Delta_X$"),y="Feedback on production",
                                                    shape=expression(paste("N:C plants (r"[P],")",", N:C decomposers (r"[B],")   ")))+
           theme(plot.title = element_text(size=9),axis.text = element_text(size=10),
                 axis.title.y = element_text(size=12),
                 axis.title.x = element_text(size=14),
                 legend.text = element_text(size=12),legend.title = element_text(size=11)))
  
  
  assign(paste0("p_",5),
         ggplot(NULL)+
           geom_point(data=filter(d2_mean_rep,round(Phi,6) %in% round(phi_keep,6)),aes(x=Phi,y=mean_feedbackA2,shape=interaction(rP,rB)),color="#2B50A7",size=3)+
           geom_line(data=d2_mean_rep,aes(x=Phi,y=mean_feedbackA2,group=interaction(rP,rB)),color="#2B50A7")+
           
           ggtitle("Aquatic secondary production")+
           scale_shape_manual(values=c(1,2,0,5))+
           the_theme+ geom_hline(yintercept=0)+labs(x=TeX("$\\Delta_X$"),y="",
                                                    shape=expression(paste("N:C plants (r"[P],")",", N:C decomposers (r"[B],")   ")))+
           theme(plot.title = element_text(size=9),axis.text = element_text(size=10),
                 axis.title.y = element_text(size=12),
                 axis.title.x = element_text(size=14),
                 legend.text = element_text(size=12),legend.title = element_text(size=11)))
  
  
  assign(paste0("p_",6),
         ggplot(NULL)+
           geom_point(data=filter(d2_mean_rep,round(Phi,6) %in% round(phi_keep,6)),aes(x=Phi,y=mean_feedbackA3,shape=interaction(rP,rB)),color="#000080",size=3)+
           geom_line(data=d2_mean_rep,aes(x=Phi,y=mean_feedbackA3,group=interaction(rP,rB)),color="#000080")+
           
           ggtitle("Aquatic top consumers production")+
           scale_shape_manual(values=c(1,2,0,5))+
           the_theme+ geom_hline(yintercept=0)+labs(x=TeX("$\\Delta_X$"),y="",
                                                    shape=expression(paste("N:C plants (r"[P],")",", N:C decomposers (r"[B],")   ")))+
           theme(plot.title = element_text(size=9),axis.text = element_text(size=10),
                 axis.title.y = element_text(size=12),
                 axis.title.x = element_text(size=14),
                 legend.text = element_text(size=12),legend.title = element_text(size=11)))
  
  
  p=ggarrange(ggarrange(p_1,p_2,p_3,legend = "none",ncol=3),ggarrange(p_4,p_5,p_6,ncol=3,legend = "bottom",common.legend = T),nrow=2,common.legend = T,legend = "bottom")
  ggsave(paste0("./Figures/SI/Feedback_",s,"top_pred.pdf"),p,width = 9,height = 6)
  
}

## >> Donnor Control scenario ----
for (scena in c("C-limited","N-limited")){
  type_prod="PP"
  d2=read.table(paste0("./Table/Space_rB_rP_",scena,"_phi_1_DC_.csv"),sep=";")
  
  d2_melt=melt(d2 , measure.vars = c(paste0(type_prod,"1_A"),paste0(type_prod,"1_T"),paste0(type_prod,"2_A"),paste0(type_prod,"2_T")))
  
  name_plot=c("Aquatic basal production","Terrestrial basal production","Aquatic secondary production","Terrestrial secondary production")
  
  if (scena=="N-limited")d2_melt=filter(d2_melt,rB>.12)
  
  u=1
  for (pl in unique(d2_melt$variable)){
    
    if (pl %in% c("PP2_T","PP1_T")) pal_col=pal_terr(100)
    if (pl %in% c("PP2_A","PP1_A")) pal_col=pal_aqua(100)
    
    
    assign(paste0("p_",u),ggplot(filter(d2_melt,variable==pl))+geom_tile(aes(x=rP,y=rB,fill=value))+the_theme+
             labs(y=expression(paste("N:C decomposers (r"[B],")")),x=expression(paste("N:C plants (r"[P],")")),fill='')+ggtitle(name_plot[u])+
             scale_fill_gradientn(colors = pal_col)+geom_contour(aes(x=rP,y=rB,z=value),breaks = c(0),linetype=1,lwd=.01,color="gray50")+
             theme(plot.title = element_text(size=14)))
    
    u=u+1
  }
  
  col_terr=pal_terr(100)
  col_aq=pal_aqua(100)
  
  
  
  
  p_2=p_2+labs(x="");p_1=p_1+labs(x='',y="");p_3=p_3+labs(y="",x="");p_4=p_4+labs(x='');p_5=p_5+labs(y='')
  p_tile=ggarrange(p_2,p_1,p_4,p_3,ncol=2,nrow=2)
  
  ggsave(paste0("./Figures/SI/Space_rP_rB_LRR_",scena,"_phi_1_DC.pdf"),p_tile,width = 8,height = 7)
  
}



for (s in c("C-limited","N-limited")){
  
  d2=read.table(paste0("./Table/Feedback_",s,"_DC.csv"),sep=";")
  
  d2_mean_rep=d2 %>% filter(.,Scenario==s) %>%
    group_by(.,Scenario,rB,rP,Phi)%>%
    summarise(.,mean_feedbackT1=mean(Feedback_T_1),mean_feedbackT2=mean(Feedback_T_2),
              mean_feedbackA1=mean(Feedback_A_1),mean_feedbackA2=mean(Feedback_A_2),
              sd_feedbackT1=sd(Feedback_T_1),sd_feedbackT2=sd(Feedback_T_2),
              sd_feedbackA1=sd(Feedback_A_1),sd_feedbackA2=sd(Feedback_A_2),.groups='keep')
  d2_mean_rep[,c(5,ncol(d2_mean_rep))]=round(d2_mean_rep[,c(5,ncol(d2_mean_rep))],10)
  
  phi_keep= unique(d2_mean_rep$Phi)[round(seq(1,100,length.out=12))]
  
  assign(paste0("p_",1),
         ggplot(NULL)+
           geom_point(data=filter(d2_mean_rep,Phi %in% phi_keep),
                      aes(x=Phi,y=mean_feedbackT1,shape=interaction(rP,rB)),color="#3FB500",size=2)+
           geom_line(data=d2_mean_rep,aes(x=Phi,y=mean_feedbackT1,group=interaction(rP,rB)),color="#3FB500")+
           ggtitle("Terrestrial basal production")+
           scale_shape_manual(values=c(1,2,0,5))+
           the_theme+ geom_hline(yintercept=0)+labs(x=TeX("$\\Delta_X$"),y="Feedback on production",
                                                    shape=expression(paste("N:C plants (r"[P],")",", N:C decomposers (r"[B],")   ")))+
           theme(plot.title = element_text(size=11),axis.text = element_text(size=10),
                 axis.title.y = element_text(size=12),
                 axis.title.x = element_text(size=14),
                 legend.text = element_text(size=12),legend.title = element_text(size=13)))
  
  
  
  assign(paste0("p_",2),
         ggplot(NULL)+
           geom_point(data=filter(d2_mean_rep,Phi %in% phi_keep),
                      aes(x=Phi,y=mean_feedbackT2,shape=interaction(rP,rB)),color="#3D8814",size=2)+
           geom_line(data=d2_mean_rep,aes(x=Phi,y=mean_feedbackT2,group=interaction(rP,rB)),color="#3D8814")+
           ggtitle("Terrestrial secondary production")+
           scale_shape_manual(values=c(1,2,0,5))+
           the_theme+ geom_hline(yintercept=0)+labs(x=TeX("$\\Delta_X$"),y="",
                                                    shape=expression(paste("N:C plants (r"[P],")",", N:C decomposers (r"[B],")   ")))+
           theme(plot.title = element_text(size=11),axis.text = element_text(size=10),
                 axis.title.y = element_text(size=12),
                 axis.title.x = element_text(size=14),
                 legend.text = element_text(size=12),legend.title = element_text(size=13)))
  
  assign(paste0("p_",3),
         ggplot(NULL)+
           geom_point(data=filter(d2_mean_rep,Phi %in% phi_keep),
                      aes(x=Phi,y=mean_feedbackA1,shape=interaction(rP,rB)),color="#2EAEBF",size=2)+
           geom_line(data=d2_mean_rep,aes(x=Phi,y=mean_feedbackA1,group=interaction(rP,rB)),color="#2EAEBF")+
           ggtitle("Aquatic basal production")+
           scale_shape_manual(values=c(1,2,0,5))+
           the_theme+ geom_hline(yintercept=0)+labs(x=TeX("$\\Delta_X$"),y="",
                                                    shape=expression(paste("N:C plants (r"[P],")",", N:C decomposers (r"[B],")   ")))+
           theme(plot.title = element_text(size=11),axis.text = element_text(size=10),
                 axis.title.y = element_text(size=12),
                 axis.title.x = element_text(size=14),
                 legend.text = element_text(size=12),legend.title = element_text(size=13)))
  
  assign(paste0("p_",4),
         ggplot(NULL)+
           geom_point(data=filter(d2_mean_rep,Phi %in% phi_keep),
                      aes(x=Phi,y=mean_feedbackA2,shape=interaction(rP,rB)),color="#2B50A7",size=2)+
           geom_line(data=d2_mean_rep,aes(x=Phi,y=mean_feedbackA2,group=interaction(rP,rB)),color="#2B50A7")+
           ggtitle("Aquatic secondary production")+
           scale_shape_manual(values=c(1,2,0,5))+
           the_theme+ geom_hline(yintercept=0)+labs(x=TeX("$\\Delta_X$"),y="",
                                                    shape=expression(paste("N:C plants (r"[P],")",", N:C decomposers (r"[B],")   ")))+
           theme(plot.title = element_text(size=11),axis.text = element_text(size=10),
                 axis.title.y = element_text(size=12),
                 axis.title.x = element_text(size=14),
                 legend.text = element_text(size=12),legend.title = element_text(size=13)))
  
  
  if (s=="N-limited"){
    
    d2_mean_rep3=filter(d2_mean_rep,rP==.025)
    p_small=ggplot(NULL)+
      geom_point(data=filter(d2_mean_rep3,Phi %in% phi_keep),
                 aes(x=Phi,y=mean_feedbackA2,shape=interaction(rP,rB)),color="#2EAEBF",size=2)+
      geom_line(data=d2_mean_rep3,aes(x=Phi,y=mean_feedbackA2,group=interaction(rP,rB)),color="#2EAEBF")+
      ggtitle("")+the_theme+
      scale_shape_manual(values=c(1,0))+
      geom_hline(yintercept=0)+labs(x=TeX(""),y="",
                                    shape="",color="")+theme(legend.position = "none",text=element_text(size=5))
    p_3=p_3 +
      annotation_custom(grob=ggplotGrob(p_small),
                        ymin = 0.00005, ymax=0.00015, xmin=-.1, xmax=.66)
    
    
  }
  if (gsub(pattern = '-limited',"",s)=="N"){p_1=p_1+ggtitle("");p_2=p_2+ggtitle("");p_3=p_3+ggtitle("");p_4=p_4+ggtitle("")
  p_3=p_3+scale_y_continuous(breaks = seq(0,0.00015,length.out=4),labels = seq(0,1.5,length.out=4))+
    annotate("text",x=0.02,y=0.000175,label=TeX("$10^{-4}$"))
  
  }
  
  
  assign(paste0("p_",gsub(pattern = '-limited',"",s)),ggarrange(p_1,p_2,p_3,p_4,ncol=4,common.legend = T,legend = ifelse(gsub(pattern = '-limited',"",s)=="N","bottom","none")))
}

p_feedback_dc=ggarrange(p_C,p_N,nrow=2,labels = LETTERS[1:2],common.legend = T,legend = "bottom")
ggsave(paste0("./Figures/SI/Feedback_DC.pdf"),p_feedback_dc,width = 12,height = 8)




## >> Transient increase coupling ----
ode_metaecosystem=function(t,state,param){
  "Main function for the meta-ecosystem model"
  
  names(state)=c("H","HN","C","CN","P","PN","B",'BN',"DCt","DNt","DCa","DNa","Nt","Na")
  
  
  iT = param$pulse_T(t)
  iA = param$pulse_A(t)
  
  with (as.list(c(state, param)),{
    
    
    
    fH      = aH*P*H
    fP      = aP*Nt*P
    fC      = aC*B*C
    fBN     = aBN*Na
    fBD     = aBD*DCa
    
    if (colim==T) {immo    = ((rB-(DNa/DCa))/rB)*eB*fBD*fBN
    } else{immo    = min(((rB-(DNa/DCa))/rB)*eB*fBD,fBN)}
    
    decompo = immo/((rB-(DNa/DCa))/rB)
    
    #green ecosystem
    d_H   = eH*fH   -  dH*H
    d_HN  = rH*eH*fH-  rH*dH*H
    
    d_P   = fP      -  fH     -  dP*P
    d_PN  = rP*fP   -  rP*fH  -  rP*dP*P
    
    d_DNt = IDt*(DNt/DCt)    -   lDt*DNt          +
      rH*dH*H*(1-(iT+pH))   +   rP*dP*P*(1-(iT+pP))   -     mt*DNt               +             
      rC*dC*C*(iA+pC)       +   rB*dB*B*(iA+pB)                                     
    
    d_DCt = IDt              -   lDt*DCt          +
      dH*H*(1-(iT+pH))      +   dP*P*(1-(iT+pP))      -     mt*DCt               +             
      dC*C*(iA+pC)          +   dB*B*(iA+pB)                                        
    
    d_Nt  = INt   -  lNt*Nt  -   rP*fP                                       +
      (rP-eH*rH)*fH*(1-(iT+pH))                  +     (rB-eC*rC)*fC*(iA+pC)     +             
      mt*DNt                                                  
    
    
    #brown ecosystem
    d_C    = eC*fC            -  dC*C
    d_CN   = rC*eC*fC         -  rC*dC*C
    
    d_BC   = decompo          -  fC                -  dB*B    - ma*B
    
    d_BN   = immo*rB          +  decompo*(DNa/DCa) -  fC*rB   - dB*B*rB  -  ma*B*rB
    
    d_DNa  = IDa*(DNa/DCa)    -  lDa*DNa           +
      rC*dC*C*(1-(iA+pC))   +  rB*dB*B*(1-(iA+pB))    -  decompo*(DNa/DCa)  +      
      rH*dH*H*(iT+pH)       +  rP*dP*P*(iT+pP)                                     
    
    d_DCa  = IDa              -  lDa*DCa           +
      dC*C*(1-(iA+pC))      +  dB*B*(1-(iA+pB))       -  decompo            +      
      dH*H*(iT+pH)          +  dP*P*(iT+pP)                                        
    
    d_Na  = INa   -  lNa*Na   -  immo*rB              +
      (rB-eC*rC)*fC*(1-(iA+pC))             +  ma*B*rB            +      
      (rP-eH*rH)*fH*(iT+pH)                                              
    
    list(c(    Herbivores_C  = d_H    ,  Herbivores_N  = d_HN ,
               Consumers_C   = d_C    ,  Consumers_N   = d_CN ,
               Plants_C      = d_P    ,  Plants_N      = d_PN ,
               Decomposers_C = d_BC   ,  Decomposers_N = d_BN ,
               Detritus_T_C  = d_DCt  ,  Detritus_T_N  = d_DNt, 
               Detritus_A_C  = d_DCa  ,  Detritus_A_N  = d_DNa,
               Nitrogen_T    = d_Nt   ,  Nitrogen_A    = d_Na  ))
  })
  
}






data_change=function(data){
  colors = c("Consumers" = "darkorange", "Producers" = "green3", "Nitrogen" = "darkorchid2")
  
  the_theme=theme_classic()+theme(legend.position = "bottom",
                                  strip.background = element_rect(fill = "#CCE8D8"),
                                  strip.text.y = element_text(size = 10, angle = -90, face = "italic"),
                                  strip.text.x = element_text(size = 10, face = "italic"),
                                  legend.text = element_text(size = 10))
  
  data=gather(data,variable, value,-Time)
  data$foodweb=Get_foodweb(data$variable); data$trophic_level=Get_trophic_level(data$variable)
  return(data)
}


colors = c("Consumers" = "darkorange", "Producers" = "green3")

Get_transient_dynamics=function(way="T",phi=0,limitation="C-limited",NCplant=.025,NCdecomp=.12){
  
  # reaching equilibrium
  param=Get_classical_param(scena =limitation,coupling = T)
  param[c("pP","pH","pC","pB")]=phi
  param["rB"]=NCdecomp
  param["rP"]=NCplant
  state=Get_initial_values(param)
  
  param$pulse_T <- function(t_) {
    i_M_t = ifelse(t_ > 10 & t_ <= (20), 0, 0)
    return(i_M_t)
  }
  param$pulse_A <- function(t_) {
    i_M_t = ifelse(t_ > 10 & t_ <= (20), 0, 0)
    return(i_M_t)
  }
  
  
  data_save=as.data.frame(ode(y = state,times = 1:10000,parms = param,method = "lsoda",func = ode_metaecosystem))
  
  colnames(data_save)=c("Time","Herbivores_C","Herbivores_N","Consumers_C","Consumers_N","Plants_C",
                        "Plants_N","Decomposers_C","Decomposers_N","Detritus_T_C","Detritus_T_N",
                        "Detritus_A_C","Detritus_A_N","Nitrogen_T_N","Nitrogen_A_N")
  
  # plot_dynamics(data_save)
  
  Eq1=Extract_equilibrium_from_dynamics(data_save,param)$Eq #Equilibrium
  
  
  # perturbing the spatial flows
  state[1:14]=as.numeric(Eq1[1:14])
  
  if (way =="T"){
    param$pulse_T <- function(t_) {
      i_M_t = ifelse(t_ > 100 & t_ <= (200), 1e-1, 0)
      return(i_M_t)
    }
    param$pulse_A <- function(t_) {
      i_M_t = ifelse(t_ > 100 & t_ <= (200), 0, 0)
      return(i_M_t)
    }
    
    
  }else {
    
    param$pulse_T <- function(t_) {
      i_M_t = ifelse(t_ > 100 & t_ <= (200), 0, 0)
      return(i_M_t)
    }
    param$pulse_A <- function(t_) {
      i_M_t = ifelse(t_ > 100 & t_ <= (200), 1e-1, 0)
      return(i_M_t)
    }
    
  }
  
  data_save=as.data.frame(ode(y = state,times = 1:500,parms = param,method = "lsoda",func = ode_metaecosystem))
  colnames(data_save)=c("Time","Herbivores_C","Herbivores_N","Consumers_C","Consumers_N","Plants_C",
                        "Plants_N","Decomposers_C","Decomposers_N","Detritus_T_C","Detritus_T_N",
                        "Detritus_A_C","Detritus_A_N","Nitrogen_T_N","Nitrogen_A_N")
  
  data_save=data_save[,c(1:2,4,6,8,10:15)]
  data_save[,-1]=scale(data_save[,-1])
  
  data_save=data_change(data_save)%>%
    filter(foodweb==ifelse(way=="T","Terrestrial","Aquatic"),variable %in% c("Herbivores_C","Plants_C",
                                                                             "Decomposers_C","Consumers_C"))%>%
    mutate(., foodweb=recode_factor(foodweb,"Aquatic"="Aquatic ecosystem","Terrestrial" = "Terrestrial ecosystem"))
  
  p=ggplot(data_save)+
    geom_line(aes(x=Time,y=value,color=trophic_level),lwd=1)+
    geom_rect(data=tibble(xmin=100,xmax=200,ymin=min(data_save$value)-.75,ymax=min(data_save$value)-.5),
              aes(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax),fill="#61CBFD")+
    labs(x="Time",y="Scaled densities",color="")+scale_color_manual(values=colors)+
    facet_grid(.~foodweb)+the_theme+xlim(0,400)+
    theme(strip.text.x = element_text(size=12),legend.text = element_text(size=15))+
    guides(color = guide_legend(override.aes = list(size = 10)))
  
  return(p)
}

param_for_plot=tibble(way=c("T","A","T","T","T","T","T","T","T"),
                      phi=c(.5,.5,0,0,0,0,0,.5,.9),
                      NCp=c(.025,.025,.025,.025,.1,.1,.1,.1,.1),
                      NCd=c(.12,.12,.12,.25,.12,.25,.25,.25,.25),
                      limitation=c("C-limited","C-limited","N-limited","N-limited","N-limited","N-limited","N-limited","N-limited","N-limited"))


for (i in 1:nrow(param_for_plot)){
  assign(paste0("p_",i),Get_transient_dynamics(way=param_for_plot$way[i],phi = param_for_plot$phi[i],
                                               limitation = param_for_plot$limitation[i],
                                               NCplant = param_for_plot$NCp[i],NCdecomp = param_for_plot$NCd[i]))
  
}

p_tot=ggarrange(ggarrange(p_1,p_2,ncol = 2,nrow = 1,legend = "none",labels=c(letters[1],"")),
                ggarrange(p_3+ggtitle(TeX("$r_B = 0.12, r_P = 0.025$")),p_4+ggtitle(TeX("$r_B = 0.25, r_P = 0.025$"))+
                            theme(axis.title.y = element_blank()),
                          p_5+ggtitle(TeX("$r_B = 0.12, r_P = 0.1$")),p_6+ggtitle(TeX("$r_B = 0.25, r_P = 0.1$"))+
                            theme(axis.title.y = element_blank()),
                          nrow = 2,ncol = 2,labels = c(letters[2],"","",""),legend = "none"),
                ggarrange(p_7+ggtitle(TeX("$\\phi_X = 0$")),p_8+ggtitle(TeX("$\\phi_X = 0.5$"))+theme(axis.title.y = element_blank()),
                          p_9+ggtitle(TeX("$\\phi_X = 0.9$"))+theme(axis.title.y = element_blank()),legend = "bottom",
                          nrow = 1,ncol = 3,labels = c(letters[3],"",""),common.legend = T)
                ,nrow=3,heights = c(1,2,1.3))

ggsave("./Figures/SI/Pulse_coupling_effects2.pdf",p_tot,width =10,height = 14 )






## >> Varying the parameters ----




n_point=15
stoichio_seq=expand.grid(rB=seq(0.12,.25,length.out=2),rP=seq(0.025,.1,length.out=2))
d_range_all=tibble()

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




for (resource in c("N","C")){
  
  
  if (resource=="N"){
    Sensitivity_tab$INa=seq(1,2,length.out=n_point)
    Sensitivity_tab$IDa=seq(12,18,length.out=n_point)
    
  } else {
    
    Sensitivity_tab$INa=seq(2,20,length.out=n_point)
    Sensitivity_tab$IDa=seq(2,20,length.out=n_point)
  }
  
  assign("list_sensi",list.files("./Table/",pattern = paste0("Sensitivity_indiv_param_",resource,"-limited")))
  list_sensi=list_sensi[-grep("recy",x = list_sensi)]
  
  d=tibble()
  
  for (fi in list_sensi){d_n=read.table(paste0("./Table/",fi),sep=";")
  
  d_n <- do.call(data.frame, # Replace Inf in data by NA
                 lapply(d_n,function(x) replace(x, is.infinite(x), NA)))
  
  d=rbind(d,d_n)  
  }
  
  
  d_reg=tibble()
  param_name=unlist(strsplit(list_sensi,"_"))[    grep(unlist(strsplit(list_sensi,"_")),pattern = ".csv")]
  param_name=gsub(".csv","",param_name)
  
  
  for (k in 1:length(param_name)){ #for each parameter
    
    d_param=d[((4*n_point)*(k-1)+1):(4*n_point*k),]
    d_param$beta_H=d_param$rP-d_param$eH*d_param$rH
    d_param=filter(d_param,beta_H>=0)
    
    for (nr in 1:nrow(stoichio_seq)){ #for each of the 4 values in the rP-rB space
      
      d_fil_nr=filter(d_param,rP==stoichio_seq$rP[nr],rB==stoichio_seq$rB[nr])
      
      
      #we computed the range of LRR over the range of parameter values explored
      range_T1=((range(d_fil_nr$PP1_T)[1]-range(d_fil_nr$PP1_T)[2]))/(diff(range(d_fil_nr[,which(colnames(d_fil_nr)==param_name[k])])))
      range_A2=((range(d_fil_nr$PP2_A)[1]-range(d_fil_nr$PP2_A)[2]))/(diff(range(d_fil_nr[,which(colnames(d_fil_nr)==param_name[k])])))
      range_A1=((range(d_fil_nr$PP1_A)[1]-range(d_fil_nr$PP1_A)[2]))/(diff(range(d_fil_nr[,which(colnames(d_fil_nr)==param_name[k])])))
      
      #to account for decreasing effects of parameters
      range_T1=ifelse(lm(as.formula(paste("PP1_T", paste(param_name[k]), sep="~")),data = d_fil_nr)$coefficients[2]<0,
                      -range_T1,range_T1)
      range_A2=ifelse(lm(as.formula(paste("PP2_A", paste(param_name[k]), sep="~")),data = d_fil_nr)$coefficients[2]<0,
                      -range_A2,range_A2)
      range_A1=ifelse(lm(as.formula(paste("PP1_A", paste(param_name[k]), sep="~")),data = d_fil_nr)$coefficients[2]<0,
                      -range_A1,range_A1)
      
      
      d_reg=rbind(d_reg,tibble(reg_PP1_A=range_A1,reg_PP1_T=range_T1,reg_PP2_A=range_A2,
                               rP=unique(d_param$rP),rB=unique(d_param$rB))%>%add_column(name_param=param_name[k]))
      
      
    }
    
  }
  d_reg=d_reg[-which(is.na(d_reg$reg_PP1_A)),] #for eH
  
  d_reg_mean=d_reg%>%group_by(., name_param)%>%
    
    summarise(., reg_PP1_A=mean(reg_PP1_A),reg_PP1_T=mean(reg_PP1_T),reg_PP2_A=mean(reg_PP2_A),.groups = "keep")%>%
    
    arrange(., name_param)
  
  if (resource=="N") {    d_reg_mean[1,2:4]=NA     }
  
  assign(paste0("p_",resource),ggplot(d_reg_mean%>% melt(., id.vars=1)%>%mutate(., variable=recode_factor(variable,
                                                                                                          reg_PP1_A="Aq. basal prod.",reg_PP1_T="Terr. basal prod.", 
                                                                                                          reg_PP2_A="Aq. sec. prod.")))+
           geom_point(aes(x=name_param,y=value,color=variable),shape=20,size=3,stroke = 1)+the_theme+
           scale_color_manual(values=c("#3FB500","#2EAEBF","#2B50A7"))+labs(x="Parameter",y="Average change in LRR",color="")+geom_hline(yintercept = 0,linetype=9))
  
  
  
  
  # Chack whether there is a link between the the length of parameter gradient and the change
  
  range_param=unlist(sapply(1:length(Sensitivity_tab[d_reg_mean$name_param]),function(x){
    return(abs(diff(range(Sensitivity_tab[d_reg_mean$name_param][[x]]))))
  }))
  
  d_reg_mean$Range_param=range_param
  
  d_reg_mean_melt=d_reg_mean%>% melt(., id.vars=c("name_param","Range_param"))%>%
    mutate(.,variable=recode_factor(variable,
                                    "reg_PP1_A"='Aq. basal prod.',"reg_PP1_T"="Terr. basal prod.","reg_PP2_A" = "Aq. sec. prod."))
  
  d_range_all=rbind(d_range_all,d_reg_mean_melt%>%add_column(Scena=resource))
}

#ploting the results
p_slope=ggarrange(p_C+labs(x=""),p_N,nrow=2,common.legend = T,legend = "bottom",
                                 labels=LETTERS[1:2], font.label = list(size = 19),
                                 hjust=-2)
ggsave("./Figures/SI/Slopes_variation.pdf",p_slope,width = 7,height = 6)




n_point=15
for (resource in c("N","C")){
  
  assign("list_sensi",list.files("./Table/",pattern = paste0("Sensitivity_indiv_param_",resource,"-limited")))
  list_sensi=list_sensi[-grep("recy",x = list_sensi)]
  
  d=tibble()
  
  for (fi in list_sensi){d_n=read.table(paste0("./Table/",fi),sep=";")
  
  d_n <- do.call(data.frame, # Replace Inf in data by NA
                 lapply(d_n,function(x) replace(x, is.infinite(x), NA)))
  
  d=rbind(d,d_n)  
  }
  
  
  d_all=tibble()
  param_name=unlist(strsplit(list_sensi,"_"))[    grep(unlist(strsplit(list_sensi,"_")),pattern = ".csv")]
  param_name=gsub(".csv","",param_name)
  
  for (k in 1:length(param_name)){
    
    d_param=d[((4*n_point)*(k-1)+1):(4*n_point*k),]
    d_param$beta_H=d_param$rP-d_param$eH*d_param$rH
    d_param=filter(d_param,beta_H>=0)
    
    
    d_all=rbind(d_all,tibble(PP1_A=d_param$PP1_A,PP1_T=d_param$PP1_T,PP2_A=d_param$PP2_A,
                             PP2_T=d_param$PP2_T,Value=d_param[,param_name[k]],
                             rP=d_param$rP,rB=d_param$rB)%>%add_column(name_param=param_name[k]))
  }
  
  
  param=Get_classical_param(scena = paste0(resource,"-limited"))
  
  d2_melt=melt(d_all , measure.vars = c(paste0("PP1_A"),paste0("PP1_T"),paste0("PP2_A")))%>%
    mutate(., variable=recode_factor(variable,"PP1_A"="Aq. basal prod.","PP1_T"="Terr. basal prod.","PP2_A"="Aq. sec. prod."))
  for (k in c("aP","aH","dH","eH")){
    if (resource=="N")    list_name_prod=c("eB","dB","IDa","lDa","ma",'aC')
    if (resource=="C")    list_name_prod=c("eB","dB","IDa","lDa","aBD","ma")
    
    
    if (k %in% list_name_prod){
      
      dplot=filter(d2_melt,name_param==k)
      assign(paste0("p_",k),ggplot(dplot)+geom_line(aes(x=Value,y=value,color=interaction(rP,rB)))+the_theme+scale_color_viridis(discrete = T)+
               labs(y="LRR",x=k,color="rP, rB")+facet_wrap(.~variable,ncol=4,scales = "free")+
               theme(strip.background=element_rect(colour="transparent",fill="transparent"),strip.text.x = element_text(size=12))+
               geom_vline(xintercept = param[[k]],color="gray50",lwd=.1))
    } else{
      
      dplot=filter(d2_melt,name_param==k)%>%
        mutate(.,variable=recode_factor(variable,
                                        "Basal prod. aq."='',"Basal prod. terr."="  ","Cons. prod. aq." = "   "))
      assign(paste0("p_",k),ggplot(dplot)+geom_line(aes(x=Value,y=value,color=interaction(rP,rB)))+the_theme+scale_color_viridis(discrete = T)+
               labs(y="Production",x=k,color="rP, rB")+facet_wrap(.~variable,ncol=4,scales = "free")+
               theme(strip.background=element_rect(colour="transparent",fill="transparent"),strip.text.x = element_text(size=12))+
               geom_vline(xintercept = param[[k]],color="gray50",lwd=.1))
    }
  }
  
  
  p_tot=ggarrange(p_aP,
                       p_aH+theme(strip.text.x = element_blank()),
                       p_dH+theme(strip.text.x = element_blank()),
                       p_eH+theme(strip.text.x = element_blank()),
                       nrow=4,common.legend = T,legend = "bottom")
  
  ggsave(paste0("./Figures/SI/Sensibility_all_param_",resource,"_limited.pdf"),p_tot,width = 8,height = 9)
  
  
  
}


