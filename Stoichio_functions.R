packages=c("tidyverse", "ggpubr", "latex2exp", "deSolve", "reshape2", "igraph","ggforce",
               "JuliaCall", "diffeqr", "rethinking","phaseR","ggtext","viridis","rootSolve",
               "ggquiver", "scales","boot","RColorBrewer","ggnewscale")

#install pacakges if not installed already
install.packages(setdiff(packages, rownames(installed.packages())))


x = c("tidyverse", "ggpubr", "latex2exp", "deSolve", "reshape2", "igraph","ggforce",
      "JuliaCall", "diffeqr", "rethinking","phaseR","ggtext","viridis","rootSolve",
      "ggquiver", "scales","boot","RColorBrewer","ggnewscale")

lapply(x, require, character.only = TRUE)

julia_setup()
de = diffeq_setup()

julia_library("DifferentialEquations")

the_theme=theme_classic()+theme(legend.position = "bottom",
                                strip.background = element_rect(fill = "#CCE8D8"),
                                strip.text.y = element_text(size = 10, angle = -90),
                                strip.text.x = element_text(size = 8),
                                legend.text = element_text(size = 10),text = element_text(family = "NewCenturySchoolbook"))


pal=colorRampPalette(c("#5C7ECC","#32C4E2","#7DCE9A","#B4CE7D","#EFDD35","#FFB700"))
pal2=colorRampPalette(c("#A77E00","#83BD6B","white","#FFCD33","#EFDD35","#239B4C"))
pal2=colorRampPalette(c("#A77E00","#83BD6B","white","#FFCD33","#EFDD35","#239B4C"))
pal_effects=c("Direct"=brewer.pal(4,"Set1")[2],"Indirect"=brewer.pal(4,"Set1")[3],"Total"=brewer.pal(4,"Set1")[4])
pal_aqua=colorRampPalette(rev(c("#010169","#0A5BCE","#538CDC","#92BDF9","#BFD7F9")))
pal_terr=colorRampPalette(rev(c("#023C0F","#016B19","#479C4D","#74B178","#B1D6B4")))

colors = c("Consumers" = "darkorange", "Producers" = "green3", "Nitrogen" = "darkorchid2","Detritus"="brown")#,"top_predator"="red")
colors_ecosystem=c('Aquatic'='#90D0E0','Terrestrial'="#7AA969");color_resources=c('N'='#90D0E0','C'="brown")

## Creating folders

dir.create("./Figures/",showWarnings = FALSE)
dir.create("./Figures/SI",showWarnings = FALSE)
dir.create("./Table/",showWarnings = FALSE)




# 1 ---> Main functions ----
## A) Parameters and dynamics related function ----
### Parameters ----

eval_string = function(string, ...) eval(parse(text = string), ...)


Get_param_metaecosyst = function(INt,IDt,lNt,lDt,mt,eH,aH,aP,dH,dP, #local terrestrial ecosystem
                                  INa,IDa,lNa,lDa,ma,eC,eB,aC,aBN,aBD,dC,dB, #local aquatic ecosystem
                                  pH,pC,pP,pB, #spatial flow
                                  rH,rC,rP,rB) { #stoichiometry 
  return(
    list(
      
      #local green ecosystem
      INt = INt,  # Inflow of nitrogen in green ecosystem 
      IDt = IDt,  # Inflow of detritus in green ecosystem 
      lNt = lNt,  # outflow of nitrogen in green ecosystem
      lDt = lDt,  # outflow of detritus in green ecosystem
      mt  = mt ,  # mineralization rate in terrestrial ecosystem
      eH  = eH ,  # herbivore efficiency
      aH  = aH ,  # herbivore attack rate
      aP  = aP ,  # plant attack rate
      dH  = dH ,  # herbivore death rate
      dP  = dP ,  # plants death rate
      
      #local brown ecosystem
      INa = INa,  # Inflow of nitrogen in brown ecosystem 
      IDa = IDa,  # Inflow of detritus in brown ecosystem 
      lNa = lNa,  # outflow of nitrogen in brown ecosystem
      lDa = lDa,  # outflow of detritus in brown ecosystem
      ma  = ma ,  # mineralization rate in aquatic ecosystem
      eC  = eC ,  # consumers efficiency
      eB  = eB ,  # decomposers efficiency
      aC  = aC ,  # consumers attack rate
      aBN = aBN,  # decomposers attack rate on nitrogen
      aBD = aBD,  # decomposers attack rate on detritus
      dC  = dC ,  # consumers death rate
      dB  = dB ,  # plants death rate
      
      #spatial flow
      pH = pH,    # fraction of subsidies from herbivores recycled regionally
      pC = pC,    # fraction of subsidies from consumers recycled regionally
      pP = pP,    # fraction of subsidies from plants recycled regionally
      pB = pB,    # fraction of subsidies from decomposers recycled regionally
      
      #stoichiometry
      rP = rP,    # N:C ratio of plants
      rH = rH,    # N:C ratio of herbivores
      rC = rC,    # N:C ratio of consumers
      rB = rB     # N:C ratio of decomposers

    )
  )
}


Get_classical_param=function(coupling=F,consumers=T,scena="C-limited"){ 
  
  #We take 3 different scenarios 
  `%!in%` = Negate(`%in%`)
  
  
  if (scena=="C-limited"){
    
    param=Get_param_metaecosyst(
      #local green ecosystem
      INt = 7  , 
      IDt = 7  , 
      lNt = 1  , 
      lDt = 1  , 
      mt  = .1,
      eH  = .25 , 
      aH  = .2 , 
      aP  = .34, 
      dH  = .1 , 
      dP  = .1 , 
      
      #local brown ecosystem
      INa = 7  , 
      IDa = 7  , 
      lNa = 1  ,
      lDa = 1  ,
      ma  = .5 ,
      eC  = .5 ,
      eB  = .5 ,
      aC  = .3 ,
      aBN = 1 ,
      aBD = .83  ,
      dC  = .1 ,
      dB  = .1 ,   
      
      #spatial flow
      pH = 0   ,
      pC = 0   ,
      pP = 0   ,
      pB = 0   ,
      
      #stoichiometry
      rP = 1/18 , 
      rH = 1/10,
      rC = 1/10,
      rB = 0.15
    )
  }
  if (scena=="Switch-C-N"){
    
    param=Get_param_metaecosyst(
      #local green ecosystem
      INt = 7  , 
      IDt = 7  , 
      lNt = 1  , 
      lDt = 1  , 
      mt  = .1,
      eH  = .25 , 
      aH  = .2 , 
      aP  = .34, 
      dH  = .1 , 
      dP  = .1 , 
      
      #local brown ecosystem
      INa = 2.5  , 
      IDa = 10  ,
      lNa = 2  ,
      lDa = 1  ,
      ma  = .5 ,
      eC  = .5 ,
      eB  = .5 ,
      aC  = .3 ,
      aBN = 1 ,
      aBD = .83  ,
      dC  = .1 ,
      dB  = .1 ,   
      
      #spatial flow
      pH = 0   ,
      pC = 0   ,
      pP = 0   ,
      pB = 0   ,
      
      #stoichiometry
      rP = 1/18 , 
      rH = 1/10,
      rC = 1/10,
      rB = 0.15
    )
  }
  
  if (scena=="N-limited"){
    
    
    param=Get_param_metaecosyst(
      #local green ecosystem
      INt = 7  , 
      IDt = 7  , 
      lNt = 1  , 
      lDt = 1  , 
      mt  = .1,
      eH  = .25 , 
      aH  = .2 , 
      aP  = .34, 
      dH  = .1 , 
      dP  = .1 , 
      
      #local brown ecosystem
      INa = 2  ,
      IDa = 12  ,
      lNa = 1  ,
      lDa = 1  ,
      ma  = .5 ,
      eC  = .5 ,
      eB  = .5 ,
      aC  = .3 ,
      aBN = .25 ,
      aBD = .83  ,
      dC  = .1 ,
      dB  = .1 ,   
      
      #spatial flow
      pH = 0   ,
      pC = 0   ,
      pP = 0   ,
      pB = 0   ,
      
      #stoichiometry
      rP = 1/40 , 
      rH = 1/10,
      rC = 1/10,
      rB = 0.15
    )
    
  }
  
  if (scena=="Colimitation"){
    
    param=Get_param_metaecosyst(
      #local green ecosystem
      INt = 7  , 
      IDt = 7  , 
      lNt = 1  , 
      lDt = 1  , 
      mt  = .1,
      eH  = .25 , 
      aH  = .2 , 
      aP  = .34, 
      dH  = .1 , 
      dP  = .1 , 
      
      #local brown ecosystem
      INa = 7  ,
      IDa = 7  ,
      lNa = 1  ,
      lDa = 1  ,
      ma  = .5 ,
      eC  = .5 ,
      eB  = .5 ,
      aC  = .3 ,
      aBN = 1 ,
      aBD = .83  ,
      dC  = .1 ,
      dB  = .1 ,   
      
      #spatial flow
      pH = 0   ,
      pC = 0   ,
      pP = 0   ,
      pB = 0   ,
      
      #stoichiometry
      rP = 1/18 , 
      rH = 1/10,
      rC = 1/10,
      rB = 0.15
    )
  }
  
  if (coupling) {param[c("pP","pB","pC","pH")]=1}
  
  param$colim=ifelse(scena=="Colimitation",T,F)
  
  return(param)
}


Topconsum_param=function(list){
  
  list$dTC=list$dTH=.3
  list$aTH=.5
  list$aTC=.5
  list$eTH=.25
  list$eTC=.5
  list$rTH=list$rTC=.1
  list$pTH=list$pTC=0
  
  return(list)
}




Get_initial_values=function(param,rD_ini=.1,value_given=F,val=2){
  if (value_given){
    state=c("H"=val,"HN"=0,"C"=val,"CN"=0,"P"=val,"PN"=0,"B"=val,"BN"=0,"DCt"=val,"DNt"=0,"DCa"=val,"DNa"=0,"Nt"=val,"Na"=val)
  }else {state=c("H"=2,"HN"=0,"C"=2,"CN"=0,"P"=3,"PN"=0,"B"=2,"BN"=0,"DCt"=3,"DNt"=0,"DCa"=3,"DNa"=0,"Nt"=2,"Na"=2)}
  state[c("DNt")]=rD_ini*state[c("DCt")];state[c("DNa")]=rD_ini*state[c("DCa")];state["HN"]=param$rH*state["H"];state["CN"]=param$rC*state["C"];state["PN"]=param$rP*state["P"];
  state['BN']=state["B"]*param$rB
  
  return(state)
}

State_topconsum=function(state,param){
  
  state=c("TH"=2,"THN"=param$rTH*2,"TC"=2,"TCN"=param$rTC*2,state)
  return(state)
  
  
}

### Dynamics ----

ode_metaecosystem = julia_eval("

function ode_metaecosystem(du, u, p, t)

    u[u .< 1e-5] .= 0
    INt, IDt, lNt, lDt, mt, eH, aH, aP, dH, dP, INa, IDa, lNa, lDa, ma, eC, eB, aC, aBN, aBD, dC, dB, pH, pC, pP, pB, rP, rH, rC, rB, colim = p
    H, HN, C, CN, P, PN, B, BN, DCt, DNt, DCa, DNa, Nt, Na = u

    fH = copy(aH * P * H)
    fP = copy(aP * Nt * P)
    fC = copy(aC * B * C)
    fBN = copy(aBN * Na)
    fBD = copy(aBD * DCa)


    if colim == 0
        immo = copy(min(((rB - (DNa / DCa)) / rB) * eB * fBD, fBN))
    else
        immo = copy(((rB - (DNa / DCa)) / rB) * eB * fBD * fBN)
    end

    


    decompo = copy(immo / ((rB - (DNa / DCa)) / rB))

    du[1] = eH * fH - dH * H
    du[2] = rH * eH * fH - rH * dH * H

    du[3] = eC * fC - dC * C
    du[4] = rC * eC * fC - rC * dC * C

    du[5] = fP - fH - dP * P
    du[6] = rP * fP - rP * fH - rP * dP * P

    du[7] = decompo - fC - dB * B - ma * B
    du[8] = immo * rB + decompo * (DNa / DCa) - fC * rB - dB * B * rB - ma * B * rB


    du[9] = IDt - lDt * DCt +
            dH * H * (1 - pH) + dP * P * (1 - pP) - mt * DCt +
            dC * C * pC + dB * B * pB


    du[10] = IDt * (DNt / DCt) - lDt * DNt +
             rH * dH * H * (1 - pH) + rP * dP * P * (1 - pP) - mt * DNt +
             rC * dC * C * pC + rB * dB * B * pB

    du[11] = IDa - lDa * DCa +
             dC * C * (1 - pC) + dB * B * (1 - pB) - decompo +
             dH * H * pH + dP * P * pP


    du[12] = IDa * (DNa / DCa) - lDa * DNa +
             rC * dC * C * (1 - pC) + rB * dB * B * (1 - pB) - decompo * (DNa / DCa) +
             rH * dH * H * pH + rP * dP * P * pP




    du[13] = INt - lNt * Nt - rP * fP +
             (rP - eH * rH) * fH * (1 - pH) + (rB - eC * rC) * fC * pC +
             mt * DNt

    du[14] = INa - lNa * Na - immo * rB +
             (rB - eC * rC) * fC * (1 - pC) + ma * B * rB +
             (rP - eH * rH) * fH * pH


end

")




ode_metaecosystem_feedback_no_A_to_T = julia_eval("

function ode_metaecosystem_feedback_no_A_to_T(du, u, p, t)

    u[u .< 1e-5] .= 0
    INt, IDt, lNt, lDt, mt, eH, aH, aP, dH, dP, INa, IDa, lNa, lDa, ma, eC, eB, aC, aBN, aBD, dC, dB, pH, pC, pP, pB, rP, rH, rC, rB, colim = p
    H, HN, C, CN, P, PN, B, BN, DCt, DNt, DCa, DNa, Nt, Na = u

    fH = copy(aH * P * H)
    fP = copy(aP * Nt * P)
    fC = copy(aC * B * C)
    fBN = copy(aBN * Na)
    fBD = copy(aBD * DCa)


    if colim == 0
        immo = copy(min(((rB - (DNa / DCa)) / rB) * eB * fBD, fBN))
    else
        immo = copy(((rB - (DNa / DCa)) / rB) * eB * fBD * fBN)
    end




    decompo = copy(immo / ((rB - (DNa / DCa)) / rB))

    du[1] = eH * fH - dH * H
    du[2] = rH * eH * fH - rH * dH * H

    du[3] = eC * fC - dC * C
    du[4] = rC * eC * fC - rC * dC * C

    du[5] = fP - fH - dP * P
    du[6] = rP * fP - rP * fH - rP * dP * P

    du[7] = decompo - fC - dB * B - ma * B
    du[8] = immo * rB + decompo * (DNa / DCa) - fC * rB - dB * B * rB - ma * B * rB


    du[9] = IDt - lDt * DCt +
            dH * H * (1 - pH) + dP * P * (1 - pP) - mt * DCt 

    du[10] = IDt * (DNt / DCt) - lDt * DNt +
             rH * dH * H * (1 - pH) + rP * dP * P * (1 - pP) - mt * DNt 
             
    du[11] = IDa - lDa * DCa +
             dC * C * (1 - pC) + dB * B * (1 - pB) - decompo +
             dH * H * pH + dP * P * pP


    du[12] = IDa * (DNa / DCa) - lDa * DNa +
             rC * dC * C * (1 - pC) + rB * dB * B * (1 - pB) - decompo * (DNa / DCa) +
             rH * dH * H * pH + rP * dP * P * pP




    du[13] = INt - lNt * Nt - rP * fP +
             (rP - eH * rH) * fH * (1 - pH) +
             mt * DNt

    du[14] = INa - lNa * Na - immo * rB +
             (rB - eC * rC) * fC * (1 - pC) + ma * B * rB +
             (rP - eH * rH) * fH * pH


end

")



ode_metaecosystem_feedback_no_A_to_T_DC = julia_eval("

function ode_metaecosystem_feedback_no_A_to_T_DC(du, u, p, t)

    u[u .< 1e-5] .= 0
    INt, IDt, lNt, lDt, mt, eH, aH, aP, dH, dP, INa, IDa, lNa, lDa, ma, eC, eB, aC, aBN, aBD, dC, dB, pH, pC, pP, pB, rP, rH, rC, rB, colim = p
    H, HN, C, CN, P, PN, B, BN, DCt, DNt, DCa, DNa, Nt, Na = u

    fH = copy(aH * P )
    fP = copy(aP * Nt)
    fC = copy(aC * B)
    fBN = copy(aBN * Na)
    fBD = copy(aBD * DCa)


    if colim == 0
        immo = copy(min(((rB - (DNa / DCa)) / rB) * eB * fBD, fBN))
    else
        immo = copy(((rB - (DNa / DCa)) / rB) * eB * fBD * fBN)
    end




    decompo = copy(immo / ((rB - (DNa / DCa)) / rB))

    du[1] = eH * fH - dH * H
    du[2] = rH * eH * fH - rH * dH * H

    du[3] = eC * fC - dC * C
    du[4] = rC * eC * fC - rC * dC * C

    du[5] = fP - fH - dP * P
    du[6] = rP * fP - rP * fH - rP * dP * P

    du[7] = decompo - fC - dB * B - ma * B
    du[8] = immo * rB + decompo * (DNa / DCa) - fC * rB - dB * B * rB - ma * B * rB


    du[9] = IDt - lDt * DCt +
            dH * H * (1 - pH) + dP * P * (1 - pP) - mt * DCt 

    du[10] = IDt * (DNt / DCt) - lDt * DNt +
             rH * dH * H * (1 - pH) + rP * dP * P * (1 - pP) - mt * DNt 
             
    du[11] = IDa - lDa * DCa +
             dC * C * (1 - pC) + dB * B * (1 - pB) - decompo +
             dH * H * pH + dP * P * pP


    du[12] = IDa * (DNa / DCa) - lDa * DNa +
             rC * dC * C * (1 - pC) + rB * dB * B * (1 - pB) - decompo * (DNa / DCa) +
             rH * dH * H * pH + rP * dP * P * pP




    du[13] = INt - lNt * Nt - rP * fP +
             (rP - eH * rH) * fH * (1 - pH) +
             mt * DNt

    du[14] = INa - lNa * Na - immo * rB +
             (rB - eC * rC) * fC * (1 - pC) + ma * B * rB +
             (rP - eH * rH) * fH * pH


end

")

ode_metaecosystem_feedback_no_T_to_A = julia_eval("

function ode_metaecosystem_feedback_no_T_to_A(du, u, p, t)

    u[u .< 1e-5] .= 0
    INt, IDt, lNt, lDt, mt, eH, aH, aP, dH, dP, INa, IDa, lNa, lDa, ma, eC, eB, aC, aBN, aBD, dC, dB, pH, pC, pP, pB, rP, rH, rC, rB, colim = p
    H, HN, C, CN, P, PN, B, BN, DCt, DNt, DCa, DNa, Nt, Na = u

    fH = copy(aH * P * H)
    fP = copy(aP * Nt * P)
    fC = copy(aC * B * C)
    fBN = copy(aBN * Na)
    fBD = copy(aBD * DCa)


    if colim == 0
        immo = copy(min(((rB - (DNa / DCa)) / rB) * eB * fBD, fBN))
    else
        immo = copy(((rB - (DNa / DCa)) / rB) * eB * fBD * fBN)
    end




    decompo = copy(immo / ((rB - (DNa / DCa)) / rB))

    du[1] = eH * fH - dH * H
    du[2] = rH * eH * fH - rH * dH * H

    du[3] = eC * fC - dC * C
    du[4] = rC * eC * fC - rC * dC * C

    du[5] = fP - fH - dP * P
    du[6] = rP * fP - rP * fH - rP * dP * P

    du[7] = decompo - fC - dB * B - ma * B
    du[8] = immo * rB + decompo * (DNa / DCa) - fC * rB - dB * B * rB - ma * B * rB


    du[9] = IDt - lDt * DCt +
            dH * H * (1 - pH) + dP * P * (1 - pP) - mt * DCt +
            dC * C * pC + dB * B * pB


    du[10] = IDt * (DNt / DCt) - lDt * DNt +
             rH * dH * H * (1 - pH) + rP * dP * P * (1 - pP) - mt * DNt +
             rC * dC * C * pC + rB * dB * B * pB

    du[11] = IDa - lDa * DCa +
             dC * C * (1 - pC) + dB * B * (1 - pB) - decompo 


    du[12] = IDa * (DNa / DCa) - lDa * DNa +
             rC * dC * C * (1 - pC) + rB * dB * B * (1 - pB) - decompo * (DNa / DCa) 




    du[13] = INt - lNt * Nt - rP * fP +
             (rP - eH * rH) * fH * (1 - pH) + (rB - eC * rC) * fC * pC +
             mt * DNt

    du[14] = INa - lNa * Na - immo * rB +
             (rB - eC * rC) * fC * (1 - pC) + ma * B * rB


end

")


ode_metaecosystem_feedback_no_T_to_A_DC = julia_eval("

function ode_metaecosystem_feedback_no_T_to_A_DC(du, u, p, t)

    u[u .< 1e-5] .= 0
    INt, IDt, lNt, lDt, mt, eH, aH, aP, dH, dP, INa, IDa, lNa, lDa, ma, eC, eB, aC, aBN, aBD, dC, dB, pH, pC, pP, pB, rP, rH, rC, rB, colim = p
    H, HN, C, CN, P, PN, B, BN, DCt, DNt, DCa, DNa, Nt, Na = u

    fH = copy(aH * P )
    fP = copy(aP * Nt)
    fC = copy(aC * B)
    fBN = copy(aBN * Na)
    fBD = copy(aBD * DCa)


    if colim == 0
        immo = copy(min(((rB - (DNa / DCa)) / rB) * eB * fBD, fBN))
    else
        immo = copy(((rB - (DNa / DCa)) / rB) * eB * fBD * fBN)
    end




    decompo = copy(immo / ((rB - (DNa / DCa)) / rB))

    du[1] = eH * fH - dH * H
    du[2] = rH * eH * fH - rH * dH * H

    du[3] = eC * fC - dC * C
    du[4] = rC * eC * fC - rC * dC * C

    du[5] = fP - fH - dP * P
    du[6] = rP * fP - rP * fH - rP * dP * P

    du[7] = decompo - fC - dB * B - ma * B
    du[8] = immo * rB + decompo * (DNa / DCa) - fC * rB - dB * B * rB - ma * B * rB


    du[9] = IDt - lDt * DCt +
            dH * H * (1 - pH) + dP * P * (1 - pP) - mt * DCt +
            dC * C * pC + dB * B * pB


    du[10] = IDt * (DNt / DCt) - lDt * DNt +
             rH * dH * H * (1 - pH) + rP * dP * P * (1 - pP) - mt * DNt +
             rC * dC * C * pC + rB * dB * B * pB

    du[11] = IDa - lDa * DCa +
             dC * C * (1 - pC) + dB * B * (1 - pB) - decompo 


    du[12] = IDa * (DNa / DCa) - lDa * DNa +
             rC * dC * C * (1 - pC) + rB * dB * B * (1 - pB) - decompo * (DNa / DCa) 




    du[13] = INt - lNt * Nt - rP * fP +
             (rP - eH * rH) * fH * (1 - pH) + (rB - eC * rC) * fC * pC +
             mt * DNt

    du[14] = INa - lNa * Na - immo * rB +
             (rB - eC * rC) * fC * (1 - pC) + ma * B * rB


end

")



ode_metaecosystem_DC = julia_eval("

function ode_metaecosystem_DC(du, u, p, t)

    u[u .< 1e-5] .= 0
    INt, IDt, lNt, lDt, mt, eH, aH, aP, dH, dP, INa, IDa, lNa, lDa, ma, eC, eB, aC, aBN, aBD, dC, dB, pH, pC, pP, pB, rP, rH, rC, rB, colim = p
    H, HN, C, CN, P, PN, B, BN, DCt, DNt, DCa, DNa, Nt, Na = u

    fH = copy(aH * P )
    fP = copy(aP * Nt)
    fC = copy(aC * B)
    fBN = copy(aBN * Na)
    fBD = copy(aBD * DCa)


    if colim == 0
        immo = copy(min(((rB - (DNa / DCa)) / rB) * eB * fBD, fBN))
    else
        immo = copy(((rB - (DNa / DCa)) / rB) * eB * fBD * fBN)
    end




    decompo = copy(immo / ((rB - (DNa / DCa)) / rB))

    du[1] = eH * fH - dH * H
    du[2] = rH * eH * fH - rH * dH * H

    du[3] = eC * fC - dC * C
    du[4] = rC * eC * fC - rC * dC * C

    du[5] = fP - fH - dP * P
    du[6] = rP * fP - rP * fH - rP * dP * P

    du[7] = decompo - fC - dB * B - ma * B
    du[8] = immo * rB + decompo * (DNa / DCa) - fC * rB - dB * B * rB - ma * B * rB


    du[9] = IDt - lDt * DCt +
            dH * H * (1 - pH) + dP * P * (1 - pP) - mt * DCt +
            dC * C * pC + dB * B * pB


    du[10] = IDt * (DNt / DCt) - lDt * DNt +
             rH * dH * H * (1 - pH) + rP * dP * P * (1 - pP) - mt * DNt +
             rC * dC * C * pC + rB * dB * B * pB

    du[11] = IDa - lDa * DCa +
             dC * C * (1 - pC) + dB * B * (1 - pB) - decompo +
             dH * H * pH + dP * P * pP


    du[12] = IDa * (DNa / DCa) - lDa * DNa +
             rC * dC * C * (1 - pC) + rB * dB * B * (1 - pB) - decompo * (DNa / DCa) +
             rH * dH * H * pH + rP * dP * P * pP




    du[13] = INt - lNt * Nt - rP * fP +
             (rP - eH * rH) * fH * (1 - pH) + (rB - eC * rC) * fC * pC +
             mt * DNt

    du[14] = INa - lNa * Na - immo * rB +
             (rB - eC * rC) * fC * (1 - pC) + ma * B * rB +
             (rP - eH * rH) * fH * pH


end

")






ode_metaecosystem_Topconsumers = julia_eval("


function ode_metaecosystem_Topconsumers(du, u, p, t)

    u[u .< 1e-5] .= 0
    INt, IDt, lNt, lDt, mt, eH, aH, aP, dH, dP, INa, IDa, lNa, lDa, ma, eC, eB, aC,
    aBN, aBD, dC, dB, pH, pC, pP, pB, rP, rH, rC, rB,
    colim, dTH, dTC, aTH, aTC, eTH, eTC, rTC, rTH, pTC, pTH = p
    
    TH, THN, TC, TCN, H, HN, C, CN, P, PN, B, BN, DCt, DNt, DCa, DNa, Nt, Na = u

    fTH = copy(aTH * TH * H)
    fTC = copy(aTC * TC * C)
    fH = copy(aH * P * H)
    fP = copy(aP * Nt * P)
    fC = copy(aC * B * C)
    fBN = copy(aBN * Na)
    fBD = copy(aBD * DCa)


    if colim == 0
        immo = copy(min(((rB - (DNa / DCa)) / rB) * eB * fBD, fBN))
    else
        immo = copy(((rB - (DNa / DCa)) / rB) * eB * fBD * fBN)
    end

    decompo = copy(immo / ((rB - (DNa / DCa)) / rB))


    du[1] = eTH * fTH - dTH * TH
    du[2] = (eTH * fTH - dTH * TH) * rTH

    du[3] = eTC * fTC - dTC * TC
    du[4] = (eTC * fTC - dTC * TC) * rTC

    du[5] = eH * fH - dH * H - fTH
    du[6] = rH * eH * fH - rH * dH * H - rH * fTH

    du[7] = eC * fC - dC * C - fTC
    du[8] = rC * eC * fC - rC * dC * C - rC * fTC

    du[9] = fP - fH - dP * P
    du[10] = rP * fP - rP * fH - rP * dP * P

    du[11] = decompo - fC - dB * B - ma * B
    du[12] = immo * rB + decompo * (DNa / DCa) - fC * rB - dB * B * rB - ma * B * rB


    du[13] = IDt - lDt * DCt +
             dH * H * (1 - pH) + dP * P * (1 - pP) + dTH * TH * (1 - pTH) - mt * DCt +
             dC * C * pC + dB * B * pB + dTC * TC * pTC


    du[14] = IDt * (DNt / DCt) - lDt * DNt +
             rH * dH * H * (1 - pH) + rP * dP * P * (1 - pP) + rTH * dTH * TH * (1 - pTH) - mt * DNt +
             rC * dC * C * pC + rB * dB * B * pB + rTC * dTC * TC * pTC

    du[15] = IDa - lDa * DCa +
             dC * C * (1 - pC) + dB * B * (1 - pB) + dTC * TC * (1 - pTC) - decompo +
             dH * H * pH + dP * P * pP + dTH * TH * pTH


    du[16] = IDa * (DNa / DCa) - lDa * DNa +
             rC * dC * C * (1 - pC) + rB * dB * B * (1 - pB) + rTC * dTC * TC * (1 - pTC) - decompo * (DNa / DCa) +
             rH * dH * H * pH + rP * dP * P * pP + rTH * dTH * TH * pTH



    du[17] = INt - lNt * Nt - rP * fP +
             (rP - eH * rH) * fH * (1 - pH) + (rH - eTH * rTH) * fTH * (1 - pTH) +
             (rB - eC * rC) * fC * pC + (rC - eTC * rTC) * fTC * pTC +
             mt * DNt

    du[18] = INa - lNa * Na - immo * rB +
             (rB - eC * rC) * fC * (1 - pC) + (rC - eTC * rTC) * fTC * (1 - pTC) + ma * B * rB +
             (rP - eH * rH) * fH * pH + (rH - eTH * rTH) * fTH * pTH



end

")


ode_metaecosystem_feedback_no_A_to_T_topconsumers = julia_eval("


function ode_metaecosystem_feedback_no_A_to_T_topconsumers(du, u, p, t)

    u[u .< 1e-5] .= 0
    INt, IDt, lNt, lDt, mt, eH, aH, aP, dH, dP, INa, IDa, lNa, lDa, ma, eC, eB, aC,
    aBN, aBD, dC, dB, pH, pC, pP, pB, rP, rH, rC, rB,
    colim, dTH, dTC, aTH, aTC, eTH, eTC, rTC, rTH, pTC, pTH = p
    
    TH, THN, TC, TCN, H, HN, C, CN, P, PN, B, BN, DCt, DNt, DCa, DNa, Nt, Na = u

    fTH = copy(aTH * TH * H)
    fTC = copy(aTC * TC * C)
    fH = copy(aH * P * H)
    fP = copy(aP * Nt * P)
    fC = copy(aC * B * C)
    fBN = copy(aBN * Na)
    fBD = copy(aBD * DCa)


    if colim == 0
        immo = copy(min(((rB - (DNa / DCa)) / rB) * eB * fBD, fBN))
    else
        immo = copy(((rB - (DNa / DCa)) / rB) * eB * fBD * fBN)
    end

    decompo = copy(immo / ((rB - (DNa / DCa)) / rB))


    du[1] = eTH * fTH - dTH * TH
    du[2] = (eTH * fTH - dTH * TH) * rTH

    du[3] = eTC * fTC - dTC * TC
    du[4] = (eTC * fTC - dTC * TC) * rTC

    du[5] = eH * fH - dH * H - fTH
    du[6] = rH * eH * fH - rH * dH * H - rH * fTH

    du[7] = eC * fC - dC * C - fTC
    du[8] = rC * eC * fC - rC * dC * C - rC * fTC

    du[9] = fP - fH - dP * P
    du[10] = rP * fP - rP * fH - rP * dP * P

    du[11] = decompo - fC - dB * B - ma * B
    du[12] = immo * rB + decompo * (DNa / DCa) - fC * rB - dB * B * rB - ma * B * rB


    du[13] = IDt - lDt * DCt +
             dH * H * (1 - pH) + dP * P * (1 - pP) + dTH * TH * (1 - pTH) - mt * DCt 

    du[14] = IDt * (DNt / DCt) - lDt * DNt +
             rH * dH * H * (1 - pH) + rP * dP * P * (1 - pP) + rTH * dTH * TH * (1 - pTH) - mt * DNt 
    du[15] = IDa - lDa * DCa +
             dC * C * (1 - pC) + dB * B * (1 - pB) + dTC * TC * (1 - pTC) - decompo +
             dH * H * pH + dP * P * pP + dTH * TH * pTH


    du[16] = IDa * (DNa / DCa) - lDa * DNa +
             rC * dC * C * (1 - pC) + rB * dB * B * (1 - pB) + rTC * dTC * TC * (1 - pTC) - decompo * (DNa / DCa) +
             rH * dH * H * pH + rP * dP * P * pP + rTH * dTH * TH * pTH



    du[17] = INt - lNt * Nt - rP * fP +
             (rP - eH * rH) * fH * (1 - pH) + (rH - eTH * rTH) * fTH * (1 - pTH) +
             mt * DNt

    du[18] = INa - lNa * Na - immo * rB +
             (rB - eC * rC) * fC * (1 - pC) + (rC - eTC * rTC) * fTC * (1 - pTC) + ma * B * rB +
             (rP - eH * rH) * fH * pH + (rH - eTH * rTH) * fTH * pTH



end

")


ode_metaecosystem_feedback_no_T_to_A_topconsumers = julia_eval("


function ode_metaecosystem_feedback_no_T_to_A_topconsumers(du, u, p, t)

    u[u .< 1e-5] .= 0
    INt, IDt, lNt, lDt, mt, eH, aH, aP, dH, dP, INa, IDa, lNa, lDa, ma, eC, eB, aC,
    aBN, aBD, dC, dB, pH, pC, pP, pB, rP, rH, rC, rB,
    colim, dTH, dTC, aTH, aTC, eTH, eTC, rTC, rTH, pTC, pTH = p
    
    TH, THN, TC, TCN, H, HN, C, CN, P, PN, B, BN, DCt, DNt, DCa, DNa, Nt, Na = u

    fTH = copy(aTH * TH * H)
    fTC = copy(aTC * TC * C)
    fH = copy(aH * P * H)
    fP = copy(aP * Nt * P)
    fC = copy(aC * B * C)
    fBN = copy(aBN * Na)
    fBD = copy(aBD * DCa)


    if colim == 0
        immo = copy(min(((rB - (DNa / DCa)) / rB) * eB * fBD, fBN))
    else
        immo = copy(((rB - (DNa / DCa)) / rB) * eB * fBD * fBN)
    end

    decompo = copy(immo / ((rB - (DNa / DCa)) / rB))


    du[1] = eTH * fTH - dTH * TH
    du[2] = (eTH * fTH - dTH * TH) * rTH

    du[3] = eTC * fTC - dTC * TC
    du[4] = (eTC * fTC - dTC * TC) * rTC

    du[5] = eH * fH - dH * H - fTH
    du[6] = rH * eH * fH - rH * dH * H - rH * fTH

    du[7] = eC * fC - dC * C - fTC
    du[8] = rC * eC * fC - rC * dC * C - rC * fTC

    du[9] = fP - fH - dP * P
    du[10] = rP * fP - rP * fH - rP * dP * P

    du[11] = decompo - fC - dB * B - ma * B
    du[12] = immo * rB + decompo * (DNa / DCa) - fC * rB - dB * B * rB - ma * B * rB


    du[13] = IDt - lDt * DCt +
             dH * H * (1 - pH) + dP * P * (1 - pP) + dTH * TH * (1 - pTH) - mt * DCt +
             dC * C * pC + dB * B * pB + dTC * TC * pTC


    du[14] = IDt * (DNt / DCt) - lDt * DNt +
             rH * dH * H * (1 - pH) + rP * dP * P * (1 - pP) + rTH * dTH * TH * (1 - pTH) - mt * DNt +
             rC * dC * C * pC + rB * dB * B * pB + rTC * dTC * TC * pTC

    du[15] = IDa - lDa * DCa +
             dC * C * (1 - pC) + dB * B * (1 - pB) + dTC * TC * (1 - pTC) - decompo 

    du[16] = IDa * (DNa / DCa) - lDa * DNa +
             rC * dC * C * (1 - pC) + rB * dB * B * (1 - pB) + rTC * dTC * TC * (1 - pTC) - decompo * (DNa / DCa) 


    du[17] = INt - lNt * Nt - rP * fP +
             (rP - eH * rH) * fH * (1 - pH) + (rH - eTH * rTH) * fTH * (1 - pTH) +
             (rB - eC * rC) * fC * pC + (rC - eTC * rTC) * fTC * pTC +
             mt * DNt

    du[18] = INa - lNa * Na - immo * rB +
             (rB - eC * rC) * fC * (1 - pC) + (rC - eTC * rTC) * fTC * (1 - pTC) + ma * B * rB 


end

")

ode_metaecosystem_flexible_stoichio = julia_eval("

function ode_metaecosystem_flexible_stoichio(du, u, p, t)

    u[u .< 1e-5] .= 0
    INt, IDt, lNt, lDt, mt, eH, aH, aP, dH, dP, INa, IDa, lNa, lDa, ma, eC, eB, aC, aBN, aBD, dC, dB, pH, pC, pP, pB, rP, rH, rC, rB, colim, KN,Nstar = p
    H, HN, C, CN, P, PN, B, BN, DCt, DNt, DCa, DNa, Nt, Na = u

    fH = copy(aH * P * H)
    fP = copy(aP * Nt * P)
    fC = copy(aC * B * C)
    fBN = copy(aBN * Na)
    fBD = copy(aBD * DCa)

    if colim == 0
        immo = copy(min(((rB - (DNa / DCa)) / rB) * eB * fBD, fBN))
    else
        immo = copy(((rB - (DNa / DCa)) / rB) * eB * fBD * fBN)
    end

    alpha_P=copy((((.1-0.025)*(Nt-Nstar))/(KN+(Nt-Nstar)))+.025)
    
    println(alpha_P)
    
    decompo = copy(immo / ((rB - (DNa / DCa)) / rB))

    du[1] = eH * fH - dH * H
    du[2] = rH * eH * fH - rH * dH * H

    du[3] = eC * fC - dC * C
    du[4] = rC * eC * fC - rC * dC * C

    du[5] = fP - fH - dP * P
    du[6] = alpha_P * du[5]

    du[7] = decompo - fC - dB * B - ma * B
    du[8] = immo * rB + decompo * (DNa / DCa) - fC * rB - dB * B * rB - ma * B * rB


    du[9] = IDt - lDt * DCt +
            dH * H * (1 - pH) + dP * P * (1 - pP) - mt * DCt +
            dC * C * pC + dB * B * pB


    du[10] = IDt * (DNt / DCt) - lDt * DNt +
             rH * dH * H * (1 - pH) + alpha_P * dP * P * (1 - pP) - mt * DNt +
             rC * dC * C * pC + rB * dB * B * pB

    du[11] = IDa - lDa * DCa +
             dC * C * (1 - pC) + dB * B * (1 - pB) - decompo +
             dH * H * pH + dP * P * pP


    du[12] = IDa * (DNa / DCa) - lDa * DNa +
             rC * dC * C * (1 - pC) + rB * dB * B * (1 - pB) - decompo * (DNa / DCa) +
             rH * dH * H * pH + alpha_P * dP * P * pP




    du[13] = INt - lNt * Nt - alpha_P * fP +
             (alpha_P - eH * rH) * fH * (1 - pH) + (rB - eC * rC) * fC * pC +
             mt * DNt

    du[14] = INa - lNa * Na - immo * rB +
             (rB - eC * rC) * fC * (1 - pC) + ma * B * rB +
             (alpha_P - eH * rH) * fH * pH


end

")





Compute_ode=function(state,param,TRESH=1e-5,method_ode="lsoda",
                     consumers=T,n_time=10000,type_ode="full"){
  
  julia_assign("state", state)
  julia_assign("p", unlist(param))
  
  
  
  tspan = c(0, n_time) #to avoid long transient
  julia_assign("tspan", tspan)
  
  if (type_ode=="full"){
    prob = julia_eval("ODEProblem(ode_metaecosystem, state, tspan, p)")
  } else if (type_ode=="feedback_T"){
    prob = julia_eval("ODEProblem(ode_metaecosystem_feedback_no_T_to_A, state, tspan, p)")
  } else if (type_ode=="feedback_A"){
    prob = julia_eval("ODEProblem(ode_metaecosystem_feedback_no_A_to_T, state, tspan, p)")
  } else if (type_ode=="topconsum"){
    prob = julia_eval("ODEProblem(ode_metaecosystem_Topconsumers, state, tspan, p)")
  } else if (type_ode=="DC"){
    prob = julia_eval("ODEProblem(ode_metaecosystem_DC, state, tspan, p)")
  } else if (type_ode=="feedback_A_DC"){
    prob = julia_eval("ODEProblem(ode_metaecosystem_feedback_no_A_to_T_DC, state, tspan, p)")
  } else if (type_ode=="feedback_T_DC"){
    prob = julia_eval("ODEProblem(ode_metaecosystem_feedback_no_T_to_A_DC, state, tspan, p)")
  } else if (type_ode=="feedback_T_topconsum"){
    prob = julia_eval("ODEProblem(ode_metaecosystem_feedback_no_T_to_A_topconsumers, state, tspan, p)")
  } else if (type_ode=="feedback_A_topconsum"){
    prob = julia_eval("ODEProblem(ode_metaecosystem_feedback_no_A_to_T_topconsumers, state, tspan, p)")
  } 
  
  if (type_ode=="flexible_stoi"){
    prob = julia_eval("ODEProblem(ode_metaecosystem_flexible_stoichio, state, tspan, p)")
  }
  
  sol = de$solve(prob, de$Tsit5())
  d = as.data.frame(t(sapply(sol$u, identity)))
  d$time=sol$t
  
  if (type_ode %in% c("topconsum","feedback_T_topconsum","feedback_A_topconsum")){ 
    d=d[,c(ncol(d),1:(ncol(d)-1))]
    names_for_col=c("Time","Top_cons_H_C","Top_cons_H_N","Top_cons_C_C","Top_cons_C_N","Herbivores_C","Herbivores_N","Consumers_C","Consumers_N","Plants_C","Plants_N","Decomposers_C","Decomposers_N","Detritus_T_C","Detritus_T_N","Detritus_A_C","Detritus_A_N","Nitrogen_T_N","Nitrogen_A_N")
  } else {
    d=d[,c(ncol(d),1:(ncol(d)-1))]
    names_for_col=c("Time","Herbivores_C","Herbivores_N","Consumers_C","Consumers_N","Plants_C","Plants_N","Decomposers_C","Decomposers_N","Detritus_T_C","Detritus_T_N","Detritus_A_C","Detritus_A_N","Nitrogen_T_N","Nitrogen_A_N")}
  
    
    final_point=as.numeric(d[nrow(d),-1])
    final_point[which(final_point<TRESH)]=0
    d[nrow(d),-1]=final_point
    colnames(d)=names_for_col  
  
  return(as_tibble(d))
}








### Managing or ploting ode output ----

Split_N_C_dynamics=function(data,Time=T){
  
  keep_c=c()
  for (i in 2:ncol(data)){
    if (strsplit(colnames(data)[i],split="_")[[1]][length(strsplit(colnames(data)[i],split="_")[[1]])]=="C"){
      keep_c=c(keep_c,i)
    }
  }
  
  
  d_C=data[,c(1,keep_c)]
  d_N=data[,-keep_c]
  if (Time==F) d_N=d_N[,-1]
  
  return(list(N=d_N,C=d_C))
  
}


plot_dynamics=function(data,log_=T){
  
  if (ncol(data)<16){
    colors = c("Consumers" = "darkorange", "Producers" = "green3", "Nitrogen" = "darkorchid2","Detritus"="brown")
  }else {
    colors = c("Consumers" = "darkorange", "Producers" = "green3", "Nitrogen" = "darkorchid2","Detritus"="brown","Top predators"="blue")
  }
  
  the_theme=theme_classic()+theme(legend.position = "bottom",
                                  strip.background = element_rect(fill = "#CCE8D8"),
                                  strip.text.y = element_text(size = 10, angle = -90, face = "italic"),
                                  strip.text.x = element_text(size = 10, face = "italic"),
                                  legend.text = element_text(size = 10))
  
  data=gather(data,variable, value,-Time)
  data$foodweb=Get_foodweb(data$variable); data$trophic_level=Get_trophic_level(data$variable);data$Resources=Get_type_resources(data$variable)
  p=ggplot(data)+
    geom_line(aes(x=Time,y=value,color=trophic_level,linetype=Resources),lwd=1)+ylim(0,max(data$value))+
    labs(x="Time",y="Patch density",color="Trophic level",linetype="Resource")+scale_color_manual(values=colors)+
    facet_grid(.~foodweb)+the_theme+
    theme(legend.box = "vertical")
  
  if (log_) {p=p+scale_x_log10()}
  
  return(p)
}



## B) Equilibrium related function ----

### Equilibrium ----

Condition_on_C=function(Eq,param){
  
  Num=param$IDa-param$ma*Eq$Decomposers_C-((param$lDa/(param$eB*param$aBD))*(param$dB+param$ma))
  
  Denom=param$aC*Eq$Decomposers_C-param$dC+(param$aC*param$lDa)/(param$eB*param$aBD)
  return(list(Num=Num,Denom=Denom))
}


Extract_equilibrium_from_dynamics=function(data,param,consumers=F){
  
  n_begin=ifelse(consumers,19,15)
  
  data_mean=as_tibble(t(colMeans(data[(nrow(data)-1000):nrow(data),-1])))
  data_with_param=cbind(data_mean,matrix(unlist(param),ncol = length(param),nrow=1))
  colnames(data_with_param)[n_begin:ncol(data_with_param)]=names(param)
  
  return(list(Eq=data_with_param))
  
}




Get_foodweb=function(vector){
  return(unlist(lapply(vector, function(x){
    ifelse (x %in% c("Top_cons_H_C","Top_cons_H_N","Plants_N","Plants_C","Herbivores_N","Herbivores_C","Nitrogen_T_N","Detritus_T_C","Detritus_T_N"),"Terrestrial","Aquatic")
  })
  ))
}

Get_type_resources=function(vector){
  return(unlist(lapply(vector, function(x){
    if (x %in% c("Top_cons_H_N","Top_cons_C_N","Herbivores_N","Consumers_N","Decomposers_N","Plants_N",'Nitrogen_T_N','Nitrogen_A_N',"Detritus_T_N","Detritus_A_N")) {return("N")
    } else {return("C")}
  })
  ))
}

Get_trophic_level=function(vector){
  
  return(unlist(lapply(vector, function(x){
    if (x %in% c("Plants_N","Plants_C","Decomposers_N","Decomposers_C")) return("Producers")
    if (x %in% c("Herbivores_C","Consumers_C","Herbivores_N","Consumers_N")) return("Consumers")
    if (x %in% c("Top_cons_H_C","Top_cons_H_N","Top_cons_C_C","Top_cons_C_N")) return("Top predators")
    if (x %in% c("Nitrogen_T_N","Nitrogen_A_N")) {return("Nitrogen")
      
    } else {return("Detritus")}
  })
  ))
  
}


Get_limitation=function(data,param){
  
  if (class(data)=="numeric"){
    data=as_tibble(t(data))
    colnames(data)=c("Herbivores_C","Herbivores_N","Consumers_C","Consumers_N","Plants_C","Plants_N","Decomposers_C","Decomposers_N","Detritus_T_C","Detritus_T_N","Detritus_A_C","Detritus_A_N","Nitrogen_T_N","Nitrogen_A_N")
    
  }
  
  if (((((param$rB-data$Detritus_A_N/data$Detritus_A_C)/param$rB)*param$eB*param$aBD*data$Detritus_A_C )/(param$aBN*data$Nitrogen_A_N) ) <1){
    limit="C-limited"
  }
  if (((((param$rB-data$Detritus_A_N/data$Detritus_A_C)/param$rB)*param$eB*param$aBD*data$Detritus_A_C )/(param$aBN*data$Nitrogen_A_N) ) >1){
    limit="N-limited"
  }
  
  ratio_C_N=((((param$rB-data$Detritus_A_N/data$Detritus_A_C)/param$rB)*param$eB*param$aBD*data$Detritus_A_C )/(param$aBN*data$Nitrogen_A_N) )
  
  return(list(Ratio=ratio_C_N,Limitation=limit))
  
}

Limitation_data=function(data){
  data$Limitation=sapply(1:nrow(data),function(x){
    
    param=data[x,15:ncol(data)]
    
    Get_limitation(data[x,-1],param)$Ratio
    
    
  })
 return(data) 
}




State_at_equilibrium=function(data,consumers=F){
  data=round(data,5)
  if (consumers==F){
    if (data$Plants_C==0 & data$Decomposers_C!=0){
      state="Terrestrial_extinct"
      
    }  else if (data$Plants_C!=0 & data$Decomposers_C==0){
      state="Aquatic_extinct"
      
    } else if (data$Plants_C!=0 & data$Herbivores_C!=0 & data$Decomposers_C!=0 & data$Consumers_C!=0 ){
      state="Coexistence"
      
    } else if (data$Plants_C!=0 & data$Herbivores_C==0 & data$Decomposers_C!=0 & data$Consumers_C==0 ){
      state="Primary_Producers"
      
    } else if (data$Plants_C!=0 & data$Herbivores_C==0 & data$Decomposers_C!=0 & data$Consumers_C!=0 ){
      state="H_extinct"
      
    } else if (data$Plants_C!=0 & data$Herbivores_C!=0 & data$Decomposers_C!=0 & data$Consumers_C==0 ){
      state="C_extinct"
      
    } else {state="no_organisms"}
  } else {
    
    if (data$Plants_C==0 & data$Decomposers_C!=0){
      state="Terrestrial_extinct"
      
    }  else if (data$Plants_C!=0 & data$Decomposers_C==0){
      state="Aquatic_extinct"
      
    } else if (data$Plants_C!=0 & data$Herbivores_C!=0 & data$Decomposers_C!=0 & data$Consumers_C!=0 & data$Top_cons_H_C!=0 & data$Top_cons_C_C!=0){
      state="Coexistence"
      
    } else if (data$Plants_C!=0 & data$Herbivores_C==0 & data$Decomposers_C!=0 & data$Consumers_C==0 & data$Top_cons_H_C==0 & data$Top_cons_C_C==0 ){
      state="Primary_Producers"
      
    } else if (data$Plants_C!=0 & data$Herbivores_C!=0 & data$Decomposers_C!=0 & data$Consumers_C!=0 & data$Top_cons_H_C==0 & data$Top_cons_C_C!=0 ){
      state="Top H extinct"
      
    } else if (data$Plants_C!=0 & data$Herbivores_C!=0 & data$Decomposers_C!=0 & data$Consumers_C!=0 & data$Top_cons_H_C!=0 & data$Top_cons_C_C==0 ){
      state="Top C extinct"
      
    } else {state="no_organisms"}
    
  }  
  return(state)
}



#2 ---> Metrics at the local and meta-ecosystem scale ----

## Productivity related functions----
#We distinguish the productivity (production per capita) and the production (flow of resources)

### Local scale----

Primary_production=function(state,param,consumers=F,colim=F,DC=F){
  
  if (class(state)[1]=="tbl_df"){
    state=state[nrow(state),-1]
  } 
  if (ncol(state)<30){ #i.e. there is not the parameters
    state=cbind(state,matrix(unlist(param),ncol = length(param),nrow=1))
    col_begin=ifelse(consumers,19,15)
    colnames(state)[col_begin:ncol(state)]=names(param)
  }
  
  limit=Get_limitation(state,param)
  
  if (limit$Limitation=="C-limited"){
    prod_aqua=state$eB*state$aBD*state$Detritus_A_C
  }
  if (limit$Limitation=="N-limited"){
    prod_aqua=state$aBN*state$Nitrogen_A_N
  }
  
  if (DC==T){ 
    prod_terr=state$aP*state$Nitrogen_T_N 

  } else {
    prod_terr=state$aP*state$Nitrogen_T_N*state$Plants_C
  }
  
  
  if (colim){prod_aqua=state$eB*state$aBD*state$Detritus_A_C*state$aBN*state$Nitrogen_A_N}
  
  prod_tot_aq=state$aBD*state$Decomposers_C*state$Detritus_A_C+state$aBN*state$Decomposers_C*state$Nitrogen_A_N # for testing
  
  return(list(Terrestrial=prod_terr,
              Aquatic=prod_aqua,Aqua_tot=prod_tot_aq))
}



Primary_productivity=function(state,param,consumers=F,colim=F){
  
  if (class(state)[1]=="tbl_df"){
    state=state[nrow(state),-1]
  } 
  if (ncol(state)<30){ #i.e. there is not the parameters
    state=cbind(state,matrix(unlist(param),ncol = length(param),nrow=1))
    col_begin=ifelse(consumers,19,15)
    colnames(state)[col_begin:ncol(state)]=names(param)
  }
  
  limit=Get_limitation(state,param)
  if (limit$Limitation=="C-limited"){
    prod_aqua=state$eB*state$aBD*state$Detritus_A_C
  }
  if (limit$Limitation=="N-limited"){
    prod_aqua=state$aBN*state$Nitrogen_A_N
  }
  if (colim){prod_aqua=state$eB*state$aBD*state$Detritus_A_C*state$aBN*state$Nitrogen_A_N}
  
  prod_tot_aq=state$aBD*state$Detritus_A_C+state$aBN*state$Nitrogen_A_N # for testing
  return(list(Terrestrial=state$aP*state$Nitrogen_T_N,
              Aquatic=prod_aqua,Aqua_tot=prod_tot_aq))
}


Secondary_production=function(state,param,consumers=F,DC=F){
  
  if (class(state)[1]=="tbl_df"){
    state=state[nrow(state),-1]
  }
  if (ncol(state)<30){ #i.e. there is not the parameters
    state=cbind(state,matrix(unlist(param),ncol = length(param),nrow=1))
    col_begin=ifelse(consumers,19,15)
    colnames(state)[col_begin:ncol(state)]=names(param)
  }
  
  
  if (DC==T){ 
    prod_terr=state$eH*state$aH*state$Plants_C 
    prod_aq=state$eC*state$aC*state$Decomposers_C  
    
  } else {
    prod_terr=state$eH*state$aH*state$Plants_C*state$Herbivores_C
    prod_aq=state$eC*state$aC*state$Decomposers_C*state$Consumers_C
  }
  
  
  return(list(Terrestrial=prod_terr,
              Aquatic=prod_aq))
}



Secondary_productivity=function(state,param,consumers=F){
  
  if (class(state)[1]=="tbl_df"){
    state=state[nrow(state),-1]
  }
  if (ncol(state)<30){ #i.e. there is not the parameters
    state=cbind(state,matrix(unlist(param),ncol = length(param),nrow=1))
    col_begin=ifelse(consumers,19,15)
    colnames(state)[col_begin:ncol(state)]=names(param)
  }
  
  return(list(Terrestrial=state$eH*state$aH*state$Plants_C,
              Aquatic=state$eC*state$aC*state$Decomposers_C))
}


Secondary_productivity_topconsum=function(state,param){
  
  if (class(state)[1]=="tbl_df"){
    state=state[nrow(state),-1]
  }
  if (ncol(state)<30){ #i.e. there is not the parameters
    state=cbind(state,matrix(unlist(param),ncol = length(param),nrow=1))
    colnames(state)[19:ncol(state)]=names(param)
  }
  
  return(list(Terrestrial=state$eTH*state$aTH*state$Herbivores_C,
              Aquatic=state$eTC*state$aTC*state$Consumers_C))#constant
}

Secondary_production_topconsum=function(state,param){
  
  if (class(state)[1]=="tbl_df"){
    state=state[nrow(state),-1]
  }
  if (ncol(state)<30){ #i.e. there is not the parameters
    state=cbind(state,matrix(unlist(param),ncol = length(param),nrow=1))
    colnames(state)[19:ncol(state)]=names(param)
  }
  
  return(list(Terrestrial=state$eTH*state$aTH*state$Top_cons_H_C*state$Herbivores_C,
              Aquatic=state$eTC*state$aTC*state$Top_cons_C_C*state$Consumers_C))
}



### Meta-ecosystem scale----

LRR_meta_ecosystem=function(P1,P2,P1_recy,P2_recy,PP1,PP2,PP1_recy,PP2_recy){
  
  # "_recy" designs the productivity at local scale with only local recycling
  
  # We consider two metrics at the meta-ecosystem scale : 
  
  ## We compute the LRR of productivity at the meta-ecosystem scale as : log (sum(Prod (delta_X !=0))/ sum(Prod(delta_X=0)))
  # We do this for productivity and production
  
  #Productivity
  Metric_log_sum_P=log(sum(P1$Terrestrial,P1$Aquatic,P2$Terrestrial,P2$Aquatic)/ #for all productions
                         sum(P1_recy$Terrestrial,P1_recy$Aquatic,P2_recy$Terrestrial,P2_recy$Aquatic))
  
  Metric_log_sum_P_1=log(sum(P1$Terrestrial,P1$Aquatic)/#for primary production only
                         sum(P1_recy$Terrestrial,P1_recy$Aquatic))
  
  Metric_log_sum_P_2=log(sum(P2$Terrestrial,P2$Aquatic)/#for secondary production only
                         sum(P2_recy$Terrestrial,P2_recy$Aquatic))
  
  
  #Production
  Metric_log_sum_PP=log(sum(PP1$Terrestrial,PP1$Aquatic,PP2$Terrestrial,PP2$Aquatic)/
                          sum(PP1_recy$Terrestrial,PP1_recy$Aquatic,PP2_recy$Terrestrial,PP2_recy$Aquatic))

  Metric_log_sum_PP_1=log(sum(PP1$Terrestrial,PP1$Aquatic)/#for primary production only
                           sum(PP1_recy$Terrestrial,PP1_recy$Aquatic))
  
  Metric_log_sum_PP_2=log(sum(PP2$Terrestrial,PP2$Aquatic)/#for secondary production only
                           sum(PP2_recy$Terrestrial,PP2_recy$Aquatic))
  

  
  d=tibble(Metric_log_sum_P,Metric_log_sum_P_1,Metric_log_sum_P_2,Metric_log_sum_PP,Metric_log_sum_PP_1,Metric_log_sum_PP_2)
  return(d)
}




Compute_feedbacks=function(Eq,param,type_prod="Production",n_time=1000,
                           DC=F,plot=F,top_consumers=F,
                           feedback_with_input=F){
  
  "Function that take the equilibrium values from a meta-ecosystem fully connected and return the value of the feedback"
  
  #Parameters should be given for full coupling
  
  `%!in%` = Negate(`%in%`)
  
  Eq_b=Eq;param_b=param
  
  if (nrow(Eq_b)>1) stop("Eq_b should be given by the equilibrium values")
  if (type_prod %!in% c("Production","Productivity")) stop("This should be productivity or production")
  
  state=Get_initial_values(param_b)
  
  if (top_consumers) {state=State_topconsum(state,param_b)}
  
  #First we need the case of bidirectional flows : this is Eq_b
  
  #Now we compute the case where there is only unidirectional flows and all local recycling flows are lost from the meta-ecosystem
  
  #First where there is only unidirectional flows of subsidies from A to T
  
  param_uni_T=param_b
  
  data_T=Compute_ode(state,param_uni_T,n_time = n_time,type_ode = ifelse(DC==T,"feedback_T_DC",ifelse(top_consumers==F,"feedback_T","feedback_T_topconsum")))
  
  if (feedback_with_input==T){
    param_uni_T$DC_input=Eq_b$Plants_C*param_b$dP*param_b$pP +
      Eq_b$Herbivores_C*Eq_b$dH*param_b$pH
    param_uni_T$DN_input=Eq_b$Plants_C*param_b$dP*param_b$pP*param_b$rP +
      Eq_b$Herbivores_C*Eq_b$dH*param_b$pH*param_b$rH
    param_uni_T$N_input=(Eq_b$rP-Eq_b$rH*Eq_b$eH)*Eq_b$aH*Eq_b$Herbivores_C*Eq_b$Plants_C*Eq_b$pH
    data_T=Compute_ode(state,param_uni_T,n_time = n_time,type_ode ="feedback_T_input")
  }
  
  
  if (plot==T) print(plot_dynamics(data_T,last = F))
  
  Eq_no_T_to_A=Extract_equilibrium_from_dynamics(data_T,param_uni_T,consumers = ifelse(top_consumers==T,T,F))$Eq
  
  #Second where there is only unidirectional flows of subsidies from T to A
  
  param_uni_A=param_b
  data_A=Compute_ode(state,param_uni_A,n_time = n_time,type_ode = ifelse(DC==T,"feedback_A_DC",ifelse(top_consumers==F,"feedback_A","feedback_A_topconsum")))
  
  if (feedback_with_input==T){
    param_uni_A$DC_input=Eq_b$Decomposers_C*param_b$dB*param_b$pB +
      Eq_b$Consumers_C*Eq_b$dC*param_b$pC
    param_uni_A$DN_input=Eq_b$Decomposers_C*param_b$dB*param_b$pB*param_b$rB +
      Eq_b$Consumers_C*Eq_b$dC*param_b$pC*param_b$rC
    param_uni_A$N_input=(Eq_b$rB-Eq_b$rC*Eq_b$eC)*Eq_b$aC*Eq_b$Consumers_C*Eq_b$Decomposers_C*Eq_b$pC
    data_A=Compute_ode(state,param_uni_A,n_time = n_time,type_ode ="feedback_A_input")
  }
  
  if (plot==T) print(plot_dynamics(data_A,last = F))

  Eq_no_A_to_T=Extract_equilibrium_from_dynamics(data_A,param_uni_A,consumers = ifelse(top_consumers==T,T,F))$Eq
  
  #now we compare the metrics
  
  if (type_prod=="Production"){
    
    P1_bidirectional=Primary_production(Eq_b,param_b,DC=DC)
    P1_no_A_to_T=Primary_production(Eq_no_A_to_T,param_b,DC=DC)
    P1_no_T_to_A=Primary_production(Eq_no_T_to_A,param_b,DC=DC)
    
    P2_bidirectional=Secondary_production(Eq_b,param_b,DC=DC)
    P2_no_A_to_T=Secondary_production(Eq_no_A_to_T,param_b,DC=DC)
    P2_no_T_to_A=Secondary_production(Eq_no_T_to_A,param_b,DC=DC)
    
    #And we calculate the net effect
    
    Net_T_1=P1_bidirectional$Terrestrial-P1_no_T_to_A$Terrestrial
    Net_A_1=P1_bidirectional$Aquatic-P1_no_A_to_T$Aquatic

    Net_T_2=P2_bidirectional$Terrestrial-P2_no_T_to_A$Terrestrial
    Net_A_2=P2_bidirectional$Aquatic-P2_no_A_to_T$Aquatic
    
    
    
  } else{
    
    P1_bidirectional=Primary_productivity(Eq_b,param_b)
    P1_no_A_to_T=Primary_productivity(Eq_no_A_to_T,param_b)
    P1_no_T_to_A=Primary_productivity(Eq_no_T_to_A,param_b)
    
    P2_bidirectional=Secondary_productivity(Eq_b,param_b)
    P2_no_A_to_T=Secondary_productivity(Eq_no_A_to_T,param_b)
    P2_no_T_to_A=Secondary_productivity(Eq_no_T_to_A,param_b)
    
    #And we calculate the net effect
    
    Net_T_1=P1_bidirectional$Terrestrial-P1_no_T_to_A$Terrestrial
    Net_A_1=P1_bidirectional$Aquatic-P1_no_A_to_T$Aquatic
    
    Net_T_2=P2_bidirectional$Terrestrial-P2_no_T_to_A$Terrestrial
    Net_A_2=P2_bidirectional$Aquatic-P2_no_A_to_T$Aquatic
    
    
  }
  
  if (top_consumers==T){
    
    P3_bidirectional=Secondary_production_topconsum(Eq_b,param_b)
    P3_no_T_to_A=Secondary_production_topconsum(Eq_no_T_to_A,param_b)
    P3_no_A_to_T=Secondary_production_topconsum(Eq_no_A_to_T,param_b)
    Net_T_3=P3_bidirectional$Terrestrial-P3_no_T_to_A$Terrestrial
    Net_A_3=P3_bidirectional$Aquatic-P3_no_A_to_T$Aquatic
  
    return(list(Net_T_1=Net_T_1,Net_A_1=Net_A_1,Net_T_2=Net_T_2,Net_A_2=Net_A_2,Net_T_3=Net_T_3,Net_A_3=Net_A_3,
                P1_no_A_to_T=P1_no_A_to_T$Terrestrial,P1_no_T_to_A=P1_no_T_to_A$Aquatic,P2_no_A_to_T=P2_no_A_to_T$Terrestrial,P2_no_T_to_A=P2_no_T_to_A$Aquatic,
                Eq_uni_A=Eq_no_A_to_T,Eq_uni_T=Eq_no_T_to_A))
    
  } else{
  
    return(list(Net_T_1=Net_T_1,Net_A_1=Net_A_1,Net_T_2=Net_T_2,Net_A_2=Net_A_2,
                P1_no_A_to_T=P1_no_A_to_T$Terrestrial,P1_no_T_to_A=P1_no_T_to_A$Aquatic,P2_no_A_to_T=P2_no_A_to_T$Terrestrial,P2_no_T_to_A=P2_no_T_to_A$Aquatic,
                Eq_uni_A=Eq_no_A_to_T,Eq_uni_T=Eq_no_T_to_A))
  }
}






### Flow of nitrogen ----

Net_flow_nitrogen=function(data,param){
  
  #At equilibrium we compute the netflow of subsidies of carbon and nitrogen. 
  #It could be useful to understand which ecosystem drives the pattern observed in N-limitation
  
  #first nitrogen
  flow_N_A_to_T=(param$rB-param$eC*param$rC)*param$aC*data$Decomposers_C*data$Consumers_C*param$pC  +  #excretion consumers
                param$rC*param$dC*data$Consumers_C*param$pC   +  param$rB*param$dB*data$Decomposers_C*param$pB
  
  flow_N_T_to_A=(param$rP-param$eH*param$rH)*param$aH*data$Herbivores_C*data$Plants_C*param$pH  +  #excretion herbivores
                param$rH*param$dH*data$Herbivores_C*param$pH  +  param$rP*param$dP*data$Plants_C*param$pP
  
  flow_N_A_to_A=(param$rB-param$eC*param$rC)*param$aC*data$Decomposers_C*data$Consumers_C*(1-param$pC)  +  #excretion consumers
    param$rC*param$dC*data$Consumers_C* (1-param$pC)   +  param$rB*param$dB*data$Decomposers_C*(1-param$pB)
  
  flow_N_T_to_T=(param$rP-param$eH*param$rH)*param$aH*data$Herbivores_C*data$Plants_C*(1-param$pH)  +  #excretion herbivores
    param$rH*param$dH*data$Herbivores_C*(1-param$pH ) +  param$rP*param$dP*data$Plants_C*(1-param$pP)
  
  
  #and carbon
  flow_C_A_to_T=param$dC*data$Consumers_C*param$pC + param$dB*data$Decomposers_C*param$pB
  
  flow_C_T_to_A=param$dH*data$Herbivores_C*param$pH + param$dP*data$Plants_C*param$pP
  
  flow_C_A_to_A=param$dC*data$Consumers_C*(1-param$pC) + param$dB*data$Decomposers_C*(1-param$pB)
  
  flow_C_T_to_T=param$dH*data$Herbivores_C*(1-param$pH) + param$dP*data$Plants_C*(1-param$pP)
  
  
  #Now the netflow :
  
  N_receive_A=flow_N_T_to_A-flow_N_A_to_T
  C_receive_A=flow_C_T_to_A-flow_C_A_to_T
  
  netflow=tibble(N_receive_A=N_receive_A,C_receive_A=C_receive_A,
                 flow_C_T_to_A=flow_C_T_to_A,flow_C_A_to_T=flow_C_A_to_T,
                 flow_N_T_to_A=flow_N_T_to_A,flow_N_A_to_T=flow_N_A_to_T,
                 flow_C_T_to_T=flow_C_T_to_T,flow_N_T_to_T=flow_N_T_to_T,
                 flow_C_A_to_A=flow_C_A_to_A,flow_N_A_to_A=flow_N_A_to_A)
  
  return(netflow)
  
}

  


# 4 ---> Plotting with igraph ----

## 1) Simple igraph ecosystem with net flows ----
Plot_net_ecosystem_flow=function(net_flow_C_to_A,net_flow_N_to_A,vertex_size=50,scaling_width=4){
  
  transform_C_width=(scaling_width*net_flow_C_to_A/0.8874744)+2;  transform_N_width=(scaling_width*net_flow_N_to_A/0.8874744)+2
  
  matrix=matrix(c(0,0,0,0),nrow=2,byrow = F);colnames(matrix)=rownames(matrix)=c("A","T")
  net=graph_from_adjacency_matrix(matrix , mode='directed', diag=F )
  V(net)$label=c("A","T");V(net)$size =50;V(net)$frame.color=c("black")
  
  if (net_flow_C_to_A>0 & net_flow_N_to_A>0) {
  
  net=net%>%add_edges(.,c("T","A", "T","A")) ;E(net)$color=c("#90D0E0","#B17F4F") 
  plot(net,vertex.color=c(col.alpha('#3023C7',.7),col.alpha('#21AB10',.5)),label.color="white",vertex.label.color="white",
     edge.arrow.width=c(2),edge.arrow.size=.5,edge.width=c(0,transform_N_width,transform_C_width))
  }
  
  if (net_flow_C_to_A<0 & net_flow_N_to_A>0) {
    
  net=net%>%add_edges(.,c("T","A", "A","T")) ;E(net)$color=c("#90D0E0","#B17F4F") 
  plot(net,vertex.color=c(col.alpha('#3023C7',.7),col.alpha('#21AB10',.5)),label.color="white",vertex.label.color="white",edge.curved=0.8,
       edge.arrow.width=c(2),edge.arrow.size=.75,c(transform_N_width,transform_C_width))
  }
  
  if (net_flow_C_to_A<0 & net_flow_N_to_A<0) {
    
  net=net%>%add_edges(.,c("A","T", "A","T")) ;E(net)$color=c("#B17F4F","#90D0E0") 
  plot(net,vertex.color=c(col.alpha('#3023C7',.7),col.alpha('#21AB10',.5)),label.color="white",vertex.label.color="white",
       edge.arrow.width=c(2),0,edge.arrow.size=.75,edge.width=c(transform_N_width,0,transform_C_width))
  }
  
  if (net_flow_C_to_A>0 & net_flow_N_to_A<0) {
    
  net=net%>%add_edges(.,c("A","T", "T","A")) ;E(net)$color=c("#90D0E0","#B17F4F") 
  plot(net,vertex.color=c(col.alpha('#3023C7',.7),col.alpha('#21AB10',.5)),label.color="white",vertex.label.color="white",edge.curved=0.8,
       edge.arrow.width=c(2),edge.arrow.size=.75,edge.width=c(transform_N_width,transform_C_width))

  }
  
}

## 2) Meta-ecosystem dynamics----
Plot_meta_ecosystem_graph=function(data_save,data,param){
  
 
  #colors
  color_troph=c("Nt"="#90D0E0","Dt"="#B17F4F","P"="#89C584","H"= col.alpha("red",.3),"Na"="#90D0E0","Da"="#B17F4F","B"="#89C584","C"=col.alpha("red",.6))
  color_net=c("C"="#D2B96F","N"="#AC86DA")
  
 
  color_ecosys=c("Aquatic"="#A4DEE6","Terrestrial"=col.alpha("#BBE0A7",.8))
  color_all=c(color_ecosys,color_troph)
  
  
  #Size of point proportional to their density
  value_eq_C=c(data$Nitrogen_T_N,data$Detritus_T_C,data$Plants_C+.5,data$Herbivores_C,data$Nitrogen_A_N,data$Detritus_A_C,data$Decomposers_C+1,data$Consumers_C)
  
  
  circle=tibble(x=c(0.5,1.5,1,1,2.5,3.5,3,3),y=rep(c(1,1,2,3),2),name=c("Nt","Dt","P","H","Na","Da","B","C"),r=value_eq_C/20)
  
  #two ecosystems
  rectangle=tibble(xmin=c(0.5-filter(circle,name=="Nt")$r -.1,2.5-filter(circle,name=="Na")$r -.1),
                   xmax=c(1.5+filter(circle,name=="Dt")$r +.1,3.5+filter(circle,name=="Da")$r +.1),
                   ymin=c(1-max(filter(circle,name=="Dt")$r,filter(circle,name=="Nt")$r,filter(circle,name=="Da")$r,filter(circle,name=="Na")$r)-.1,
                          1-max(filter(circle,name=="Dt")$r,filter(circle,name=="Nt")$r,filter(circle,name=="Da")$r,filter(circle,name=="Na")$r)-.1),
                   ymax=c(3+max(filter(circle,name=="H")$r,filter(circle,name=="C")$r)+.1,3+max(filter(circle,name=="H")$r,filter(circle,name=="C")$r)+.1),
                   name=c("Terrestrial","Aquatic"))
  
  #trophic interaction
  arrow_line_troph=tibble(x=c(0),xend=c(0),y=c(0),yend=c(0),linetype=c(0),color=c(""))%>%
    add_row(.,x=1,xend=1,y=2+filter(circle,name=="P")$r,yend=3-filter(circle,name=="H")$r,linetype=1,color="black") %>% # plant-herbivore
    add_row(.,x=3,xend=3,y=2+filter(circle,name=="B")$r,yend=3-filter(circle,name=="C")$r,linetype=1,color="black") %>% # decomposers-consumers
    add_row(.,x=.5,xend=1,y=1+filter(circle,name=="Nt")$r,yend=2-filter(circle,name=="P")$r,linetype=1,color="black")   # plant-nitrogen
  arrow_line_troph=arrow_line_troph[-1,]
  
  arrow_curve_troph=tibble(x=c(0),xend=c(0),y=c(0),yend=c(0),linetype=c(0),color=c(""))%>%
    add_row(x=2.5,xend=3,y=1+filter(circle,name=="Na")$r,yend=2-filter(circle,name=="B")$r,linetype=1,color='black')%>%  # immobilisation
    add_row(x=3.5,xend=3+filter(circle,name=="B")$r,y=1+filter(circle,name=="Da")$r,yend=2,linetype=1,color='black')     # decomposition
  arrow_curve_troph=arrow_curve_troph[-1,]
  
  
  #recycling
  arrow_line_recy=tibble(x=c(0),xend=c(0),y=c(0),yend=c(0),linetype=c(0),color=c(""))%>%  
    add_row(x=3+filter(circle,name=="C")$r,xend=3.5,y=3,yend=1+filter(circle,name=="Da")$r,linetype=4,color='gray40')%>%    # recycling C
    add_row(x=1+filter(circle,name=="H")$r,xend=1.5,y=3,yend=1+filter(circle,name=="Dt")$r,linetype=4,color='gray40')%>%    # recycling H
    add_row(x=3-filter(circle,name=="C")$r,xend=2.5,y=3,yend=1+filter(circle,name=="Na")$r,linetype=4,color='gray40')%>%    # imbalance C
    add_row(x=1-filter(circle,name=="H")$r,xend=0.5,y=3,yend=1+filter(circle,name=="Nt")$r,linetype=4,color='gray40')%>%    # imbalance H
    add_row(x=1.5-filter(circle,name=="Dt")$r,xend=.5+filter(circle,name=="Nt")$r,y=1,yend=1,linetype=4,color='gray40')%>%  # mineralization T
    add_row(x=1+filter(circle,name=="P")$r,xend=1.5,y=2,yend=1+filter(circle,name=="Dt")$r,linetype=4,color='gray40')       # recycling P
  arrow_line_recy=arrow_line_recy[-1,]
  
  
  arrow_curve_recy=tibble(x=c(0),xend=c(0),y=c(0),yend=c(0),linetype=c(0),color=c(""))%>%
    add_row(x=3-filter(circle,name=="B")$r,xend=2.5,y=2,yend=1+filter(circle,name=="Na")$r,linetype=4,color='gray40')%>% # mineralization A
    add_row(x=3,xend=3.5,y=2-filter(circle,name=="B")$r,yend=1+filter(circle,name=="Da")$r,linetype=4,color='gray40')    # recycing B
  arrow_curve_recy=arrow_curve_recy[-1,]
  
    
    

  # we add the netflow of nitrogen and carbon
  net_N=Net_flow_nitrogen(data_save,param)$N_receive_A
  net_C=Net_flow_nitrogen(data_save,param)$C_receive_A
  
  arrow_netflow=tibble(x=c(1.5,1.5),y=c(1.8,2.2),yend=c(1.8,2.2),xend=c(2.5,2.5),name=c("N","C"))
  
  
  if (net_N<0 & net_C>0) {
    savey=filter(arrow_netflow,name=="N")$xend
    arrow_netflow$xend[1]=arrow_netflow$x[1]
    arrow_netflow$x[1]=savey
  }
  if (net_N>0 & net_C<0) {
    savey=filter(arrow_netflow,name=="C")$xend
    arrow_netflow$xend[2]=arrow_netflow$x[2]
    arrow_netflow$x[2]=savey
  }
  if (net_N<0 & net_C<0) {
    savey=filter(arrow_netflow,name=="N")$xend
    savey2=filter(arrow_netflow,name=="C")$xend
    arrow_netflow$xend[1]=arrow_netflow$x[1]
    arrow_netflow$x[1]=savey
    arrow_netflow$xend[2]=arrow_netflow$x[2]
    arrow_netflow$x[2]=savey2
  }
  
  ggplot() +
    
    geom_rect(data=rectangle, mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=name), color="transparent", alpha=0.5) + scale_fill_manual(values=color_all)+
    
    geom_segment(data=arrow_netflow,aes(x = x, y = y, xend = xend, yend = yend,color=name,size=name),arrow = arrow(length = unit(0.2, "cm")))+
    
    geom_circle(aes(x0 = x, y0 = y, r = r, fill = name),color="transparent", data = circle)+
    
    geom_segment(data=arrow_line_recy,aes(x = x, y = y, xend = xend, yend = yend,linetype=linetype,color=color),arrow = arrow(length = unit(0.2, "cm")),
                 linetype=1,color="gray70",size=.1)+
    
    geom_segment(data=arrow_line_troph,aes(x = x, y = y, xend = xend, yend = yend,linetype=linetype,color=color),arrow = arrow(length = unit(0.2, "cm")),
                 size=.7,linetype=1,color="black")+
    
    geom_curve(data=arrow_curve_recy,aes(x = x, y = y, xend =xend , yend = yend,linetype=linetype,color=color),arrow = arrow(length = unit(0.2, "cm")),
               curvature = 0.2,size=.1,linetype=1,color="gray70")+
    
    geom_curve(data=arrow_curve_troph,aes(x = x, y = y, xend =xend , yend = yend,linetype=linetype,color=color),arrow = arrow(length = unit(0.2, "cm")),
               curvature = 0.2,size=.7,linetype=1,color="black")+
    
    annotate("text",x=c(mean(c(rectangle[1,]$xmax,rectangle[2,]$xmin)),mean(c(rectangle[1,]$xmax,rectangle[2,]$xmin))),y=c(1.6,2.4),label=c("N","C"),color=c("#AC86DA","#D2B96F"),fontface="bold")+
    scale_color_manual(values = color_net)+
    scale_size_manual(values=c("N"=1.5*abs(net_N),"C"=1.5*abs(net_C)))+theme_transparent()+theme(legend.position = "none")+labs(x="")
  
  
    

}
