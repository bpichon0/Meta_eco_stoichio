# Code for running analyses from the stoichiometric meta-ecosystem model

Contact: Benoît Pichon, *benoit.pichon0@gmail.com*



<p align="center">
    <img src="https://github.com/bpichon0/Meta_eco_stoichio/blob/master/Example/Aquatic_terrestrial_ecotone.jpg" width="500">
</p>


This folder contains all code necessary to replicate the analysis in the main and supplementary text. 

## `Data`

The **`Empirical_data`** contains four *csv* files necessary to reproduce Figure 1. 

1. The first csv, **`List_papers.csv`**, corresponds to the list of papers from which data have been collected. 
   
2. The second (**`N_flows_Aq_Terr.csv`**) and the third (**`C_flows_Aq_Terr.csv`**) contain the nitrogen and carbon flows at terrestrial-freshwater ecotone respectively. 
   
3. Finally, the last *csv* contains the stoichiometric ratio of fluxes exported at this ecotone (**`Stoichio_NC.csv`**).


## `Simulations`

The simulations are made using R (*v4.1.0*) and Julia (*v1.7.3*). All scripts are written in R, and we call julia via R using the **JuliaCall** R package.
All simulations are gathered in the file `Stoichio_main.R`.
This file is organized in different section, with each corresponding to a different Figure (press Alt+O to see the sections). **To replicate a specific figure, please look at the sections number in `Stoichio_main.R` and `Make_figs.R`. This code is indicated to see which simulation is needed to replicate a given figure.** 
All simulations are fast to run (~2h for all analyses) using the functions from Julia. 


## `Figures`

All figures in the main text and in supplementary can be replicated using the `Make_figs.R` file. Each section in the file contains the code to replicate a given figure. **Before ploting the figures, you need to run the simulations.**

For more details, see the paper: **Quality matters: stoichiometry of resources modulates spatial feedbacks in aquatic-terrestrial meta-ecosystems. *Ecology Letters*. Benoît Pichon, Elisa Thébault, Gérard Lacroix et Isabelle Gounand (2023)**.



## `Working example`

The model is displayed bellow; it simulates the dynamics of carbon and nitrogen in a meta-ecosystem at terrestrial-aquatic ecotone.



<p align="center">
    <img src="https://github.com/bpichon0/Meta_eco_stoichio/blob/master/Example/Model.jpg" width="600">
</p>

Let's take an example of a simulation where decomposers are carbon-limited and there is spatial flows coupling both ecosystems.

```R

source("Stoichio_functions.R") #initializing the Julia-R link

param=Get_classical_param(scena = "C-limited",coupling = T) #carbon-limitation and with spatial coupling. Parameters can be changed in the list "param". To access parameters used under nitrogen limitation scenario, scena has to be changed to "N-limited"

state=Get_initial_values(param) #initial conditions
dynamics=Compute_ode(state,param) #running dynamics of the fully connected ecosystems

plot_dynamics(dynamics) #displaying the dynamics
```

<p align="center">
    <img src="https://github.com/bpichon0/Meta_eco_stoichio/blob/master/Example/Dynamics.svg" width="500">
</p>


Finally, we can display the net flows of carbon and nitrogen, as well as the carbon stocks in all trophic levels:

```R
Eq=Extract_equilibrium_from_dynamics(dynamics,param) #Equilibrium
Plot_meta_ecosystem_graph(Eq$Eq,Eq$Eq,param)
```

<p align="center">
    <img src="https://github.com/bpichon0/Meta_eco_stoichio/blob/master/Example/Stocks.svg" width="400">
</p>
