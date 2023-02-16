# Code for running analyses from the stoichiometric meta-ecosystem model

Contact: Benoît Pichon, *benoit.pichon0@gmail.com*

This folder contains all code necessary to replicate the analysis in the main text and in supplementary. 

## `Data`

The **`Empirical_data`** contains four *csv* files necessary to reproduce the Figure 1. 

1. The first csv, **`List_papers.csv`**, correspond to the list of papers from which data have been collected. 
   
2. The second (**`N_flows_Aq_Terr.csv`**) and the third (**`C_flows_Aq_Terr.csv`**) one contain the nitrogen and carbon flows at terrestrial-freshwater ecotone respectively. 
   
3. Finally, the last *csv* contains the stoichiometric ratio of fluxes exported at this ecotone (**`Stoichio_NC.csv`**).


## `Simulations`

The simulations are made using R (*v4.1.0*) and Julia (*v1.7.3*). All scripts are written in R, and we call julia via R using the **JuliaCall** R package.
All simulations are gathered in the file `Stoichio_main.R`.
This file is organized in different section, with each corresponding to a different Figure (press Alt+O to see the sections).
All simulations are fast to run (~2h for all analyses) using the functions from Julia. 


## `Figures`

All figures in the main text and in supplementary can be replicated using the `Make_figs.R` file. Each section in the file contain the code to replicate a given figure.

For more details, see the preprint: **Quality matters: stoichiometry of resources modulates spatial feedbacks in aquatic-terrestrial meta-ecosystems. Benoît Pichon, Elisa Thébault, Gérard Lacroix et Isabelle Gounand**.



