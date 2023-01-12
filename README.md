# Code for running analyses from the stoichiometric meta-ecosystem model

Contact: Benoît Pichon, *benoit.pichon0@gmail.com*

This folder contains all code necessary to replicate the analysis in the main text and in supplementary. 

First, **empirical data** can be found in their folder Empirical data, where the files corresponding to list of paper used (List_papers.csv), the nitrogen and carbon flows at terrestrial-freshwater ecotone (N_flows_Aq_Terr.csv, C_flows_Aq_Terr.csv respectively) and the stoichiometric ratio of fluxed exported at this ecotone (Stoichio_NC.csv) are present.
To replicate Fig1 on empirical data, the script is in Make_figs.R file.

For the **simulations** there are 2 scripts: one of functions (Stoichio_function.R) and the other for the different analysis made (Stoichio_main.R). Figures can be generated using Make_figs.R 

All simulations are fast to run using the *DifferentialEquation* package from Julia (~3 h for all simulations).

For more details, see the preprint: Quality matters: stoichiometry of resources modulates spatial feedbacks in an aquatic-terrestrial meta-ecosystem. Benoît Pichon, Elisa Thébault, Gérard Lacroix et Isabelle Gounand bioRxiv.