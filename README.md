# Code for running analyses from the stoichiometric meta-ecosystem model

Contact: Benoît Pichon, *benoit.pichon0@gmail.com*

This folder contains all code necessary to replicate the analysis in the main text and in supplementary. 

First, **empirical data** can be found in their folder Empirical data. The four csv files correspond to the list of papers from which data was collected (**List_papers.csv**), the nitrogen and carbon flows at terrestrial-freshwater ecotone (**N_flows_Aq_Terr.csv**, **C_flows_Aq_Terr.csv** respectively) and the stoichiometric ratio of fluxes exported at this ecotone (**Stoichio_NC.csv**).
To replicate Fig1 on empirical data, the script is in Make_figs.R file.

For the **simulations** there are 2 scripts: one of functions (Stoichio_function.R) and the other for the different analysis made (Stoichio_main.R). The latter is organized by figure: a chunck of code (press Alt+O to see the different chunks) for each different figure. Once simulations are generated, figures can be made using Make_figs.R.

All simulations are fast to run (~2h for all analyses) with the *DifferentialEquation* package from Julia. Hence, Julia needs to be installed on the computer which run the simulations.

For more details, see the preprint: Quality matters: stoichiometry of resources modulates spatial feedbacks in aquatic-terrestrial meta-ecosystems. Benoît Pichon, Elisa Thébault, Gérard Lacroix et Isabelle Gounand.