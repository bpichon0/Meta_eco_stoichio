# Code for running analyses from the stoichiometric meta-ecosystem model

Contact: Benoît Pichon, *benoit.pichon0@gmail.com*

This folder contains all code necessary to replicate the analysis in the main text and in supplementary. 

First, **empirical data** can be found in their folder Empirical data. The four csv correspond to the list of paper from which data was collected (List_papers.csv), the nitrogen and carbon flows at terrestrial-freshwater ecotone (N_flows_Aq_Terr.csv, C_flows_Aq_Terr.csv respectively) and the stoichiometric ratio of fluxes exported at this ecotone (Stoichio_NC.csv).
To replicate Fig1 on empirical data, the script is in Make_figs.R file.

For the **simulations** there are 2 scripts: one of functions (Stoichio_function.R) and the other for the different analysis made (Stoichio_main.R). The script is organized by figure: a chunck of code for each different analysis. Once simulations are generated, figures can be made using Make_figs.R 

All simulations are fast to run (~5 min each)  with the *DifferentialEquation* package from Julia. Hence, Julia needs to be installed on the computer which run the simulations.

For more details, see the preprint: Quality matters: stoichiometry of resources modulates spatial feedbacks in an aquatic-terrestrial meta-ecosystem. Benoît Pichon, Elisa Thébault, Gérard Lacroix et Isabelle Gounand *in prep*.