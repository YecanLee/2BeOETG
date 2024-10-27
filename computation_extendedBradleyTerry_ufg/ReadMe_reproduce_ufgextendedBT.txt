# Explanation to run the files to obtain the results.

This folder contains all files to reproduce the results of the extended Bradley-Terry model and the ufg-depth based on the evaluated and already computed metrices coherence, diversity and perplexity.
Each folder one benchmarking problem corresponding to one dataset. Here, 
- wikitext corresponds to WikiText-103
- wikinews corresponds to Wikinews
- books corresponds to Book
- all_datasets corresponds to the combined datasets WikiText-103, Wikinews and Book

To reproduce the file please first install all necessary packages in _setup_session.R file. Some remarks on the loaded packages there:
- For the computation of the linear programs we used the R-interface of the gurobi optimizer, see here (accessed: 01.02.2024). This is a commercial solver that offers a free academic license, which can be found here (accessed: 01.02.2024). To install this package, please follow the instructions there. A documentation can be found here (page 643ff) (accessed: 01.02.2024).
- The packages oofos and ddandrda are still under development and have to be downloaded from github.

To compute the extended Bradley Terry file:
- Please select one of the four benchmarking problems above and store the file ..._bt.R from the corresponding folder.
- Please save the metric evaluation calculation as file "results_with_pareto_efficiency.csv" in the same folder.
- Install all packages in _setup_session.R
- Now you can run the code. All results and intermediate results are stored in the same folder.

To compute the ufg-depth for WikiText-103 dataset:
- Please store the file wikitext_ufg.R from the wikitext folder.
- Please save the metric evaluation calculation as file "results_with_pareto_efficiency.csv" in the same folder.
- Install all packages in _setup_session.R
- Now you can run the code. All results and intermediate results are stored in the same folder.

