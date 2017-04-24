Run the r script: 1_TOP_preparedata.R It sources the preparedata.R script, 
which creates an imput dataset for Latent Gold. 

Next, it creates a LG syntax file which it used to simulate data using Latent Gold,
by placing the parameter values in the right places in the brew file. It creates 
sample datasets with varying scenario's as shown in 'scens'.

The scenario values are translated into logit coefficients and placed in the correct
place in the brew file ("brew_data.brew"). Next, the "preparedata_template.R" is used
to use the brew file as syntax in Latent Gold and run it. 