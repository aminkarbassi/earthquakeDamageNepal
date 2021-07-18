# earthquakeDamageNepal
Clustering and classification for structural damage portfolio after 2015 earthquake in Nepal

Following the 7.8 magnitude Gorkha earthquake on April 25, 2015, Nepal conducted a massive household survey using mobile technology to assess building damage in earthquake-affected districts. The data was collected between January 2016 and May 2016. 

This is a clusterring and classification data analysis project in R Markdown. I have used the Structural dataset, which includes floor type, roof type, foundation type, and other structural-related information for each building along with damage level. Data cleaning was done separately, and the dataset to be processed was stored in an RData file. It should be noted that this file is an extract of the original dataset, which contains over 750’000 observations.

For this project, I have used the Uniform Manifold Approximation and Projection (UMAP) as the nonlinear dimensionality reduction algorithm to visualize the complex data with 24 variables in the 2D space. Partitioning Around Medoids (PAM) was my choice for clustering the buildings which were divided originally into five damage grades. For the classification part, I tried Decision trees and Random Forest models to build classifiers to predict the damage grade for unseen observations using the 24 available variables.
