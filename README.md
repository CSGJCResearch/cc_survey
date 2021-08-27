# More Community, Less Confinement

In 2018, The Council of State Governments (CSG) Justice Center sent out an annual survey to corrections departments in all 50 states to collect data on the impact of supervision revocations of supervision on prison populations. The More Community, Less Confinement study found that incarceration due to probation and parole violations make up between one third and one half of of state prison admissions nationwide, with wide variation across states. With continued partnership with the Correctional Leaders Association and support from Arnold Ventures, we will be collecting and reporting these numbers annually through 2022.  
The full report and state-by-state data visualizations can be found on our website.

## Repository Structure
    
    |-- clean.R               # imports and cleans survey data
    |-- multiple_imputation.R # imputes missing values, calculates national estimates and cost calculations

    |-- data
      |-- Data for web team 2021 v13.xlsx # state-level admissions, population, and cost data - 2018, 2019, 2020

## Multiple Imputation Methodology

To develop national level estimates from the state-by-state figures, the CSG Justice Center used multiple imputation to address missing state-level data. The decision to impute these missing values was decided upon in order to prevent excluding states' reported populations from the national estimates. Because data were not missing at random, multiple imputation was used to allow for the uncertainty about the missing data by accounting for variability due to unknown values.

Multiple imputation uses random draws from the conditional distribution of the target variable given other viarables. The imputation was repeated M = 5 times resulting in completed datasets that were summarized into national population estimates. These were then analyzed with a standard regression approach, and the parameter estimates were averaged over the multiple imputations to obtain the best estimate rather than using a single imputation. Finally, to obtain 95% confidence intervals, the variance-covariance matrix of the averaged parameter estimates adjusted for variability due to imputation was estimated (Harrell, 2010).

### References

Harrell, Jr., Frank E. Regression modeling strategies: with applications to linear models, logistic regression, and survival analysis. 2010.

## Collaborations
[Supervision Success: Recidiviz Cost Calculator](https://github.com/Recidiviz/supervision-success-component)

[Justice Counts: Recidiviz Components](https://github.com/Recidiviz/justice-counts-components)

[The Council of State Governments GitHub](https://github.com/csg-org)

## License
[CSG Justice Center](https://csgjusticecenter.org/)