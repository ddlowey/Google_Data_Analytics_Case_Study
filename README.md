# Google_Data_Analytics_Case_Study
Case study created as part of the [Google Data Analytics Professional Certificate](https://www.coursera.org/professional-certificates/google-data-analytics), showcasting the data analysis process for a fictional scenario with real raw data. Functioning as a junior data analyst for Bellabeat - a wellness company that specialises in health-related products for women - the goal is to unlock new growth opportunities for the company by analysing smart device fitness data and identifying trends in smart device usage, which will help conceptualise a new marketing strategy.

The analysis follows a classic data analysis approach, consisting of the six phases 1.) Ask, 2.) Prepare, 3.) Process, 4.) Analyse, 5.) Share and 6.) Act. It includes: 

* data collection/data management
* data cleaning
* data wrangling
* data exploration/data interpretation
* data visualisation
* communication of the findings and recommendations for future marketing

# Usage

**1. Install JupyterLab** 

```
brew install jupyterlab
```

**2. Add IRkernel to Jupyter** 

In the terminal open the R shell

```
% R
```

Install the 'IRkernel'-package 

```
% install.packages("IRkernel")
```

Run the ```ìnstallspec()``` function from the 'IRkernel'-package

```
% IRkernel::installspec()
```

**3. Add the dataset to Jupyter**

*Note: In order for this to work, you will need a kaggle account. Then go to settings under 'Profile' and create and API token. Finally download the .json-file that provides your username and key.*
<br></br>
Create a notebook in Jupyter and run the following code:

```
!pip install opendatasets

import opendatasets as od
```

Copy the URL from the corresponding dataset (here: [FitBit Fitness Tracker Data](https://www.kaggle.com/datasets/arashnic/fitbit)) and download it. Provide the username and key from the .json-file

```
od.download("https://www.kaggle.com/datasets/arashnic/fitbit")
``` 

# License

**CC BY-NC 4.0**

*This work is licensed under [Creative Commons Attribution-NonCommercial 4.0 International](https://creativecommons.org/licenses/by-nc/4.0/deed.en).*


