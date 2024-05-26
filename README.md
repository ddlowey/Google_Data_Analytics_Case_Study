# Google_Data_Analytics_Case_Study
Case study created as part of the [Google Data Analytics Professional Certificate](https://www.coursera.org/professional-certificates/google-data-analytics), showcasting the data analysis process for a fictional scenario with real raw data. Functioning as a junior data analyst for Bellabeat - a wellness company that specialises in health-related products for women - the goal is to unlock new growth opportunities for the company by analysing smart device fitness data and identifying trends in smart device usage, which will help conceptualise a new marketing strategy.

The analysis follows a classic data analysis approach, consisting of the six phases 1.) Ask, 2.) Prepare, 3.) Process, 4.) Analyse, 5.) Share and 6.) Act. It includes: 

* data collection/data management
* data cleaning
* data wrangling
* data exploration/data interpretation
* data visualisation
* communication of the findings and recommendations

The complete set of plots is provided in the 'Plots' folder for reference and is organised in accordance with the chapters from the notebook.

![Bellabeat DB 1](https://github.com/ddlowey/Google_Data_Analytics_Case_Study/assets/169537418/7c9cc515-147a-453a-9bee-3257bd5efecd)
![Bellabeat DB 2](https://github.com/ddlowey/Google_Data_Analytics_Case_Study/assets/169537418/31abf677-a7a2-47d2-a921-3189437e50ba)


# Set Up

**1. Install JupyterLab** 

```
% brew install jupyterlab
```
<br>

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
<br>

**3. Add the dataset to Jupyter**

###### *Note: In order for this to work, you will need a kaggle account. Then go to settings under 'Profile' and create an API token. Finally download the .json-file that provides your username and key.*

Create a notebook in Jupyter and run the following code:

```
!pip install opendatasets

import opendatasets as od
```

Copy the URL from the corresponding dataset (here: [FitBit Fitness Tracker Data](https://www.kaggle.com/datasets/arashnic/fitbit)) and download it by providing the username and key from the .json-file

```
od.download("https://www.kaggle.com/datasets/arashnic/fitbit")
``` 

# License

**CC BY-NC 4.0**

*This work is licensed under [Creative Commons Attribution-NonCommercial 4.0 International](https://creativecommons.org/licenses/by-nc/4.0/deed.en).*


