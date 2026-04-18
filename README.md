# Biomarkers and Alzheimer's - A Shiny Application

## Project Summary
This Shiny application is intended to provide users with a way to compare biomarkers in patients with no or mild cogntive impairment. Users can also select whether to consider Age or the APOE genotype (characterized into risk categories) as factors. The application then displays plots and statistical testing for the user's selections.

This project was a collaboration by Pierce Lovinger and Jeremy Reimann, as an assignment for Brown University's BHDS 2010 course, "Statistical Programming in Health Data Science". This GitHub is hosted on Pierce Lovinger's account. The final application is hosted on shinyapps.io

## Access the Full Application here
https://pilovinger.shinyapps.io/Alzheimers_Exploration/

## Raw Data Description
The data set contains biomarker data for 333 volunteers enrollled in longitudinal studies for healthy aging and dementia. Most of the variables are various biomarkers and proteins pulled from Cerebrospinal Fluid (CSF) of the participants. In addition to these variables, the authors of the dataset also classified the cognitive status of the participants as either normal or impaired and provided their genotype and gender.

For privacy reasons, the dataset is not included with the Github and is only pulled at app creation

## Team Member Roles
Pierce - ReadMe, shiny app layout, graphing functionality

Jeremy - Statistical testing, summary functionality, markdown file submission with project

## Using the Application
The app can be run at the above link

To use the code for the app, please do the following:

1. Clone the GitHub repository using the URL

2. Download the code to your local computer

3. Download the dataset at the link below

4. Download R and R Studio, if not yet on the local computer

5. Launch R Studio and open the app.R file

6. Install the required dependencies using install.packages()

7. Launch the code using the "Run" button in the file window

## Version Control Workflow
GitHub for this code is hosted on Pierce Lovinger's personal account. Pull Requests were enforced, meaning that any change required a separate branch be created and later merged to main. These branches were created and merged as needed. Pull requests were reviewed by both Pierce and Jeremy, with approval required from the other author before merges were allowed (enforced by GitHub rules). In the event of a conflict in merging, the code was scanned and edited in GitHub to ensure proper merging.

## Citation for Data
Craig-Schapiro R et al. Multiplexed immunoassay panel identifies novel CSF biomarkers for Alzheimer's disease diagnosis and prognosis. PLoS One. 2011 Apr 19;6(4):e18850. doi: 10.1371/journal.pone.0018850. PMID: 21526197; PMCID: PMC3079734.

## Disclaimer
### THIS APP IS NOT MEDICAL ADVICE
This application and all of its contents, including images, code, text, and graphics, is intended for educational purposes only. It should not be taken as medical advice, diagnosis, or treatment, nor should it be taken as a substitute for such. 

Always consult a licensed physician or other qualified health professional with any questions regarding medical conditions and treatments. Never disregard medical advice because of information gained from this application. 

By using this application in any way, including, but not limited to, launching it on the Web Browser or examining or altering the code, you agree that the authors assume no liability for diagnosis or treatment of any medical condition. 