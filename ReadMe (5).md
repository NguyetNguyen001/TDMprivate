# YSU Data Mine - DriveOhio Project - ReadMe

## 2024 Fall - 2025 Spring Team

Godwin Ampadu (Fall & Spring)

Ryan Coldren (Fall & Spring) (Spring TA)

Diana Drobnjak (Fall & Spring)

Vincent Hepola (Fall only) (Fall TA)

Jasmine Le (Fall & Spring)

Leslie Wedraogo (Fall & Spring)

## Description

DriveOhio, an initiative under the Ohio Department of Transportation, is interested in improving the capabilities of automated vehicles in rural environments. To support this effort, they sent out vans equipped with automated driving systems (ADS) into rural parts of the Athens and Vinton counties of Ohio to collect data, and have asked us at YSU for help analyzing this data. They are particularly interested in understanding where and why the ADS is not performing well in rural areas and were interested in seeing what insights we could gather about moments when the driver inside the vans felt uncomfortable with what the ADS was doing and switched it off to take manual control (these moments are referred to as disengagements). They are also interested in understanding the behavior of the van under ADS control as opposed to manual control (what we refer to as automatic vs manual driving mode).

Using the 330 van runs worth of data and some of the simpler quantitative variables collected during the runs (speed, brake %, throttle %, steering %, and positional standard deviation), we performed basic EDA with line charts, box plots, scatter plots, histograms, and other visualizations to examine the differences between ADS and manual control, and disengagements and non-disengagements. To enhance our EDA, we trained unsupervised machine learning models such as K-Means and Fuzzy C to differentiate the disengagements into different types and made detailed map plots and animations of the disengagements. We also trained supervised machine learning models such as Logistic Regression, K-Nearest Neighbors, and Decision Trees to predict driving modes and disengagements (with a particular emphasis on training models to predict disengagements using data BEFORE they happened). Finally, we started investigating a hypothesis proposed by our DriveOhio mentors that after instances where the GPS switches solution type while under ADS control, the vans might 'wobble' back and forth on the road (although we did not make much headway into this).

## First Steps

Before discussing how to get access to the data and working with it, first (once you get access to the DATX-DriveOhio Microsoft Teams group) follow the tutorials made by the 2023 Fall - 2024 Spring Team and Purdue. These can be found in the GettingStarted Section of the General channel of the Teams. Complete tutorials 01-08 ('08 Setup R and RStudio' is optional unless you want to use R. Note: We HIGHLY encourage you to primarily use Python for your work since the majority of our codebase is written in Python.)

Once you reach '09 Setup Git and Github', proceed until you reach the part of the tutorial in the 'Setting up ssh Keys' section that says 'Run the following cat command and then Copy (Ctrl + Shift + C) the output of this command to the clipboard.' You will notice that the cat command is missing; use this command:

cat ~/.ssh/id_ed25519.pub

When you reach the 'Cloning the Repository' section of the tutorial, do not use the listed link to the repository; it is not the repository our team used. Use this one instead:
 
https://github.com/NguyetNguyen001/TDMprivate

Right after you complete the part of the tutorial where you clone the repository to your $HOME directory on your Amazon WorkSpace, move the repository (called 'TDMprivate') to your WorkSpace's Desktop. Much of our codebase depends on the repository being on the Desktop.

Because the repository is now on the Desktop, on Step 2 of the Working in GitHub section, you will instead need to run:

cd ~/Desktop/TDMprivate

After completing all these steps, you should be ready to get access to the data.

## Accessing the Data

Tutorial '07 Use DynamoDB' hints at accessing the DriveOhio data from their DyanamoDB database using a Python library called 'boto3'. Learning how to query data out of this database using this library will likely be essential to continuing the project. We have built notebooks to query data from the database for our group's own usage (which we will discuss later), but to query data which we had not looked at, you will need to learn 'boto3'.

For a tutorial on using 'boto3' to query the data, read the 'Query_Structure.ipynb' Jupyter notebook in the 'Documented_Code/Fully_Documented' folder of Ryan's branch of the GitHub repository (https://github.com/NguyetNguyen001/TDMprivate).

To query the data exactly how we did, read the documentation of and run the 'Mass_Data_Query_v2.ipynb' notebook in the 'Documented_Code/Fully_Documented' folder of Ryan's branch of the GitHub repository. This will pull the data for all 330 van runs (each assigned a unique ID called a 'groupMetadataID') into a folder called 'data' in your 'TDMprivate' folder. 

Each 'groupMetadataID' will get a folder inside the 'data' folder, and each 'groupMetadataID' folder will have 4 folders inside it corresponding to different data 'topics' for that 'groupMetadataID'. Inside each of the 'topic' folders will be a .csv.

Note: The 'Mass_Data_Query_v2.ipynb' notebook will take a while to run, probably more than 10 hours. There is a lot of data to download.

## Working with the Data

Once you get access to the data, the world is your oyster. However, you will quickly realize that the data is quite complex and requires a lot of cleaning and manipulation to be used. Our team developed a communal Jupyter notebook called 'CalculatedFieldSubroutines.ipynb' to act as a container for tools (Python functions) to access and manipulate the data (specifically, there are data access functions, functions to create a lot of different calculated fields for the data, and some machine learning related functions). All the functions in here are documented (describing exactly what they require to work, and what they will output), and we highly encourage you to look at them before building your own tools to work with the data; it could save you a lot of time.

An example of a data access and manipulation tool are:

'retrieve_gmID_topic' - 

Allows one to access the data for a topic for a groupMetadataID quickly as a Pandas DataFrame, without having to work with the directory                         structure of 'TDMprivate' (assumes one ran the 'Mass_Data_Query_v2.ipynb' notebook)

'ProgressAlongRoute_v2' - 

A function that allows one to represent a van's position along a route with a single value instead of a set of latitude and                                   longitude coordinates

This notebook is meant to be downloaded as a '.py' file, which should then be placed in the same directory where you have the code you are working on. If you do this, you can import it as a Python module for easy access to the functions inside. We often imported it like this:

import CalculatedFieldSubroutines as cfs

## Notable Work Inside Our Branches

Godwin's Branch - This branch (MyCode) contains all R scripts and supporting Python code used to build, evaluate, and visualize logistic regression models for both BinaryDrivingMode and BinaryDisengagementExpanded.

- MyCode/3_Routes
  - Contains R scripts for training logistic regression models on Red, Green, and Blue routes.

  - Predicts Auto Mode (1) vs. Manual Mode (0) using key features: speedMps, throttlePercentage, brakePercentage, steeringPercentage, and latLonStd.

  - Evaluated using accuracy, precision, recall, and confusion matrices.

- MyCode/0Disengagement_0Sec/, 2Disengagement_2Sec/, 4Disengagement_4Sec/
  - Each folder contains R scripts used to detect disengagements with different time windows.

  - Applied downsampling to address class imbalance in disengagement labels.
 
  - Performance tracked before and after balancing using confusion matrices, recall, precision, and F1-score.

Ryan's Branch - (All my work, documented or not, can be found in the 'my_code' folder of my branch)

- Documented_Code/Fully_Documented/Query_Structure.ipynb
  - Tutorial on how to query for the data in DyanamoDB using Boto3 in Python

- Documented_Code/Fully_Documented/Mass_Data_Query_v2.ipynb 
  - Notebook to query the data exactly how our group did

- Documented_Code/Fully_Documented/CalculatedFieldSubroutines.ipynb
  - Container for tools to access and manipulate the data; intended to be downloaded as a .py file and placed in the directories where one is coding to be called in as a Python module 

- Documented_Code/Lightly_Documented/Creating_Preprocessed_Data_Folder_v3.ipynb
  - Notebook for preprocessing the data exactly like our group did (merges the 'chassis' and 'best_pose' topics, creates lots of new columns, one-hot encodes variables, calculates trailing moving averages of variables, etc.)

- Documented_Code/Lightly_Documented/Creating_Preprocessed_Data_Folder_v3_reduced.ipynb
  - Identical to the 'Creating_Preprocessed_Data_Folder_v3.ipynb' notebook, except it also reduces the size of the preprocessed data by ~90%

- Documented_Code/Lightly_Documented/Disengagement_Decision_Trees_v3_reduced.ipynb
  - Notebook for utilizing Decision Trees, with hyperparameter tuning and 10-fold cross validation, to predict disengagements (using the reduced preprocessed data from 'Creating_Preprocessed_Data_Folder_v3_reduced.ipynb')

- Documented_Code/Lightly_Documented/Disengagement_Clustering.ipynb
  - Contains preliminary work on clustering disengagements using K-Means

- Documented_Code/Lightly_Documented/MapDisengagementVizSetup_Redo.ipynb
  - A notebook for plotting disengagements on maps and looking for areas of higher disengagement density 

- Documented_Code/Lightly_Documented/New_EDA.ipynb
  - A notebook for doing box plot analysis of the driving modes and disengagements as well as making correlation plots of the variables

- Documented_Code/Lightly_Documented/POI_Animation.ipynb
  - A notebook for making plots and animations of the vans around user specified points of interest

- Documented_Code/Lightly_Documented/Wobble_Hypothesis.ipynb
  - Contains preliminary work on testing the 'Wobble' Hypothesis

Diana's Branch -
(The following list contains significant codes that were at least partially labelled; all other codes can be found in my main "my_code" branch)

- my_code/Documented_Code/DisengageTreeTakeTwo.ipynb

  - Despite the name, this code was used to run decision trees to predict the binary driving mode, Albeit without using tenfold cross-validation and Bayesian Optimization


- my_code/Documented_Code/DrivingMode_Decision_Trees_a3.ipynb
  
  - Used to build a Decision Tree to determine the binary driving mode, using tenfold cross-validation                                                                                        and Bayesian Optimization

- my_code/Documented_Code/solFunction.ipynb

  - Used to run a function that created the “SolChange” and “Changesol” variables


Jasmine's Branch -
(This branch has all my Python notebooks throughout this year, documented or not, an be found in the 'my_code' folder of my branch)

- MyCode/Documented_Code/Clustering.ipynb

  - Used to run k-means on binary driving mode data

- MyCode/Documented_Code/Segments.ipynb

  - Used for Data Analysis on segmentation of the data sets 

- MyCode/Documented_Code/fuzzy-c.ipynb

  - Soft unsupervised clustering on binary driving modes per route 

Leslie's Branch -
This branch(my_code) contains all the Python notebooks used throughout the project, including fully documented, partially documented, and exploratory versions.

- my_code/Optimal_K_Selection.ipynb
This notebook was used to determine the optimal value of K for the KNN model in predicting binary driving mode. It utilizes numpy's argmax function to identify the value of K that yields the highest validation accuracy.

- my_code/KNN_V1_Greenroute_documented.ipynb
This notebook applies the KNN model to the ChassisBestPoseMatchedTime dataset to predict binary driving mode. Using the optimal K value, it trains on approximately 80% of the Greenroute group metadata IDs and tests on the selected 5 gmIDs. The same approach was applied to all routes.

- my_code/KNN_V2_Greenroute_documented.ipynb
An extension of V1, this version additionally plots the model’s predictions on an actual route map using matplotlib and cartopy for visualization. The same 80%/5 train/test split was maintained across all routes.

- my_code/KNN_V3_Greenroute_documented.ipynb
This notebook tests the KNN model on a single gmID (1 test sample) while training on the remaining 80%. It also includes route-level map plotting. This version is partially documented.

- my_code/KNN_Disen_Greenroute_documented.ipynb
This notebook applies the KNN model to the Preprocessed_Moving_Data_v3 dataset to detect disengagement events. It uses the optimal K value and visualizes predicted disengagements on the actual route map. This version is partially documented and follows the same modeling approach used across all routes.




```python

```
