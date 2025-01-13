import numpy as np

import pandas as pd

import os

import CalculatedFieldSubroutines as cfs

def disengageCount(time_sorted_chassis_df):
    #Create "BinaryDrivingMode" 
    cfs.BinaryDrivingMode(time_sorted_chassis_df)
    #Create "TernaryDrivingModeTransition" variable for time_sorted chassis
    cfs.TernaryDrivingModeTransition(time_sorted_chassis_df)
    #Pull ternary from chassis 
    time_sorted_chassis_df["TernaryDrivingModeTransition"].value_counts()
    #L
    chassis_new = time_sorted_chassis_df[time_sorted_chassis_df["TernaryDrivingModeTransition"] == -1]
     #Create empty lists for red, blue, and green 
    ads_index = pd.read_csv("ads_data_index_w_percentage.csv")
    #Get ids
    blue_ids = ads.index[ads.index["route"]=="Blue Route"]["groupMetadataID"]
    #red_ids = ads.index[ads.index["route"]=="RedRoute"]["groupMetadataID"]
    #green_ids = ads.index[ads.index["route"]=="GreenRoute"]["groupMetadataID"]
    #Separate disengagements based on route type
    blue_df = chassis_new[chassis_new["groupMetadataID"].isin(blue_ids)]
    #red_df = chassis_new[chassis_new["groupMetadataID"].isin(red_ids)]
    #green_df = chassis_new[chassis_new["groupMetadataID"].isin(green_ids)]
    #
    print(blue_df.shape[0])
    #print(red_df.shape[0])
    #print(green_df.shape[0])
    
#def rowPercent(time_sorted_chassis_df):
    #time_sorted_chassis_df["rowPercent"] = 