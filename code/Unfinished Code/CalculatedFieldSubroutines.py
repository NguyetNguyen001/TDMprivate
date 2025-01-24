#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy as np

import pandas as pd

import random

import os


# In[3]:


def BinaryDrivingMode( chassis_df ):

    '''
    Arguments:
    
        chassis_df -> A Pandas dataframe containing the data for the chassis topic of a singular groupMetadataID.
        
    Internal Task (subroutine):
    
        Adds an extra column to the Pandas dataframe 'chassis_df' argument called 'BinaryDrivingMode'. This column is a 
        calculated column based on the 'drivingMode' column within the 'chassis_df' argument, having a value of 0 when 
        'drivingMode' has a value of 'COMPLETE_MANUAL' or 'EMERGENCY_MODE', or a value of 1 when 'drivingMode' has a value 
        of 'COMPLETE_AUTO_DRIVE'

    Output:

        No output. (Only modifies the input 'chassis_df' argument) 

    Purpose:

        Create a column in the chassis dataframe for a singular groupMetadataID that indicates whether the car is in automatic
        or manual driving mode at a given moment and is easier to work with than the 'drivingMode' column.
    '''

    drive_mode_lst = chassis_df[ 'drivingMode' ].tolist()

    binary_drive_mode_lst = []
    
    for drive_mode in drive_mode_lst:

        if ( ( drive_mode == 'COMPLETE_MANUAL' ) or ( drive_mode == 'EMERGENCY_MODE' ) ):

            binary_drive_mode_lst.append( 0 )

        elif ( drive_mode == 'COMPLETE_AUTO_DRIVE' ):

            binary_drive_mode_lst.append( 1 )

        else:

            raise Exception( f'Unknown driving mode: { drive_mode }' )

    chassis_df[ 'BinaryDrivingMode' ] = binary_drive_mode_lst


# In[4]:


def TernaryDrivingModeTransition( time_sorted_chassis_df ):

    '''
    Arguments:
    
        time_sorted_chassis_df -> A Pandas dataframe containing the data for the chassis topic of a singular groupMetadataID 
                                  that must satisfy two conditions:

                                      1. Must have had the 'BinaryDrivingMode' function run on it previously.

                                      2. Must be sorted by time using the Pandas '.sort_values' method.
        
    Internal Task (subroutine):
    
        Adds an extra column to the Pandas dataframe 'time_sorted_chassis_df' argument called 'TernaryDrivingModeTransition'. 
        This column is a calculated column based on the 'BinaryDrivingMode' column within the 'time_sorted_chassis_df' argument,
        having a value of -1 on rows where 'BinaryDrivingMode' switches from 1 to 0, a value of 1 on rows where 
        'BinaryDrivingMode' switches from 0 to 1, and a value of 0 on rows where 'BinaryDrivingMode' does not change. 

    Output:

        No output. (Only modifies the input 'time_sorted_chassis_df' argument) 

    Purpose:

        Create a column in the chassis dataframe for a singular groupMetadataID that indicates at what moments the car switches to
        automatic or manual driving mode or does not change driving mode. An alternative metric for the 'BinaryDrivingMode' column.
    '''

    binary_drive_mode_lst = time_sorted_chassis_df[ 'BinaryDrivingMode' ].tolist()

    ternary_drive_mode_trans_lst = []
    
    for index in range( 1, len( binary_drive_mode_lst ) ):

        drive_mode_trans = binary_drive_mode_lst[ index ] - binary_drive_mode_lst[ index - 1 ]

        ternary_drive_mode_trans_lst.append( drive_mode_trans )

    ternary_drive_mode_trans_lst = [ 0 ] + ternary_drive_mode_trans_lst

    time_sorted_chassis_df[ 'TernaryDrivingModeTransition' ] = ternary_drive_mode_trans_lst


# In[5]:


def LatLonTotalStdDev( best_pose_df ):

    '''
    Arguments:
    
        best_pose_df -> A Pandas dataframe containing the data for the best_pose topic of a singular groupMetadataID.
        
    Internal Task (subroutine):
    
        Adds an extra column to the Pandas dataframe 'best_pose_df' argument called 'LatLonTotalStdDev'. This column is a 
        calculated column based on the 'latitudeStdDev' and 'longitudeStdDev' columns within the 'best_pose_df' argument, 
        containing a single float value representing a magnitude of the values within the 'latitudeStdDev' and 'longitudeStdDev'
        columns for each row. This magnitude is calculated via the fomula for Euclidean distance (d = sqrt(x**2 + y**2)), where the
        x and y values are the values within the 'latitudeStdDev' and 'longitudeStdDev' columns respectively. (The units for the 
        values in the added 'LatLonTotalStdDev' column, as well as in the 'latitudeStdDev' and 'longitudeStdDev' columns, is meters)

    Output:

        No output. (Only modifies the input 'best_pose_df' argument) 

    Purpose:

        Create a column in the best_pose dataframe for a singular groupMetadataID that combines the values for the standard 
        deviation in latitude and longitude into a single easier to work with value.
    '''

    lat_stddev_lst = best_pose_df[ 'latitudeStdDev' ].tolist()

    lon_stddev_lst = best_pose_df[ 'longitudeStdDev' ].tolist()

    def planar_distance( x, y ): return ( x ** 2 + y ** 2 ) ** ( 1 / 2 )

    latlon_total_stddev_lst = []

    for lat_stddev, lon_stddev in zip( lat_stddev_lst, lon_stddev_lst ):

        latlon_total_stddev = planar_distance( lat_stddev, lon_stddev )

        latlon_total_stddev_lst.append( latlon_total_stddev )

    best_pose_df[ 'LatLonTotalStdDev' ] = latlon_total_stddev_lst


# In[6]:


def ChassisBestPoseMatchedTime( same_gmID_chassis_df, same_gmID_best_pose_df ):

    '''
    Arguments:
    
        same_gmID_chassis_df -> A Pandas dataframe containing the data for the chassis topic of a singular groupMetadataID.
                                Requirements:

                                    1. Must be for the same groupMetadataID as the 'same_gmID_best_pose_df' argument.

        same_gmID_best_pose_df -> A Pandas dataframe containing the data for the best_pose topic of a singular groupMetadataID.
                                  Requirements:

                                      1. Must be for the same groupMetadataID as the 'same_gmID_chassis_df' argument.
        
    Internal Task (subroutine):
    
        Adds an extra column to both the 'same_gmID_chassis_df' and 'same_gmID_best_pose_df' Pandas dataframe arguments called 
        'ChassisBestPoseMatchedTime'. 
        
        The 'ChassisBestPoseMatchedTime' column added to the 'same_gmID_chassis_df' argument contains every time value within the
        'time' column of 'same_gmID_chassis_df' rounded to the nearest time value to them in the 'time' column of the 
        'same_gmID_best_pose_df' argument.

        The 'ChassisBestPoseMatchedTime' column added to the 'same_gmID_best_pose_df' argument is an exact copy of the 'time' 
        column in 'same_gmID_best_pose_df'.

    Output:

        No output. (Only modifies the input 'same_gmID_chassis_df' and 'same_gmID_best_pose_df' arguments) 

    Purpose:

        The 'ChassisBestPoseMatchedTime' column added to both the 'same_gmID_chassis_df' and 'same_gmID_best_pose_df' arguments
        allow the 'same_gmID_chassis_df' and 'same_gmID_best_pose_df' Pandas dataframes to be inner merged on the 
        'ChassisBestPoseMatchedTime' column using the Pandas 'merge' function. Ex:

            merged_df = pd.merge( same_gmID_chassis_df, same_gmID_best_pose_df, on = 'ChassisBestPoseMatchedTime', how = 'inner' )

        This is useful because it allows one to compare the data entries within the chassis topic with the data entries within the 
        best_pose topic that were recorded at approximately the same time.

        (The chassis and best_pose topics for a groupMetadataID are initially not mergable on time due to the fact that data in the 
        chassis topic is recorded at a much higher time frequency than the data in the best_pose topic [every 0.03 seconds compared
        to every 1 second], and that the probabilty of a row of data being recorded in the chassis and best_pose topics at the 
        exact same nanosecond [which is the unit time is recorded in within the topcis] is exceptionally low. The 
        'ChassisBestPoseMatchedTime' column introduced by this function is one way to solve these issues)
    '''

    chassis_time_array = np.array( same_gmID_chassis_df[ 'time' ] )

    best_pose_time_array = np.array( same_gmID_best_pose_df[ 'time' ] )

    chassis_best_pose_matched_time_lst = []

    for chassis_time in chassis_time_array:

        time_diff_array = best_pose_time_array - chassis_time

        abs_time_diff_array = np.abs( time_diff_array )

        min_index = np.where( abs_time_diff_array == np.min( abs_time_diff_array ) )

        chassis_best_pose_matched_time = best_pose_time_array[ min_index ][ 0 ]

        chassis_best_pose_matched_time_lst.append( chassis_best_pose_matched_time )

    same_gmID_chassis_df[ 'ChassisBestPoseMatchedTime' ] = chassis_best_pose_matched_time_lst

    same_gmID_best_pose_df[ 'ChassisBestPoseMatchedTime' ] = same_gmID_best_pose_df[ 'time' ]


# In[7]:


def ProgressAlongRoute( best_pose_df, time_sorted_reference_best_pose_df):

    '''
    No documentation for now, needs to be fixed
    '''

    reference_ProgressAlongRoute_list = [ 0 ]

    reference_latitude_array = np.array( time_sorted_reference_best_pose_df[ 'latitude' ] )

    reference_longitude_array = np.array( time_sorted_reference_best_pose_df[ 'longitude' ] )

    for index in range( len( reference_latitude_array[ : -1 ] ) ):

        reference_ProgressAlongRoute_list.append( reference_ProgressAlongRoute_list[ index ] + \
                                                  np.sqrt( ( reference_latitude_array[ index + 1 ] - reference_latitude_array[ index ] ) ** 2 + \
                                                           ( reference_longitude_array[ index + 1 ] - reference_longitude_array[ index ] ) ** 2 ) )

    reference_ProgressAlongRoute_array = np.array( reference_ProgressAlongRoute_list )

    reference_ProgressAlongRoute_array = reference_ProgressAlongRoute_array / np.max( reference_ProgressAlongRoute_array )

    #

    current_latitude_array = np.array( best_pose_df[ 'latitude' ] )

    current_longitude_array = np.array( best_pose_df[ 'longitude' ] )

    current_ProgressAlongRoute_list = []

    for latitude, longitude in zip( current_latitude_array, current_longitude_array ):

        distance_analog_array = ( reference_latitude_array - latitude ) ** 2 + ( reference_longitude_array - longitude ) ** 2

        min_distance_index = np.where( distance_analog_array == np.min( distance_analog_array ) )

        current_ProgressAlongRoute_list.append( reference_ProgressAlongRoute_array[ min_distance_index ][ 0 ] )

    best_pose_df[ 'ProgressAlongRoute' ] = current_ProgressAlongRoute_list


# In[8]:


def ProgressAlongRoute_v2( time_sorted_best_pose_df, time_sorted_reference_best_pose_df = 'auto', num_of_partitions = 100, \
                           max_distance_coefficient = 5 ):

    if ( time_sorted_reference_best_pose_df == 'auto' ):

        if ( give_route( time_sorted_best_pose_df[ 'groupMetadataID' ][ 0 ] ) == 'Red' ):

            reference_best_pose_gmID = '9798fe24-f143-11ee-ba78-fb353e7798cd'

        elif ( give_route( time_sorted_best_pose_df[ 'groupMetadataID' ][ 0 ] ) == 'Green' ):

            reference_best_pose_gmID = '3a7dc9a6-f042-11ee-b974-fb353e7798cd'

        else:

            reference_best_pose_gmID = '3d2a80f0-ec81-11ee-b297-3b0ad9d5d6c6'
            
        time_sorted_reference_best_pose_df = retrieve_gmID_topic( gmID = reference_best_pose_gmID, 
                                                                  topic = '/apollo/sensor/gnss/best/pose' )

        time_sorted_reference_best_pose_df.sort_values( 'time' )

    reference_latitude_array = np.array( time_sorted_reference_best_pose_df[ 'latitude' ] )

    reference_longitude_array = np.array( time_sorted_reference_best_pose_df[ 'longitude' ] )

    reference_delta_latitude_array = np.diff( reference_latitude_array )

    reference_delta_longitude_array = np.diff( reference_longitude_array )

    reference_delta_distance_analog_array = np.sqrt( reference_delta_latitude_array ** 2 + reference_delta_longitude_array ** 2 )

    reference_ProgressAlongRoute_list = [ 0 ]

    for index, delta_distance_analog in enumerate( reference_delta_distance_analog_array ):

        running_delta_distance_analog = reference_ProgressAlongRoute_list[ index ] + delta_distance_analog

        reference_ProgressAlongRoute_list.append( running_delta_distance_analog )
    
    reference_ProgressAlongRoute_array = np.array( reference_ProgressAlongRoute_list )

    reference_ProgressAlongRoute_array = reference_ProgressAlongRoute_array / np.max( reference_ProgressAlongRoute_array )

    #

    partition_size = 1 / num_of_partitions

    reference_ProgressAlongRoute_partition_array = reference_ProgressAlongRoute_array // partition_size

    reference_ProgressAlongRoute_partition_array[ reference_ProgressAlongRoute_partition_array == \
                                                  num_of_partitions ] = num_of_partitions - 1

    #

    latitude_array = np.array( time_sorted_best_pose_df[ 'latitude' ] )

    longitude_array = np.array( time_sorted_best_pose_df[ 'longitude' ] )

    ProgressAlongRoute_list = []

    ProgressAlongRoute_partition_list = []
    #

    avg_reference_delta_distance_analog = np.median( reference_delta_distance_analog_array )

    max_distance_analog = avg_reference_delta_distance_analog * max_distance_coefficient

    #

    full_scan = True

    for index, ( latitude, longitude ) in enumerate( zip( latitude_array, longitude_array ) ):

        if ( full_scan == True ):

            distance_analog_array = np.sqrt( ( reference_latitude_array - latitude ) ** 2 + \
                                             ( reference_longitude_array - longitude ) ** 2 )

            min_distance_analog = np.min( distance_analog_array )

            if ( min_distance_analog > max_distance_analog ):

                ProgressAlongRoute_list.append( np.nan )

                ProgressAlongRoute_partition_list.append( np.nan )

                continue

            full_scan = False

            min_distance_analog_index = np.where( distance_analog_array == min_distance_analog )

            assigned_ProgressAlongRoute = reference_ProgressAlongRoute_array[ min_distance_analog_index ][ 0 ]

            assigned_ProgressAlongRoute_partition = reference_ProgressAlongRoute_partition_array[ min_distance_analog_index ][ 0 ]

        else:

            reference_subset_middle_parititon_num = ProgressAlongRoute_partition_list[ index - 1 ]

            reference_subset_lower_partition_num = reference_subset_middle_parititon_num - 1

            reference_subset_upper_partition_num = reference_subset_middle_parititon_num + 1

            if ( reference_subset_lower_partition_num < 0 ):

                reference_subset_lower_partition_num = num_of_partitions - 1

            if ( reference_subset_upper_partition_num > num_of_partitions - 1 ):

                reference_subset_upper_partition_num = 0

            reference_subset_indexes = np.where( ( reference_ProgressAlongRoute_partition_array == reference_subset_lower_partition_num ) | \
                                                 ( reference_ProgressAlongRoute_partition_array == reference_subset_middle_parititon_num ) | \
                                                 ( reference_ProgressAlongRoute_partition_array == reference_subset_upper_partition_num ) )

            reference_subset_latitude_array = reference_latitude_array[ reference_subset_indexes ]

            reference_subset_longitude_array = reference_longitude_array[ reference_subset_indexes ]

            subset_distance_analog_array = np.sqrt( ( reference_subset_latitude_array - latitude ) ** 2 + \
                                                    ( reference_subset_longitude_array - longitude ) ** 2 )

            min_subset_distance_analog = np.min( subset_distance_analog_array )

            if ( min_subset_distance_analog > max_distance_analog ):

                ProgressAlongRoute_list.append( np.nan )

                ProgressAlongRoute_partition_list.append( np.nan )

                full_scan = True

                continue

            min_subset_distance_analog_index = np.where( subset_distance_analog_array == min_subset_distance_analog )

            reference_subset_ProgressAlongRoute_array = reference_ProgressAlongRoute_array[ reference_subset_indexes ]

            reference_subset_ProgressAlongRoute_partition_array = reference_ProgressAlongRoute_partition_array[ reference_subset_indexes ]

            assigned_ProgressAlongRoute = reference_subset_ProgressAlongRoute_array[ min_subset_distance_analog_index ][ 0 ]

            assigned_ProgressAlongRoute_partition = reference_subset_ProgressAlongRoute_partition_array[ min_subset_distance_analog_index ][ 0 ]

        ProgressAlongRoute_list.append( assigned_ProgressAlongRoute )

        ProgressAlongRoute_partition_list.append( assigned_ProgressAlongRoute_partition )

    time_sorted_best_pose_df[ 'ProgressAlongRoute' ] = ProgressAlongRoute_list

    time_sorted_best_pose_df[ 'PartitionNumber' ] = ProgressAlongRoute_partition_list

    #

    if ( np.any( np.isnan( np.array( time_sorted_best_pose_df[ 'ProgressAlongRoute' ] ) ) ) == True ):

        return True

    else:

        return False


# In[9]:


def NormalizedTime( topic_df ):

    '''
    Arguments:
    
        topic_df -> A Pandas dataframe containing the data for one of the topics of a singular groupMetadataID.
                    Requirements:

                        1. The topic must have a 'time' column
        
    Internal Task (subroutine):
    
        Adds an extra column to the Pandas dataframe 'topic_df' argument called 'NormalizedTime'. This column is simply the 'time'
        column in 'topic_df' with all of its time values subtracted by the minimum time value in the 'time' column.

    Output:

        No output. (Only modifies the input 'topic_df' argument) 

    Purpose:

        The 'NormalizedTime' column created by this function for a topic is essentially that topic's 'time' column, but instead of 
        treating the start, end, and all moments in-between of a groupMetadataID's run as time values in nanoseconds representing 
        datetimes, it treats the start as being at 0 nanoseconds, the end as the total duration of the run in nanoseconds, and all
        the moments in-between as how long it has been since the start of the run in nanoseconds.

        The 'NormalizedTime' column is a quick and dirty way to compare two or more different groupMetadataIDs' runs (on the same 
        route) by time. It should be kept in mind though that time is a poor measure of position along a route, as not all runs
        on a route take the same amount of time, nor do they always start at the exact same position.
    '''

    topic_time_array = np.array( topic_df[ 'time' ] )

    normalized_topic_time_array = topic_time_array - np.min( topic_time_array )

    topic_df[ 'NormalizedTime' ] = list( normalized_topic_time_array )


# In[10]:


def DeltaTime( time_sorted_topic_df ):

    '''
    Arguments:
    
        time_sorted_topic_df -> A Pandas dataframe containing the data for one of the topics of a singular groupMetadataID.
                                Requirements:
                                    
                                    1. The topic must have a 'time' column
                                    
                                    2. Must be sorted by time using the Pandas '.sort_values' method.
        
    Internal Task (subroutine):
    
        Adds an extra column to the Pandas dataframe 'time_sorted_topic_df' argument called 'DeltaTime'. This column contains 
        values representing the time elasped between each of the time values in the 'time' column of 'time_sorted_topic_df'.
        (in nanoseconds)

    Output:

        No output. (Only modifies the input 'time_sorted_topic_df' argument) 

    Purpose:

        The 'DeltaTime' column was originally created for integration purposes, but now has no current use. It may still be useful
        however.
    '''

    topic_time_array = np.array( time_sorted_topic_df[ 'time' ] )

    topic_delta_time_array = np.diff( topic_time_array )

    topic_delta_time_list = list( topic_delta_time_array )

    topic_delta_time_list = [ topic_delta_time_list[ 0 ] ] + topic_delta_time_list

    time_sorted_topic_df[ 'DeltaTime' ] = topic_delta_time_list


# In[11]:


def Distance( time_sorted_chassis_df ):

    '''
    Do not use this function, it is legacy
    '''

    chassis_DeltaTime_array = np.array( time_sorted_chassis_df[ 'DeltaTime' ] ) * 1e-9 # seconds

    chassis_speedMps_array = np.array( time_sorted_chassis_df[ 'speedMps' ] ) # meters/second

    chassis_Distance_list = [] # meters

    for index in range( len( chassis_DeltaTime_array ) ):

        current_index_Distance = np.sum( chassis_DeltaTime_array[ : index ] * chassis_speedMps_array[ : index ] )

        chassis_Distance_list.append( current_index_Distance )

    time_sorted_chassis_df[ 'Distance' ] = chassis_Distance_list


# In[ ]:


def Distance( df ):

    '''
    Do not use as a standalone metric; vulnerable to time discontinuities. Useful in producing other calculated fields.
    '''

    time_array = np.array( df[ 'time' ] ) * 1e-9 # seconds

    speedMps_array = np.array( df[ 'speedMps' ] ) # meters/second

    #

    deltatime_array = np.diff( time_array )

    deltatime_array = np.insert( deltatime_array, 0, deltatime_array[ 0 ] )

    #

    deltadistance_array = deltatime_array * speedMps_array #meters

    Distance_col = np.cumsum( deltadistance_array )

    #

    df[ 'Distance' ] = Distance_col


# In[12]:


def MergeChassisDriveEvent( chassis_df, drive_event_df ):

    chassis_time_array = np.array( chassis_df[ 'time' ] )

    #

    drive_event_time_array = np.array( drive_event_df[ 'time' ] )

    drive_event_event_array = np.array( drive_event_df[ 'event' ] )

    drive_event_type_array = np.array( drive_event_df[ 'type' ] )

    #

    chassis_num_rows = len( chassis_time_array )

    chassis_event_array = np.array( [ None for index in range( chassis_num_rows ) ] )

    chassis_type_array = np.array( [ None for index in range( chassis_num_rows ) ] )

    #

    for drive_event_time, drive_event_event, drive_event_type in zip( drive_event_time_array, drive_event_event_array,
                                                                      drive_event_type_array ):

        abs_time_diff_array = np.abs( chassis_time_array - drive_event_time )

        min_abs_time_diff_array_index = np.where( abs_time_diff_array == np.min( abs_time_diff_array ) )

        chassis_event_array[ min_abs_time_diff_array_index ] = drive_event_event

        chassis_type_array[ min_abs_time_diff_array_index ] = drive_event_type

    chassis_df[ 'DriveEvent' ] = chassis_event_array

    chassis_df[ 'DriveEventType' ] = chassis_type_array


# In[3]:


def DistanceToNearestDisengagement( time_sorted_chassis_df ):

    chassis_time_array = np.array( time_sorted_chassis_df[ 'time' ] ) * 1e-9 # seconds

    chassis_speedMps_array = np.array( time_sorted_chassis_df[ 'speedMps' ] ) # meters/second

    chassis_TernaryDrivingModeTransition_array = np.array( time_sorted_chassis_df[ 'TernaryDrivingModeTransition' ] )

    gmID = time_sorted_chassis_df[ 'groupMetadataID' ][ 0 ]

    #

    chassis_deltatime_array = np.diff( chassis_time_array )

    chassis_deltatime_array = np.insert( chassis_deltatime_array, 0, chassis_deltatime_array[ 0 ] )

    #

    chassis_deltadistance_array = chassis_deltatime_array * chassis_speedMps_array

    chassis_distance_array = np.cumsum( chassis_deltadistance_array )

    #

    chassis_disengagement_indexes = np.where( chassis_TernaryDrivingModeTransition_array == -1 )

    chassis_disengagement_distance_array = chassis_distance_array[ chassis_disengagement_indexes ]

    #

    if ( len( chassis_disengagement_indexes[ 0 ] ) == 0 ):

        nan_list = [ np.nan for index in chassis_time_array ]

        time_sorted_chassis_df[ 'DistanceToNearestDisengagement' ] = nan_list

        time_sorted_chassis_df[ 'NearestDisengagementID' ] = nan_list

        return None

    #

    distance_to_disengagement_array_list = []

    for disengagement_distance in chassis_disengagement_distance_array:

        distance_to_disengagement_array = chassis_distance_array - disengagement_distance

        distance_to_disengagement_array_list.append( distance_to_disengagement_array )

    distance_to_disengagement_array_matrix = np.vstack( distance_to_disengagement_array_list )

    chassis_DistanceToNearestDisengagement_list = []

    chassis_NearestDisengagementID_list = []
    
    for colnum in range( distance_to_disengagement_array_matrix.shape[ 1 ] ):

        col = distance_to_disengagement_array_matrix[ :, colnum ]

        abs_col = np.abs( col )

        min_abs_col_index = np.where( abs_col == np.min( abs_col ) )

        DistanceToNearestDisengagement = col[ min_abs_col_index ][ 0 ]

        NearestDisengagementID = gmID + '_' + str( int( min_abs_col_index[ 0 ][ 0 ] ) )

        chassis_DistanceToNearestDisengagement_list.append( DistanceToNearestDisengagement )

        chassis_NearestDisengagementID_list.append( NearestDisengagementID )

    time_sorted_chassis_df[ 'DistanceToNearestDisengagement' ] = chassis_DistanceToNearestDisengagement_list

    time_sorted_chassis_df[ 'NearestDisengagementID' ] = chassis_NearestDisengagementID_list


# In[1]:


def Acceleration_chassistime( time_sorted_chassis_df ):

    # Made by Jasmine

    speedMps_array = np.array( time_sorted_chassis_df[ 'speedMps' ] )

    time_array = np.diff( np.array( time_sorted_chassis_df[ 'time' ] ) ) * 1e-9 # seconds

    time_array = np.insert( time_array, 0, time_array[ 0 ] )

    acceleration_list = [] # meters/second^2
    for index in range( 1, len( speedMps_array ) ):

        acceleration = ( speedMps_array[ index ] - speedMps_array[ index - 1 ] ) / time_array[ index ]

        acceleration_list.append( acceleration )

    acceleration_list = [ 0 ] + acceleration_list # Initial acceleration is 0

    time_sorted_chassis_df[ 'Acceleration' ] = acceleration_list


# In[2]:


def Acceleration( time_sorted_chassis_df, time_interval = 1 ):

    speedMps_array = np.array( time_sorted_chassis_df[ 'speedMps' ] )

    time_array = np.array( time_sorted_chassis_df[ 'time' ] ) * 1e-9 # seconds

    time_diff_array = np.diff( time_array )

    #

    time_diff_sum = 0

    index_list = []

    list_of_index_lists = []

    for index, time_diff in enumerate( time_diff_array ):

        index_list.append( index )

        time_diff_sum = time_diff_sum + time_diff

        if ( ( time_diff_sum >= time_interval ) or ( index == len( time_diff_array ) - 1 ) ):

            index_list.append( index_list[ -1 ] + 1 )

            list_of_index_lists.append( index_list )

            time_diff_sum = 0

            index_list = []

    #

    acceleration_array = np.array( [ 0. for index in range( len( time_array ) ) ] )

    for ilist in list_of_index_lists:

        speedMps_array_subset = speedMps_array[ ilist ]

        time_array_subset = time_array[ ilist ]

        acceleration = ( speedMps_array_subset[ -1 ] - speedMps_array_subset[ 0 ] ) / ( time_array_subset[ -1 ] - time_array_subset[ 0 ] ) # meters / second ^ 2

        acceleration_array[ ilist ] = acceleration

    time_sorted_chassis_df[ 'Acceleration' ] = acceleration_array


# In[ ]:


def MovingFunction( df, moving_colname, window, operation, desired_colnames ):

    if ( operation == 'mean' ):

        def operation_func( arraylike_obj ): return np.mean( arraylike_obj )

    elif ( operation == 'median' ):

        def operation_func( arraylike_obj ): return np.median( arraylike_obj )

    elif ( operation == 'overall_diff' ):

        def operation_func( arraylike_obj ): return ( arraylike_obj[ -1 ] - arraylike_obj[ 0 ] )

    elif ( operation == 'mean_diff' ):

        def operation_func( arraylike_obj ): 

            if ( len( arraylike_obj ) > 1 ):

                return ( np.mean( np.diff( arraylike_obj ) ) )

            elif ( len( arraylike_obj ) == 1 ):

                return 0

    elif ( operation == 'stddev' ):

        def operation_func( arraylike_obj ): return ( np.std( arraylike_obj ) )

    #

    moving_col = np.array( df[ moving_colname ] )

    moving_col_diff = np.diff( moving_col )

    #
    
    desired_cols = [ np.array( df[ desired_colname ] ) for desired_colname in desired_colnames ]

    #

    output_colnames = [ f'{ desired_colname }_{ operation }' for desired_colname in desired_colnames ]

    #

    window_indexes_list = []

    for index in range( 1, len( moving_col ) ):

        window_end_index = index - 1

        #

        value_sum = moving_col_diff[ window_end_index ]

        window_start_index = window_end_index

        while ( ( value_sum < window ) and ( window_start_index > 0 ) ):

            window_start_index = window_start_index - 1

            value_sum = value_sum + moving_col_diff[ window_start_index ]

        #

        window_indexes = [ i for i in range( window_start_index, window_end_index + 1 ) ]

        window_indexes_list.append( window_indexes )

    window_indexes_list = [ window_indexes_list[ 0 ] ] + window_indexes_list

    #

    output_cols = []

    for desired_col in desired_cols:

        output_col = []

        for window_indexes in window_indexes_list:

            output_col.append( operation_func( desired_col[ window_indexes ] ) )

        output_cols.append( output_col )

    #

    for output_colname, output_col in zip( output_colnames, output_cols ):

        df[ output_colname ] = output_col


# In[ ]:


def BinaryDisengagement( df ):

    TernaryDrivingModeTransition_col = df[ 'TernaryDrivingModeTransition' ]

    BinaryDisengagement_col = []

    for val in TernaryDrivingModeTransition_col:

        if ( ( val == 0 ) or ( val == 1 ) ):

            BinaryDisengagement_col.append( 0 )

        elif ( val == -1 ):

            BinaryDisengagement_col.append( 1 )

    df[ 'BinaryDisengagement' ] = BinaryDisengagement_col


# In[ ]:


def BinaryDisengagementExpanded( df, moving_colname, window ):

    moving_col = np.array( df[ moving_colname ] )

    moving_col_diff = np.diff( moving_col )

    #

    BinaryDisengagement_col = np.array( df[ 'BinaryDisengagement' ] )

    disengagement_indexes = np.where( BinaryDisengagement_col == 1 )[ 0 ]

    #
    
    window_indexes_list = []

    for index in disengagement_indexes:
        
        window_indexes = [ index ]

        cur_loop_index = index - 1

        value_sum = moving_col_diff[ cur_loop_index ]

        while ( ( value_sum <= window ) and ( cur_loop_index > -1 ) ):

            window_indexes = [ cur_loop_index ] + window_indexes

            #

            cur_loop_index = cur_loop_index - 1

            value_sum = value_sum + moving_col_diff[ cur_loop_index ]

        window_indexes_list.append( window_indexes )

    #

    BinaryDisengagementExpanded_col = np.copy( BinaryDisengagement_col )

    for window_indexes in window_indexes_list:

        BinaryDisengagementExpanded_col[ window_indexes ] = 1

    #

    df[ 'BinaryDisengagementExpanded' ] = BinaryDisengagementExpanded_col


# In[ ]:


def Index( df ):

    num_of_rows = df.shape[ 0 ]

    df[ 'Ind' ] = [ index for index in range( num_of_rows ) ]


# In[ ]:


def DisengagementID( df, expanded = False ):

    '''
    Needs adjustment for expanded setting, but can still be used.
    '''

    gmID = df[ 'groupMetadataID' ][ 0 ]
    
    #

    if ( expanded == False ):

        disengagment_col = np.array( df[ 'BinaryDisengagement' ] )

        string1 = ''

        string2 = ''

    elif ( expanded == True ):

        disengagment_col = np.array( df[ 'BinaryDisengagementExpanded' ] )

        string1 = 'Expanded'

        string2 = 'e'

    #

    disengagement_indexes = np.where( disengagment_col == 1 )[ 0 ]

    #

    DisengagementID_col = [ 'NAD' for i in disengagment_col ]

    counter = 0

    for index in disengagement_indexes:

        DisengagementID_col[ index ] = f'{ gmID }_{ counter }{ string2 }'

        counter = counter + 1

    df[ f'Disengagement{ string1 }ID' ] = DisengagementID_col


# ### Functions unrelated to calculated fields but are important vvv

# In[14]:


def origin_dir():

    '''
    Arguments:
    
        No arguments.
        
    Internal Task (subroutine):
    
        No internal task.

    Output:

        Outputs the path to the TDMprivate folder on one's AWS desktop, if it exists. Otherwise, raises an error.

        (a string)

    Purpose:

        A quick way to obtain the path to the TDMprivate folder, regardless of the AWS user. This path is used a lot for reading 
        data.
    '''

    home_dir_list = os.listdir( '/home' )

    for dir in home_dir_list:

        if '_linux' in dir:

            path = f'/home/{dir}/Desktop/TDMprivate'

            if not os.path.exists( path ):

                raise Exception( 'TDMprivate folder does not exist. TDMprivate folder must exist on Desktop. Notify Ryan or ' +
                                 'Vincent if this message appears.' )

            else:

                return path


# In[15]:


def retrieve_metadata_df():

    '''
    Do not use this function, it is legacy
    '''

    path = f'{ origin_dir() }/metadata/metadata.csv'

    metadata_df = pd.read_csv( f'{ origin_dir() }/metadata/metadata.csv' )

    return metadata_df


# In[16]:


def list_gmIDs():

    '''
    Arguments:
    
        No arguments.
        
    Internal Task (subroutine):
    
        No internal task.

    Output:

        Outputs a list of the names of all the groupMetadataIDs with data available to read in the data folder of the TDMprivate 
        folder.

        (a list of strings)

    Purpose:

        A quick way to obtain and choose the groupMetadataIDs one wants data from.
    '''

    path = f'{ origin_dir() }/data'

    gmID_list = [ file for file in os.listdir( path ) if os.path.isdir( f'{ path }/{ file }' ) ]

    return gmID_list


# In[17]:


def list_topics():

    '''
    Arguments:
    
        No arguments.
        
    Internal Task (subroutine):
    
        No internal task.

    Output:

        Outputs a list of the names of all the topics with data available to be read for each of the groupMetadataIDs in the data
        folder of the TDMprivate folder.

        (a list of strings)

    Purpose:

        A quick way to obtain and choose the topics one wants data for, for a groupMetadataID(s).
    '''

    gmID_list = list_gmIDs()

    path = f'{ origin_dir() }/data/{ gmID_list[ 0 ] }'

    topic_list = [ file for file in os.listdir( path ) if os.path.isdir( f'{ path }/{ file }' ) ]

    topic_list = [ topic.replace( '_', '/' ) for topic in topic_list ]

    return topic_list


# In[18]:


def retrieve_gmID_topic( gmID, topic ):

    '''
    Arguments:
    
        gmID -> The name of a singular groupMetadataID one wants data from (a string).

        topic - > The name of a singular topic one wants data for, for the given groupMetadataID (a string).
        
    Internal Task (subroutine):
    
        No internal task.

    Output:

        Outputs a Pandas dataframe containing the data for the requested topic for the requested groupMetadataID.

        (a Pandas dataframe)

    Purpose:

        A quick way to obtain the data for the requested topic for the requested groupMetadataID without needing to work with the
        directory structure of TDMprivate.
    '''

    dir_friendly_topic = topic.replace( '/', '_' )

    path = f'{ origin_dir() }/data/{ gmID }/{ dir_friendly_topic }/{ gmID + dir_friendly_topic }.csv'

    gmID_topic_df = pd.read_csv( path )

    return gmID_topic_df


# In[19]:


def give_route( gmID ):

    '''
    Arguments:
    
        gmID -> The name of a singular groupMetadataID (a string).
        
    Internal Task (subroutine):
    
        No internal task.

    Output:

        Outputs the name of the route the inputted groupMetadataID is associated with. Is always 'Red', 'Green', or 'Blue'.

        (a string)

    Purpose:

        A quick way to check what route the requested groupMetadataID is on.
    '''

    red_route_gmID_list = [ 'c0555ef0-f50f-11ee-8afa-cb629b0d53e6', '1bbbfbae-c839-11ee-a7fc-dd032dba19e8', '05c7c824-cab8-11ee-aa4d-1d66adf2f0c7', '2462c9d0-eecd-11ee-9385-ef789ffde1d3', '94c53148-eeed-11ee-9385-ef789ffde1d3', '88a68dd8-eef9-11ee-9385-ef789ffde1d3', '4ed017ee-ef05-11ee-9385-ef789ffde1d3', 'ce6465b6-f51b-11ee-8afa-cb629b0d53e6', 'aa5dbcd2-ef10-11ee-9385-ef789ffde1d3', 'e7b934a8-ef1a-11ee-9385-ef789ffde1d3', '85b6e70e-ef7a-11ee-b966-fb353e7798cd', '219f7eb8-ef87-11ee-b966-fb353e7798cd', '3d2d29ec-ef95-11ee-b966-fb353e7798cd', 'd3698592-ef9d-11ee-b966-fb353e7798cd', 'fd1ab258-efa7-11ee-b966-fb353e7798cd', '8347b862-efad-11ee-b966-fb353e7798cd', '817d6848-efb6-11ee-b966-fb353e7798cd', 'be857244-efc0-11ee-b966-fb353e7798cd', 'fc211bb2-efca-11ee-b966-fb353e7798cd', '6d2ea45a-c839-11ee-a7fc-dd032dba19e8', '01e65360-efd4-11ee-b966-fb353e7798cd', '1b6aca0e-efdf-11ee-b966-fb353e7798cd', '72a03d4a-efe9-11ee-b966-fb353e7798cd', '8fa6fe80-c869-11ee-a7fc-dd032dba19e8', '7fb7b9c0-c881-11ee-a7fc-dd032dba19e8', '3151e9e2-eff3-11ee-b966-fb353e7798cd', 'f41cbd44-eff8-11ee-b966-fb353e7798cd', '5a4bccf4-effe-11ee-b966-fb353e7798cd', '2f95c748-f009-11ee-b966-fb353e7798cd', 'fcc6fcd2-f013-11ee-b966-fb353e7798cd', '51ef6da6-ca9f-11ee-909c-e1dc60cf66f9', '8437f77a-cab7-11ee-909c-e1dc60cf66f9', 'f43b6a70-f01e-11ee-b966-fb353e7798cd', '457dc5ee-f02a-11ee-b966-fb353e7798cd', '853ef120-cad3-11ee-909c-e1dc60cf66f9', '2a61b8a8-f528-11ee-8afa-cb629b0d53e6', '41b67a28-f52f-11ee-8afa-cb629b0d53e6', 'fe973c9c-f53c-11ee-8afa-cb629b0d53e6', '96f7a614-f549-11ee-8afa-cb629b0d53e6', 'd12cd1c4-caec-11ee-909c-e1dc60cf66f9', 'c338788a-d324-11ee-b437-336917683bb8', 'c9c6856c-d33c-11ee-b437-336917683bb8', '787f70da-f036-11ee-b966-fb353e7798cd', '6daff50c-f041-11ee-b972-fb353e7798cd', '23a7aa3e-f048-11ee-b97d-fb353e7798cd', 'ef63db62-f051-11ee-b986-fb353e7798cd', '48021fe0-f05c-11ee-b992-fb353e7798cd', 'bb52690a-f066-11ee-b99e-fb353e7798cd', '599673dc-f070-11ee-b9a9-fb353e7798cd', '26af7004-f07a-11ee-b9b2-fb353e7798cd', '5230b9be-f083-11ee-b9b8-fb353e7798cd', '4bbe3c64-f088-11ee-b9c3-fb353e7798cd', '5c2ad8ec-f08c-11ee-b9c8-fb353e7798cd', '6c5f7416-f096-11ee-b9d4-fb353e7798cd', 'ede139be-f098-11ee-b9d8-fb353e7798cd', 'dbff355c-f0a2-11ee-b9e3-fb353e7798cd', 'eb22edf4-f0ab-11ee-b9e9-fb353e7798cd', 'd1d69a76-f0b5-11ee-b9f0-fb353e7798cd', '906d3c4e-f0be-11ee-b9fb-fb353e7798cd', '2bb03aaa-f0c7-11ee-ba06-fb353e7798cd', 'dd72fdec-f0cf-11ee-ba0d-fb353e7798cd', '211bdb36-f0da-11ee-ba1b-fb353e7798cd', 'f0eebb6a-f0dc-11ee-ba1e-fb353e7798cd', '1f70a4f0-f0e0-11ee-ba1e-fb353e7798cd', 'f711e68e-f0e1-11ee-ba1f-fb353e7798cd', '622bd2e8-f0e4-11ee-ba1f-fb353e7798cd', '7d27535e-f0e6-11ee-ba21-fb353e7798cd', '8dbbbf1c-f0ef-11ee-ba29-fb353e7798cd', 'd21965e6-f0fa-11ee-ba37-fb353e7798cd', '171c50bc-f106-11ee-ba42-fb353e7798cd', 'de933de8-f112-11ee-ba4d-fb353e7798cd', '9189a2a8-f121-11ee-ba5b-fb353e7798cd', 'f755cf60-f132-11ee-ba6d-fb353e7798cd', '9798fe24-f143-11ee-ba78-fb353e7798cd', '35518ec4-f153-11ee-ba88-fb353e7798cd', 'ecebb942-f162-11ee-ba97-fb353e7798cd', '1ee938a2-f172-11ee-baa6-fb353e7798cd', '38ac9526-f182-11ee-bab0-fb353e7798cd', '6f5b3612-f18d-11ee-bab8-fb353e7798cd', 'd24820c8-f197-11ee-babe-fb353e7798cd', 'bf518644-f1a6-11ee-bac9-fb353e7798cd', '3950298e-f1b4-11ee-bad3-fb353e7798cd', 'cccc7d32-f1c0-11ee-bada-fb353e7798cd', 'a6895b74-f1cd-11ee-bae2-fb353e7798cd', '61b4e416-f1da-11ee-bae8-fb353e7798cd', 'fc1e1b6a-f1e6-11ee-baf6-fb353e7798cd', 'b82476fe-f1f3-11ee-baff-fb353e7798cd', '286e019a-f204-11ee-bb07-fb353e7798cd', '84d96f18-f214-11ee-bb13-fb353e7798cd', '88dd6fbe-f224-11ee-bb21-fb353e7798cd', '61b12e7a-f234-11ee-bb33-fb353e7798cd', '7cbd932e-f244-11ee-bb3f-fb353e7798cd', 'cf831f42-f353-11ee-bb4e-fb353e7798cd', '43a1a35e-f362-11ee-bb4e-fb353e7798cd', 'bfde2aec-f370-11ee-bb4e-fb353e7798cd', '0b72a836-f37e-11ee-bb4e-fb353e7798cd', '662741a4-f38a-11ee-bb4e-fb353e7798cd', '65cfbfd6-f396-11ee-bb4e-fb353e7798cd', 'c25271be-f3a4-11ee-bb4e-fb353e7798cd', '868de15e-f3b3-11ee-bb4e-fb353e7798cd', '3344a3c0-f502-11ee-8afa-cb629b0d53e6' ]

    green_route_gmID_list = [ '5afabc8c-f035-11ee-b966-fb353e7798cd', 'fe0395f0-f1ea-11ee-baf9-fb353e7798cd', '7a22a34c-f1f0-11ee-bafe-fb353e7798cd', '0f3cdf60-f1f6-11ee-bb00-fb353e7798cd', 'a08a8c7e-f1fb-11ee-bb05-fb353e7798cd', 'df6c3fb4-f200-11ee-bb07-fb353e7798cd', '43abeb00-f206-11ee-bb07-fb353e7798cd', '7948628e-f20b-11ee-bb0f-fb353e7798cd', 'fa852f30-f210-11ee-bb10-fb353e7798cd', '986a0b90-f215-11ee-bb15-fb353e7798cd', '57a2192c-f21a-11ee-bb17-fb353e7798cd', 'cd11fc28-f21e-11ee-bb1c-fb353e7798cd', 'b2f58080-f223-11ee-bb20-fb353e7798cd', '2f2939cc-f228-11ee-bb28-fb353e7798cd', '40706f50-f03b-11ee-b96e-fb353e7798cd', '3a7dc9a6-f042-11ee-b974-fb353e7798cd', 'ece2a8be-f047-11ee-b97d-fb353e7798cd', '8adb6498-f04d-11ee-b981-fb353e7798cd', '3ec95686-f053-11ee-b988-fb353e7798cd', 'cf7148d8-f058-11ee-b98a-fb353e7798cd', '7f824ea2-f05e-11ee-b993-fb353e7798cd', '8b0593cc-cb4e-11ee-909c-e1dc60cf66f9', '837fc882-cb5a-11ee-909c-e1dc60cf66f9', '25641404-cb66-11ee-909c-e1dc60cf66f9', '14b6bc9c-f064-11ee-b998-fb353e7798cd', '25e27b86-f06a-11ee-b9a3-fb353e7798cd', 'c2f54552-f06f-11ee-b9a9-fb353e7798cd', 'c4146d46-f074-11ee-b9ac-fb353e7798cd', 'c1b320e2-f079-11ee-b9b0-fb353e7798cd', 'a6539bd2-cb72-11ee-909c-e1dc60cf66f9', 'ba87f3ec-f07e-11ee-b9b4-fb353e7798cd', '3d8020aa-cb7f-11ee-909c-e1dc60cf66f9', '271fee10-cb8b-11ee-909c-e1dc60cf66f9', 'b31aca98-cb95-11ee-909c-e1dc60cf66f9', 'ed352100-cba0-11ee-909c-e1dc60cf66f9', '072ef896-cbac-11ee-909c-e1dc60cf66f9', '3c415ade-d353-11ee-b437-336917683bb8', '88b0613a-d35d-11ee-b437-336917683bb8', '879e3fa2-f085-11ee-b9bd-fb353e7798cd', 'a2ed8b42-f089-11ee-b9c3-fb353e7798cd', 'b6227b56-f08d-11ee-b9c9-fb353e7798cd', 'd1a3a310-f091-11ee-b9ce-fb353e7798cd', '878e1a02-f092-11ee-b9cf-fb353e7798cd', 'e640cf0a-f096-11ee-b9d5-fb353e7798cd', '31b48540-f09b-11ee-b9da-fb353e7798cd', '2c8690b4-f09f-11ee-b9de-fb353e7798cd', '00ff88b6-f0a3-11ee-b9e3-fb353e7798cd', '900701bc-f0a6-11ee-b9e4-fb353e7798cd', '41cd65b4-f0aa-11ee-b9e8-fb353e7798cd', 'b2bfb60c-f0ad-11ee-b9ea-fb353e7798cd', '7aa336e6-f0b1-11ee-b9ed-fb353e7798cd', '56b8baa4-f0b5-11ee-b9f0-fb353e7798cd', '471890c0-f0b9-11ee-b9f5-fb353e7798cd', '6c415180-f0bd-11ee-b9fa-fb353e7798cd', '3b6d2a5c-f0c2-11ee-ba01-fb353e7798cd', 'a437811e-ccf4-11ee-9435-f7e542e2436c', 'fe729430-f232-11ee-bb32-fb353e7798cd', '4cf81634-f238-11ee-bb34-fb353e7798cd', 'e9a1d768-f23d-11ee-bb39-fb353e7798cd', 'f8fd0fd8-f243-11ee-bb3f-fb353e7798cd', '58d78342-f24a-11ee-bb45-fb353e7798cd', '25135418-f250-11ee-bb4a-fb353e7798cd', 'cbdc93f4-f255-11ee-bb4e-fb353e7798cd', 'de226278-f25a-11ee-bb4e-fb353e7798cd', '81a5a96e-f0c6-11ee-ba06-fb353e7798cd', '8be24d52-f0ca-11ee-ba0a-fb353e7798cd', 'ba80ba8c-f0ce-11ee-ba0d-fb353e7798cd', '240ebe64-f0d3-11ee-ba14-fb353e7798cd', 'b3ee0dd8-f0d7-11ee-ba18-fb353e7798cd', 'bbbd0cc6-f0dc-11ee-ba1e-fb353e7798cd', 'c9be2042-f0de-11ee-ba1e-fb353e7798cd', '044d976e-f0e5-11ee-ba20-fb353e7798cd', 'f9c5e53e-f0ea-11ee-ba28-fb353e7798cd', 'fa9cba86-f0f0-11ee-ba2a-fb353e7798cd', '53fad09e-f0f7-11ee-ba2f-fb353e7798cd', 'a901fe40-f0fd-11ee-ba39-fb353e7798cd', '961fd9cc-f103-11ee-ba3f-fb353e7798cd', 'bb4d37d4-f109-11ee-ba46-fb353e7798cd', 'de493be2-f10f-11ee-ba4b-fb353e7798cd', 'd846a080-f115-11ee-ba51-fb353e7798cd', 'd454c586-f11c-11ee-ba55-fb353e7798cd', 'dea29156-f123-11ee-ba5d-fb353e7798cd', '7e3d64da-f12d-11ee-ba68-fb353e7798cd', '848e44a6-f134-11ee-ba6d-fb353e7798cd', '5c7a9ab2-f13b-11ee-ba72-fb353e7798cd', 'a231c0b0-f142-11ee-ba76-fb353e7798cd', '8e5c4fc2-f149-11ee-ba7f-fb353e7798cd', '73bc30cc-f150-11ee-ba84-fb353e7798cd', '4c88757c-f157-11ee-ba89-fb353e7798cd', 'f570c51c-f15d-11ee-ba91-fb353e7798cd', 'd7cb9c92-f164-11ee-ba97-fb353e7798cd', 'bf9157f0-f16b-11ee-ba9e-fb353e7798cd', '9df14b4e-f172-11ee-baa6-fb353e7798cd', 'c59a54e0-f179-11ee-baab-fb353e7798cd', 'c14299be-f180-11ee-bab0-fb353e7798cd', '9736e77c-f187-11ee-bab6-fb353e7798cd', 'c4fca7bc-f18e-11ee-bab8-fb353e7798cd', '5774dcde-f196-11ee-babe-fb353e7798cd', '51b74168-f19d-11ee-babf-fb353e7798cd', '98692fde-f1a4-11ee-bac6-fb353e7798cd', '5fc763f6-f1ab-11ee-bacd-fb353e7798cd', '99b9f446-f1b2-11ee-bad3-fb353e7798cd', '870cfd32-f1b9-11ee-bad5-fb353e7798cd', 'f12112ba-f1c0-11ee-bada-fb353e7798cd', '5f7ce340-f1c8-11ee-bae0-fb353e7798cd', '96ceec56-f1cf-11ee-bae4-fb353e7798cd', '3ed4aa16-f1d6-11ee-bae6-fb353e7798cd', '236836f6-f1dd-11ee-bae8-fb353e7798cd', '1c74d294-f1e4-11ee-baf0-fb353e7798cd' ]

    blue_route_gmID_list = [ '06cbdbc0-db4d-11ee-a158-97f8443fd730', '3a116996-93a9-11ee-956e-9da2d070324c', '2837eb9c-9542-11ee-956e-9da2d070324c', 'da853e0c-a10f-11ee-981c-d126ddbe9afa', '154fab12-a43f-11ee-88ec-eb6a8d5269b4', '88180f82-ed4f-11ee-9385-ef789ffde1d3', 'ba6e1072-9524-11ee-956e-9da2d070324c', 'baf0e4be-bede-11ee-835b-599066b5eb60', '6af236d6-d98f-11ee-a158-97f8443fd730', '39ba7438-d0d5-11ee-9435-f7e542e2436c', 'c0624e24-d9aa-11ee-a158-97f8443fd730', '19b7ebd0-d9b7-11ee-a158-97f8443fd730', '47561998-d9c3-11ee-a158-97f8443fd730', '59c189d8-ed54-11ee-9385-ef789ffde1d3', 'd94ef300-ed60-11ee-9385-ef789ffde1d3', '8b6a6cfc-ed6d-11ee-9385-ef789ffde1d3', '530de03a-ed79-11ee-9385-ef789ffde1d3', '43914d48-ed85-11ee-9385-ef789ffde1d3', '86841630-d9d0-11ee-a158-97f8443fd730', 'c9023e32-ed90-11ee-9385-ef789ffde1d3', 'e269948a-ed9d-11ee-9385-ef789ffde1d3', '60546ef4-edaa-11ee-9385-ef789ffde1d3', '76683d3c-db18-11ee-a158-97f8443fd730', '3a2a78cc-db21-11ee-a158-97f8443fd730', 'f9d62032-db2a-11ee-a158-97f8443fd730', 'dc39aa14-db32-11ee-a158-97f8443fd730', 'e6d7d384-db40-11ee-a158-97f8443fd730', 'f6ac3c82-a445-11ee-88ec-eb6a8d5269b4', '45ad3a9a-edb4-11ee-9385-ef789ffde1d3', 'e8a8b2be-edbf-11ee-9385-ef789ffde1d3', '7613801a-edcb-11ee-9385-ef789ffde1d3', '3ce8a358-edd8-11ee-9385-ef789ffde1d3', '68c289fa-dbd4-11ee-a158-97f8443fd730', '8c57e8ac-dbec-11ee-a158-97f8443fd730', 'aa86a660-dc05-11ee-a158-97f8443fd730', '69ab88ec-dc17-11ee-a158-97f8443fd730', 'e2079a78-dc1d-11ee-a158-97f8443fd730', 'a17c1280-ea10-11ee-b297-3b0ad9d5d6c6', 'ed7f2038-ea1e-11ee-b297-3b0ad9d5d6c6', '58263e34-a45c-11ee-88ec-eb6a8d5269b4', '20cbfe8c-ea2b-11ee-b297-3b0ad9d5d6c6', '3ea96640-ea37-11ee-b297-3b0ad9d5d6c6', '64875cc0-d054-11ee-9435-f7e542e2436c', '57d240d6-ea4d-11ee-b297-3b0ad9d5d6c6', 'c335d84c-a45c-11ee-88ec-eb6a8d5269b4', '82d39c74-ea59-11ee-b297-3b0ad9d5d6c6', 'b76f33be-ea61-11ee-b297-3b0ad9d5d6c6', '17876fec-ea66-11ee-b297-3b0ad9d5d6c6', '5fcc4fd8-ea71-11ee-b297-3b0ad9d5d6c6', 'd1d090d4-ea7c-11ee-b297-3b0ad9d5d6c6', '36663b02-ea87-11ee-b297-3b0ad9d5d6c6', '04115e66-ea91-11ee-b297-3b0ad9d5d6c6', '0f4f0a06-ea98-11ee-b297-3b0ad9d5d6c6', '5976b77a-a504-11ee-88ec-eb6a8d5269b4', '2bc6ebb8-a529-11ee-88ec-eb6a8d5269b4', '7f09f6c6-a5b0-11ee-88ec-eb6a8d5269b4', 'cf6fdf3a-eaa3-11ee-b297-3b0ad9d5d6c6', 'f671c05c-a5e4-11ee-88ec-eb6a8d5269b4', '90101c36-a621-11ee-88ec-eb6a8d5269b4', 'aef91c4a-ede5-11ee-9385-ef789ffde1d3', '513a670c-eea9-11ee-9385-ef789ffde1d3', '559495ca-d270-11ee-b437-336917683bb8', '621c07b8-eaaf-11ee-b297-3b0ad9d5d6c6', '6458c26e-eab9-11ee-b297-3b0ad9d5d6c6', '81acf35c-eac4-11ee-b297-3b0ad9d5d6c6', 'baec243e-eacf-11ee-b297-3b0ad9d5d6c6', 'feaf2ba8-d28d-11ee-b437-336917683bb8', '05d0240e-eadb-11ee-b297-3b0ad9d5d6c6', 'd54c11ca-eae5-11ee-b297-3b0ad9d5d6c6', '23765aa8-eaf1-11ee-b297-3b0ad9d5d6c6', 'f93290be-eafe-11ee-b297-3b0ad9d5d6c6', '079f0d30-eb09-11ee-b297-3b0ad9d5d6c6', 'bcf44e58-eb0d-11ee-b297-3b0ad9d5d6c6', 'e9e14d3a-eb17-11ee-b297-3b0ad9d5d6c6', '781c0bcc-eb21-11ee-b297-3b0ad9d5d6c6', '593d4b54-d0a9-11ee-9435-f7e542e2436c', 'd4b936f6-eb36-11ee-b297-3b0ad9d5d6c6', 'cc0299e6-eb3e-11ee-b297-3b0ad9d5d6c6', '60c57e4e-eb4a-11ee-b297-3b0ad9d5d6c6', '54a02cb8-eb4f-11ee-b297-3b0ad9d5d6c6', '70060810-eb59-11ee-b297-3b0ad9d5d6c6', '80340ab8-d054-11ee-9435-f7e542e2436c', 'fc119dfc-eb67-11ee-b297-3b0ad9d5d6c6', '75f83e28-eb77-11ee-b297-3b0ad9d5d6c6', '3343fd3c-eb87-11ee-b297-3b0ad9d5d6c6', '64bbe8e0-eb94-11ee-b297-3b0ad9d5d6c6', '2d35c522-eba2-11ee-b297-3b0ad9d5d6c6', 'af10e22a-ebb1-11ee-b297-3b0ad9d5d6c6', 'a7c98b32-ebc2-11ee-b297-3b0ad9d5d6c6', 'c8f54ac0-ebd2-11ee-b297-3b0ad9d5d6c6', 'c7c02bda-ebe0-11ee-b297-3b0ad9d5d6c6', '7228e03a-ebf0-11ee-b297-3b0ad9d5d6c6', '21376e38-ec01-11ee-b297-3b0ad9d5d6c6', 'b224ef9c-ec10-11ee-b297-3b0ad9d5d6c6', '04151804-ec20-11ee-b297-3b0ad9d5d6c6', '5240e750-ec30-11ee-b297-3b0ad9d5d6c6', 'e9d67bf2-ec35-11ee-b297-3b0ad9d5d6c6', 'cb205756-ec43-11ee-b297-3b0ad9d5d6c6', 'df8e3742-ec54-11ee-b297-3b0ad9d5d6c6', '20f0b890-ec64-11ee-b297-3b0ad9d5d6c6', '4d0254fc-ec73-11ee-b297-3b0ad9d5d6c6', '3d2a80f0-ec81-11ee-b297-3b0ad9d5d6c6', 'ba28b352-ec8f-11ee-b297-3b0ad9d5d6c6', '6d62da08-ec9d-11ee-b297-3b0ad9d5d6c6', '71a18322-ecab-11ee-b297-3b0ad9d5d6c6', '25d3bdc8-ecbc-11ee-b297-3b0ad9d5d6c6', '3441fc36-ecca-11ee-b297-3b0ad9d5d6c6', '326699c2-ecd8-11ee-b297-3b0ad9d5d6c6', '721a9830-ece6-11ee-b297-3b0ad9d5d6c6', 'acd71bc0-ecf4-11ee-9385-ef789ffde1d3', 'd62ee6e8-ed02-11ee-9385-ef789ffde1d3', 'a253145a-d2a6-11ee-b437-336917683bb8', '787d9684-d2c2-11ee-b437-336917683bb8', '606347dc-ed12-11ee-9385-ef789ffde1d3', '6f887868-ed21-11ee-9385-ef789ffde1d3', '9830d896-d2dc-11ee-b437-336917683bb8', '286c70cc-d2f7-11ee-b437-336917683bb8', '64737d98-d312-11ee-b437-336917683bb8', '5f6573ba-ed2f-11ee-9385-ef789ffde1d3', 'f0bcec4e-ed3e-11ee-9385-ef789ffde1d3' ]

    if gmID in red_route_gmID_list:

        return 'Red'

    elif gmID in green_route_gmID_list:

        return 'Green'

    elif gmID in blue_route_gmID_list:

        return 'Blue'

    else:

        raise Exception( f'{ gmID } is not valid' )


# In[20]:


def list_whitelisted_gmIDs():

    gmIDs_set = set( list_gmIDs() )

    blacklisted_gmIDs_set = set( [ '879e3fa2-f085-11ee-b9bd-fb353e7798cd', '900701bc-f0a6-11ee-b9e4-fb353e7798cd', '6f5b3612-f18d-11ee-bab8-fb353e7798cd', '471890c0-f0b9-11ee-b9f5-fb353e7798cd', '3b6d2a5c-f0c2-11ee-ba01-fb353e7798cd', 'a2ed8b42-f089-11ee-b9c3-fb353e7798cd', '787f70da-f036-11ee-b966-fb353e7798cd', '6458c26e-eab9-11ee-b297-3b0ad9d5d6c6', '00ff88b6-f0a3-11ee-b9e3-fb353e7798cd', '7d27535e-f0e6-11ee-ba21-fb353e7798cd', '23765aa8-eaf1-11ee-b297-3b0ad9d5d6c6', 'f93290be-eafe-11ee-b297-3b0ad9d5d6c6', '54a02cb8-eb4f-11ee-b297-3b0ad9d5d6c6', '2f2939cc-f228-11ee-bb28-fb353e7798cd', '48021fe0-f05c-11ee-b992-fb353e7798cd', '2837eb9c-9542-11ee-956e-9da2d070324c', '593d4b54-d0a9-11ee-9435-f7e542e2436c', 'd54c11ca-eae5-11ee-b297-3b0ad9d5d6c6', '6f887868-ed21-11ee-9385-ef789ffde1d3', '45ad3a9a-edb4-11ee-9385-ef789ffde1d3', '4ed017ee-ef05-11ee-9385-ef789ffde1d3', '81a5a96e-f0c6-11ee-ba06-fb353e7798cd', '05d0240e-eadb-11ee-b297-3b0ad9d5d6c6', '6c415180-f0bd-11ee-b9fa-fb353e7798cd', '662741a4-f38a-11ee-bb4e-fb353e7798cd', '60c57e4e-eb4a-11ee-b297-3b0ad9d5d6c6', 'cd11fc28-f21e-11ee-bb1c-fb353e7798cd', 'fa852f30-f210-11ee-bb10-fb353e7798cd', '3950298e-f1b4-11ee-bad3-fb353e7798cd', '2c8690b4-f09f-11ee-b9de-fb353e7798cd', '26af7004-f07a-11ee-b9b2-fb353e7798cd', '6c5f7416-f096-11ee-b9d4-fb353e7798cd', '38ac9526-f182-11ee-bab0-fb353e7798cd', '81acf35c-eac4-11ee-b297-3b0ad9d5d6c6', '86841630-d9d0-11ee-a158-97f8443fd730', '781c0bcc-eb21-11ee-b297-3b0ad9d5d6c6', '2bb03aaa-f0c7-11ee-ba06-fb353e7798cd', '906d3c4e-f0be-11ee-b9fb-fb353e7798cd', '878e1a02-f092-11ee-b9cf-fb353e7798cd', '606347dc-ed12-11ee-9385-ef789ffde1d3', 'cccc7d32-f1c0-11ee-bada-fb353e7798cd', '6daff50c-f041-11ee-b972-fb353e7798cd', 'ba80ba8c-f0ce-11ee-ba0d-fb353e7798cd', 'bf518644-f1a6-11ee-bac9-fb353e7798cd', '3a116996-93a9-11ee-956e-9da2d070324c', '56b8baa4-f0b5-11ee-b9f0-fb353e7798cd', 'bb52690a-f066-11ee-b99e-fb353e7798cd', 'a437811e-ccf4-11ee-9435-f7e542e2436c', '7aa336e6-f0b1-11ee-b9ed-fb353e7798cd', '986a0b90-f215-11ee-bb15-fb353e7798cd', 'ef63db62-f051-11ee-b986-fb353e7798cd', 'd1d69a76-f0b5-11ee-b9f0-fb353e7798cd', '57a2192c-f21a-11ee-bb17-fb353e7798cd', 'e9e14d3a-eb17-11ee-b297-3b0ad9d5d6c6', '0b72a836-f37e-11ee-bb4e-fb353e7798cd', '8be24d52-f0ca-11ee-ba0a-fb353e7798cd', 'd4b936f6-eb36-11ee-b297-3b0ad9d5d6c6', '599673dc-f070-11ee-b9a9-fb353e7798cd', 'cc0299e6-eb3e-11ee-b297-3b0ad9d5d6c6', 'b6227b56-f08d-11ee-b9c9-fb353e7798cd', 'fe729430-f232-11ee-bb32-fb353e7798cd', 'eb22edf4-f0ab-11ee-b9e9-fb353e7798cd', 'fc1e1b6a-f1e6-11ee-baf6-fb353e7798cd', '61b4e416-f1da-11ee-bae8-fb353e7798cd', '31b48540-f09b-11ee-b9da-fb353e7798cd', '240ebe64-f0d3-11ee-ba14-fb353e7798cd', 'bfde2aec-f370-11ee-bb4e-fb353e7798cd', 'b2f58080-f223-11ee-bb20-fb353e7798cd', '4bbe3c64-f088-11ee-b9c3-fb353e7798cd', 'b2bfb60c-f0ad-11ee-b9ea-fb353e7798cd', 'ede139be-f098-11ee-b9d8-fb353e7798cd', 'bcf44e58-eb0d-11ee-b297-3b0ad9d5d6c6', 'a6895b74-f1cd-11ee-bae2-fb353e7798cd', 'e640cf0a-f096-11ee-b9d5-fb353e7798cd', '1f70a4f0-f0e0-11ee-ba1e-fb353e7798cd', '41cd65b4-f0aa-11ee-b9e8-fb353e7798cd', '621c07b8-eaaf-11ee-b297-3b0ad9d5d6c6', 'dbff355c-f0a2-11ee-b9e3-fb353e7798cd', '23a7aa3e-f048-11ee-b97d-fb353e7798cd', '5c2ad8ec-f08c-11ee-b9c8-fb353e7798cd', '079f0d30-eb09-11ee-b297-3b0ad9d5d6c6', 'baec243e-eacf-11ee-b297-3b0ad9d5d6c6', '5230b9be-f083-11ee-b9b8-fb353e7798cd' ] )

    whitelisted_gmIDs_set = gmIDs_set - blacklisted_gmIDs_set

    return list( whitelisted_gmIDs_set )


# In[21]:


def list_blacklisted_gmIDs():

    gmIDs_set = set( list_gmIDs() )

    blacklisted_gmIDs_set = set( [ '879e3fa2-f085-11ee-b9bd-fb353e7798cd', '900701bc-f0a6-11ee-b9e4-fb353e7798cd', '6f5b3612-f18d-11ee-bab8-fb353e7798cd', '471890c0-f0b9-11ee-b9f5-fb353e7798cd', '3b6d2a5c-f0c2-11ee-ba01-fb353e7798cd', 'a2ed8b42-f089-11ee-b9c3-fb353e7798cd', '787f70da-f036-11ee-b966-fb353e7798cd', '6458c26e-eab9-11ee-b297-3b0ad9d5d6c6', '00ff88b6-f0a3-11ee-b9e3-fb353e7798cd', '7d27535e-f0e6-11ee-ba21-fb353e7798cd', '23765aa8-eaf1-11ee-b297-3b0ad9d5d6c6', 'f93290be-eafe-11ee-b297-3b0ad9d5d6c6', '54a02cb8-eb4f-11ee-b297-3b0ad9d5d6c6', '2f2939cc-f228-11ee-bb28-fb353e7798cd', '48021fe0-f05c-11ee-b992-fb353e7798cd', '2837eb9c-9542-11ee-956e-9da2d070324c', '593d4b54-d0a9-11ee-9435-f7e542e2436c', 'd54c11ca-eae5-11ee-b297-3b0ad9d5d6c6', '6f887868-ed21-11ee-9385-ef789ffde1d3', '45ad3a9a-edb4-11ee-9385-ef789ffde1d3', '4ed017ee-ef05-11ee-9385-ef789ffde1d3', '81a5a96e-f0c6-11ee-ba06-fb353e7798cd', '05d0240e-eadb-11ee-b297-3b0ad9d5d6c6', '6c415180-f0bd-11ee-b9fa-fb353e7798cd', '662741a4-f38a-11ee-bb4e-fb353e7798cd', '60c57e4e-eb4a-11ee-b297-3b0ad9d5d6c6', 'cd11fc28-f21e-11ee-bb1c-fb353e7798cd', 'fa852f30-f210-11ee-bb10-fb353e7798cd', '3950298e-f1b4-11ee-bad3-fb353e7798cd', '2c8690b4-f09f-11ee-b9de-fb353e7798cd', '26af7004-f07a-11ee-b9b2-fb353e7798cd', '6c5f7416-f096-11ee-b9d4-fb353e7798cd', '38ac9526-f182-11ee-bab0-fb353e7798cd', '81acf35c-eac4-11ee-b297-3b0ad9d5d6c6', '86841630-d9d0-11ee-a158-97f8443fd730', '781c0bcc-eb21-11ee-b297-3b0ad9d5d6c6', '2bb03aaa-f0c7-11ee-ba06-fb353e7798cd', '906d3c4e-f0be-11ee-b9fb-fb353e7798cd', '878e1a02-f092-11ee-b9cf-fb353e7798cd', '606347dc-ed12-11ee-9385-ef789ffde1d3', 'cccc7d32-f1c0-11ee-bada-fb353e7798cd', '6daff50c-f041-11ee-b972-fb353e7798cd', 'ba80ba8c-f0ce-11ee-ba0d-fb353e7798cd', 'bf518644-f1a6-11ee-bac9-fb353e7798cd', '3a116996-93a9-11ee-956e-9da2d070324c', '56b8baa4-f0b5-11ee-b9f0-fb353e7798cd', 'bb52690a-f066-11ee-b99e-fb353e7798cd', 'a437811e-ccf4-11ee-9435-f7e542e2436c', '7aa336e6-f0b1-11ee-b9ed-fb353e7798cd', '986a0b90-f215-11ee-bb15-fb353e7798cd', 'ef63db62-f051-11ee-b986-fb353e7798cd', 'd1d69a76-f0b5-11ee-b9f0-fb353e7798cd', '57a2192c-f21a-11ee-bb17-fb353e7798cd', 'e9e14d3a-eb17-11ee-b297-3b0ad9d5d6c6', '0b72a836-f37e-11ee-bb4e-fb353e7798cd', '8be24d52-f0ca-11ee-ba0a-fb353e7798cd', 'd4b936f6-eb36-11ee-b297-3b0ad9d5d6c6', '599673dc-f070-11ee-b9a9-fb353e7798cd', 'cc0299e6-eb3e-11ee-b297-3b0ad9d5d6c6', 'b6227b56-f08d-11ee-b9c9-fb353e7798cd', 'fe729430-f232-11ee-bb32-fb353e7798cd', 'eb22edf4-f0ab-11ee-b9e9-fb353e7798cd', 'fc1e1b6a-f1e6-11ee-baf6-fb353e7798cd', '61b4e416-f1da-11ee-bae8-fb353e7798cd', '31b48540-f09b-11ee-b9da-fb353e7798cd', '240ebe64-f0d3-11ee-ba14-fb353e7798cd', 'bfde2aec-f370-11ee-bb4e-fb353e7798cd', 'b2f58080-f223-11ee-bb20-fb353e7798cd', '4bbe3c64-f088-11ee-b9c3-fb353e7798cd', 'b2bfb60c-f0ad-11ee-b9ea-fb353e7798cd', 'ede139be-f098-11ee-b9d8-fb353e7798cd', 'bcf44e58-eb0d-11ee-b297-3b0ad9d5d6c6', 'a6895b74-f1cd-11ee-bae2-fb353e7798cd', 'e640cf0a-f096-11ee-b9d5-fb353e7798cd', '1f70a4f0-f0e0-11ee-ba1e-fb353e7798cd', '41cd65b4-f0aa-11ee-b9e8-fb353e7798cd', '621c07b8-eaaf-11ee-b297-3b0ad9d5d6c6', 'dbff355c-f0a2-11ee-b9e3-fb353e7798cd', '23a7aa3e-f048-11ee-b97d-fb353e7798cd', '5c2ad8ec-f08c-11ee-b9c8-fb353e7798cd', '079f0d30-eb09-11ee-b297-3b0ad9d5d6c6', 'baec243e-eacf-11ee-b297-3b0ad9d5d6c6', '5230b9be-f083-11ee-b9b8-fb353e7798cd' ] )

    blacklisted_gmIDs_set = gmIDs_set & blacklisted_gmIDs_set

    return list( blacklisted_gmIDs_set )


# In[ ]:


def random_list_split( input_list, split_percentage = 0.5 ):

    randomized_input_list = random.shuffle( input_list )

    #

    list_len = len( input_list )

    split_index = round( list_len * split_percentage )

    split_list1 = input_list[ : split_index ]

    split_list2 = input_list[ split_index : ]

    return split_list1, split_list2

