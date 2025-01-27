#!/usr/bin/env python
# coding: utf-8

# In[7]:


import numpy as np

import pandas as pd

import os


# In[8]:


def BinaryDrivingMode( chassis_df ):

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


# In[9]:


def TernaryDrivingModeTransition( time_sorted_chassis_df ):

    binary_drive_mode_lst = time_sorted_chassis_df[ 'BinaryDrivingMode' ].tolist()

    ternary_drive_mode_trans_lst = []
    
    for index in range( 1, len( binary_drive_mode_lst ) ):

        drive_mode_trans = binary_drive_mode_lst[ index ] - binary_drive_mode_lst[ index - 1 ]

        ternary_drive_mode_trans_lst.append( drive_mode_trans )

    ternary_drive_mode_trans_lst = [ 0 ] + ternary_drive_mode_trans_lst

    time_sorted_chassis_df[ 'TernaryDrivingModeTransition' ] = ternary_drive_mode_trans_lst


# In[10]:


def LatLonTotalStdDev( best_pose_df ):

    lat_stddev_lst = best_pose_df[ 'latitudeStdDev' ].tolist()

    lon_stddev_lst = best_pose_df[ 'longitudeStdDev' ].tolist()

    def planar_distance( x, y ): return ( x ** 2 + y ** 2 ) ** ( 1 / 2 )

    latlon_total_stddev_lst = []

    for lat_stddev, lon_stddev in zip( lat_stddev_lst, lon_stddev_lst ):

        latlon_total_stddev = planar_distance( lat_stddev, lon_stddev )

        latlon_total_stddev_lst.append( latlon_total_stddev )

    best_pose_df[ 'LatLonTotalStdDev' ] = latlon_total_stddev_lst


# In[11]:


def ChassisBestPoseMatchedTime( same_gmID_chassis_df, same_gmID_best_pose_df ):

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


# In[12]:


def ProgressAlongRoute( best_pose_df, time_sorted_reference_best_pose_df):

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


# In[13]:


def NormalizedTime( chassis_df ):

    chassis_time_array = np.array( chassis_df[ 'time' ] )

    normalized_chassis_time_array = chassis_time_array - np.min( chassis_time_array )

    chassis_df[ 'NormalizedTime' ] = list( normalized_chassis_time_array )


# In[14]:


def DeltaTime( time_sorted_chassis_df ):

    chassis_time_array = np.array( time_sorted_chassis_df[ 'time' ] )

    chassis_delta_time_array = np.diff( chassis_time_array )

    chassis_delta_time_list = list( chassis_delta_time_array )

    chassis_delta_time_list = [ chassis_delta_time_list[ 0 ] ] + chassis_delta_time_list

    time_sorted_chassis_df[ 'DeltaTime' ] = chassis_delta_time_list


# In[15]:


def Distance( time_sorted_chassis_df ):

    # Legacy

    chassis_DeltaTime_array = np.array( time_sorted_chassis_df[ 'DeltaTime' ] ) * 1e-9 # seconds

    chassis_speedMps_array = np.array( time_sorted_chassis_df[ 'speedMps' ] ) # meters/second

    chassis_Distance_list = [] # meters

    for index in range( len( chassis_DeltaTime_array ) ):

        current_index_Distance = np.sum( chassis_DeltaTime_array[ : index ] * chassis_speedMps_array[ : index ] )

        chassis_Distance_list.append( current_index_Distance )

    time_sorted_chassis_df[ 'Distance' ] = chassis_Distance_list


# ### Functions unrelated to calculated fields but are important vvv

# In[16]:


def origin_dir():

    home_dir_list = os.listdir( '/home' )

    for dir in home_dir_list:

        if '_linux' in dir:

            path = f'/home/{dir}/Desktop/TDMprivate'

            if not os.path.exists( path ):

                raise Exception( 'TDMprivate folder does not exist. TDMprivate folder must exist on Desktop. Notify Ryan or ' +
                                 'Vincent if this message appears.' )

            else:

                return path


# In[17]:


def retrieve_metadata_df():

    path = f'{ origin_dir() }/metadata/metadata.csv'

    metadata_df = pd.read_csv( f'{ origin_dir() }/metadata/metadata.csv' )

    return metadata_df


# In[21]:


def list_gmIDs():

    path = f'{ origin_dir() }/data'

    gmID_list = [ file for file in os.listdir( path ) if os.path.isdir( f'{ path }/{ file }' ) ]

    return gmID_list


# In[19]:


def list_topics():

    gmID_list = list_gmIDs()

    path = f'{ origin_dir() }/data/{ gmID_list[ 0 ] }'

    topic_list = [ file for file in os.listdir( path ) if os.path.isdir( f'{ path }/{ file }' ) ]

    topic_list = [ topic.replace( '_', '/' ) for topic in topic_list ]

    return topic_list


# In[20]:


def retrieve_gmID_topic( gmID, topic ):

    dir_friendly_topic = topic.replace( '/', '_' )

    path = f'{ origin_dir() }/data/{ gmID }/{ dir_friendly_topic }/{ gmID + dir_friendly_topic }.csv'

    gmID_topic_df = pd.read_csv( path )

    return gmID_topic_df


# In[ ]:






