#install.packages("tibble")
#install.packages("dplyr")
#install.packages(ggplot2)
#install.packages("caret")
#install.packages("glmnet")

library(tibble)
library(dplyr)
library(ggplot2)
library(caret)
library(pROC)
library(ROSE)
library(glmnet)

load_chassis_data <- function(gmID) {
  # Construct the new path based on the updated directory structure
  path <- file.path("data", gmID, "_apollo_canbus_chassis", paste0(gmID, "_apollo_canbus_chassis.csv"))
  
  # Check if the file exists
  if (!file.exists(path)) {
    stop(paste("The file does not exist at the specified path:", path))
  }
  
  # Load the CSV file into a dataframe
  chassis_data <- read.csv(path)
  
  # Return the loaded dataframe
  return(chassis_data)
}


# Function to add a binary driving mode column
BinaryDrivingMode <- function(chassis_df) {
  # Validate input: Check if the required column exists
  if (!"drivingMode" %in% colnames(chassis_df)) {
    stop("The input dataframe must contain a 'drivingMode' column.")
  }

  # Add a new column 'BinaryDrivingMode' based on conditions
  chassis_df$BinaryDrivingMode <- sapply(chassis_df$drivingMode, function(drive_mode) {
    if (drive_mode %in% c("COMPLETE_MANUAL", "EMERGENCY_MODE")) {
      return(0)  # Manual modes
    } else if (drive_mode == "COMPLETE_AUTO_DRIVE") {
      return(1)  # Automatic mode
    } else {
      stop(paste("Unknown driving mode:", drive_mode))
    }
  })

  # Return the modified dataframe
  return(chassis_df)
}


# Function to calculate ternary driving mode transitions
TernaryDrivingModeTransition <- function(time_sorted_chassis_df) {
  # Validate input: Check if the required column exists
  if (!"BinaryDrivingMode" %in% colnames(time_sorted_chassis_df)) {
    stop("The input dataframe must contain a 'BinaryDrivingMode' column.")
  }

  # Create the 'TernaryDrivingModeTransition' column
  binary_drive_mode_lst <- time_sorted_chassis_df$BinaryDrivingMode
  
  # Calculate the transitions
  ternary_drive_mode_trans_lst <- c(0, diff(binary_drive_mode_lst))
  
  # Assign the new column to the dataframe
  time_sorted_chassis_df$TernaryDrivingModeTransition <- ternary_drive_mode_trans_lst
  
  # Return the modified dataframe
  return(time_sorted_chassis_df)
}


# Function to calculate total standard deviation of latitude and longitude
LatLonStdDev <- function(best_pose_df) {
    if (!all(c("latitudeStdDev", "longitudeStdDev") %in% colnames(best_pose_df))) {
        stop("The input dataframe must contain 'latitudeStdDev' and 'longitudeStdDev' columns.")
    }
    best_pose_df$latlongStdDev <- sqrt(best_pose_df$latitudeStdDev^2 + best_pose_df$longitudeStdDev^2)
    return(best_pose_df)
}


ChassisBestPoseMatchedTime <- function(same_gmID_chassis_df, same_gmID_best_pose_df) {
  # Validate input: Check if the required column exists in both dataframes
  if (!"time" %in% colnames(same_gmID_chassis_df) || !"time" %in% colnames(same_gmID_best_pose_df)) {
    stop("Both dataframes must contain a 'time' column.")
  }

  # Convert time columns to vectors
  chassis_time_array <- same_gmID_chassis_df$time
  best_pose_time_array <- same_gmID_best_pose_df$time
  
  # Initialize the matched time column
  chassis_best_pose_matched_time_lst <- sapply(chassis_time_array, function(chassis_time) {
    # Compute the absolute differences
    time_diff_array <- abs(best_pose_time_array - chassis_time)
    # Find the index of the minimum difference
    min_index <- which.min(time_diff_array)
    # Return the closest time
    return(best_pose_time_array[min_index])
  })

  # Add the 'ChassisBestPoseMatchedTime' column to both dataframes
  same_gmID_chassis_df$ChassisBestPoseMatchedTime <- chassis_best_pose_matched_time_lst
  same_gmID_best_pose_df$ChassisBestPoseMatchedTime <- same_gmID_best_pose_df$time

  # Return the modified dataframes as a list
  return(list(chassis_df = same_gmID_chassis_df, best_pose_df = same_gmID_best_pose_df))
}


ProgressAlongRoute_v2 <- function(time_sorted_best_pose_df, time_sorted_reference_best_pose_df = "auto",
                                  num_of_partitions = 100, max_distance_coefficient = 5) {
  # Step 1: Handle the 'auto' case for the reference dataframe
  if (time_sorted_reference_best_pose_df == "auto") {
    if (time_sorted_best_pose_df$groupMetadataID[1] == "Red") {
      reference_best_pose_gmID <- "9798fe24-f143-11ee-ba78-fb353e7798cd"
    } else if (time_sorted_best_pose_df$groupMetadataID[1] == "Green") {
      reference_best_pose_gmID <- "3a7dc9a6-f042-11ee-b974-fb353e7798cd"
    } else {
      reference_best_pose_gmID <- "3d2a80f0-ec81-11ee-b297-3b0ad9d5d6c6"
    }

    # Simulated data retrieval for the reference dataframe
    time_sorted_reference_best_pose_df <- retrieve_gmID_topic(
      gmID = reference_best_pose_gmID,
      topic = "/apollo/sensor/gnss/best/pose"
    )
  }

  # Ensure the reference dataframe is sorted by time
  time_sorted_reference_best_pose_df <- time_sorted_reference_best_pose_df[order(time_sorted_reference_best_pose_df$time), ]

  # Step 2: Calculate reference progress
  reference_latitude_array <- time_sorted_reference_best_pose_df$latitude
  reference_longitude_array <- time_sorted_reference_best_pose_df$longitude

  reference_delta_latitude_array <- diff(reference_latitude_array)
  reference_delta_longitude_array <- diff(reference_longitude_array)

  reference_delta_distance_analog_array <- sqrt(reference_delta_latitude_array^2 + reference_delta_longitude_array^2)

  reference_ProgressAlongRoute_list <- c(0)
  for (index in seq_along(reference_delta_distance_analog_array)) {
    running_delta_distance_analog <- tail(reference_ProgressAlongRoute_list, 1) + reference_delta_distance_analog_array[index]
    reference_ProgressAlongRoute_list <- c(reference_ProgressAlongRoute_list, running_delta_distance_analog)
  }

  reference_ProgressAlongRoute_array <- reference_ProgressAlongRoute_list / max(reference_ProgressAlongRoute_list)

  # Step 3: Partitioning
  partition_size <- 1 / num_of_partitions
  reference_ProgressAlongRoute_partition_array <- floor(reference_ProgressAlongRoute_array / partition_size)
  reference_ProgressAlongRoute_partition_array[reference_ProgressAlongRoute_partition_array == num_of_partitions] <- num_of_partitions - 1

  # Step 4: Calculate progress for the current dataframe
  latitude_array <- time_sorted_best_pose_df$latitude
  longitude_array <- time_sorted_best_pose_df$longitude

  ProgressAlongRoute_list <- c()
  ProgressAlongRoute_partition_list <- c()

  avg_reference_delta_distance_analog <- median(reference_delta_distance_analog_array)
  max_distance_analog <- avg_reference_delta_distance_analog * max_distance_coefficient

  full_scan <- TRUE

  for (index in seq_along(latitude_array)) {
    latitude <- latitude_array[index]
    longitude <- longitude_array[index]

    if (full_scan) {
      # Calculate distances for all reference points
      distance_analog_array <- sqrt((reference_latitude_array - latitude)^2 +
                                      (reference_longitude_array - longitude)^2)
      min_distance_analog <- min(distance_analog_array)

      if (min_distance_analog > max_distance_analog) {
        ProgressAlongRoute_list <- c(ProgressAlongRoute_list, NA)
        ProgressAlongRoute_partition_list <- c(ProgressAlongRoute_partition_list, NA)
        next
      }

      full_scan <- FALSE
      min_distance_analog_index <- which.min(distance_analog_array)
      assigned_ProgressAlongRoute <- reference_ProgressAlongRoute_array[min_distance_analog_index]
      assigned_ProgressAlongRoute_partition <- reference_ProgressAlongRoute_partition_array[min_distance_analog_index]
    } else {
      # Adjust the subset of reference points based on partitions
      reference_subset_middle_partition_num <- ProgressAlongRoute_partition_list[length(ProgressAlongRoute_partition_list)]
      reference_subset_lower_partition_num <- reference_subset_middle_partition_num - 1
      reference_subset_upper_partition_num <- reference_subset_middle_partition_num + 1

      # Ensure bounds for the partition numbers
      if (reference_subset_lower_partition_num < 0) {
        reference_subset_lower_partition_num <- 0
      }
      if (reference_subset_upper_partition_num >= num_of_partitions) {
        reference_subset_upper_partition_num <- num_of_partitions - 1
      }

      # Subset the indices based on partition
      reference_subset_indices <- which(
        reference_ProgressAlongRoute_partition_array %in% c(
          reference_subset_lower_partition_num,
          reference_subset_middle_partition_num,
          reference_subset_upper_partition_num
        )
      )

      reference_subset_latitude_array <- reference_latitude_array[reference_subset_indices]
      reference_subset_longitude_array <- reference_longitude_array[reference_subset_indices]
      reference_subset_ProgressAlongRoute_array <- reference_ProgressAlongRoute_array[reference_subset_indices]
      reference_subset_ProgressAlongRoute_partition_array <- reference_ProgressAlongRoute_partition_array[reference_subset_indices]

      # Calculate distances within the subset
      subset_distance_analog_array <- sqrt(
        (reference_subset_latitude_array - latitude)^2 +
          (reference_subset_longitude_array - longitude)^2
      )

      min_subset_distance_analog <- min(subset_distance_analog_array)

      # Check if the minimum subset distance exceeds the threshold
      if (min_subset_distance_analog > max_distance_analog) {
        ProgressAlongRoute_list <- c(ProgressAlongRoute_list, NA)
        ProgressAlongRoute_partition_list <- c(ProgressAlongRoute_partition_list, NA)
        full_scan <- TRUE
        next
      }

      # Assign the progress and partition for the closest match in the subset
      min_subset_distance_analog_index <- which.min(subset_distance_analog_array)
      assigned_ProgressAlongRoute <- reference_subset_ProgressAlongRoute_array[min_subset_distance_analog_index]
      assigned_ProgressAlongRoute_partition <- reference_subset_ProgressAlongRoute_partition_array[min_subset_distance_analog_index]
    }

    ProgressAlongRoute_list <- c(ProgressAlongRoute_list, assigned_ProgressAlongRoute)
    ProgressAlongRoute_partition_list <- c(ProgressAlongRoute_partition_list, assigned_ProgressAlongRoute_partition)
  }

  # Add results to the dataframe
  time_sorted_best_pose_df$ProgressAlongRoute <- ProgressAlongRoute_list
  time_sorted_best_pose_df$PartitionNumber <- ProgressAlongRoute_partition_list

  # Check for NA values in the ProgressAlongRoute column
  if (any(is.na(time_sorted_best_pose_df$ProgressAlongRoute))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

NormalizedTime <- function(topic_df) {
  # Validate that the dataframe contains a 'time' column
  if (!"time" %in% colnames(topic_df)) {
    stop("The input dataframe must contain a 'time' column.")
  }

  # Step 1: Extract the 'time' column
  topic_time_array <- topic_df$time

  # Step 2: Calculate the normalized time
  normalized_topic_time_array <- topic_time_array - min(topic_time_array)

  # Step 3: Add the normalized time as a new column
  topic_df$NormalizedTime <- normalized_topic_time_array

  # Return the modified dataframe
  return(topic_df)
}

DeltaTime <- function(time_sorted_topic_df) {
  # Validate that the dataframe contains a 'time' column
  if (!"time" %in% colnames(time_sorted_topic_df)) {
    stop("The input dataframe must contain a 'time' column.")
  }

  # Step 1: Extract the 'time' column
  topic_time_array <- time_sorted_topic_df$time

  # Step 2: Calculate the time differences
  topic_delta_time_array <- diff(topic_time_array)

  # Step 3: Add a leading 0 to the delta time array to match dataframe length
  topic_delta_time_list <- c(0, topic_delta_time_array)

  # Step 4: Add the delta time as a new column
  time_sorted_topic_df$DeltaTime <- topic_delta_time_list

  # Return the modified dataframe
  return(time_sorted_topic_df)
}


Distance <- function(time_sorted_chassis_df) {
  # Validate that the dataframe contains the necessary columns
  if (!all(c("DeltaTime", "speedMps") %in% colnames(time_sorted_chassis_df))) {
    stop("The input dataframe must contain 'DeltaTime' and 'speedMps' columns.")
  }

  # Step 1: Extract the required columns
  chassis_DeltaTime_array <- time_sorted_chassis_df$DeltaTime * 1e-9  # Convert nanoseconds to seconds
  chassis_speedMps_array <- time_sorted_chassis_df$speedMps  # Speed in meters per second

  # Step 2: Calculate the cumulative distance
  chassis_Distance_list <- c()
  for (index in seq_along(chassis_DeltaTime_array)) {
    current_index_Distance <- sum(chassis_DeltaTime_array[1:index] * chassis_speedMps_array[1:index])
    chassis_Distance_list <- c(chassis_Distance_list, current_index_Distance)
  }

  # Step 3: Add the distance as a new column
  time_sorted_chassis_df$Distance <- chassis_Distance_list

  # Return the modified dataframe
  return(time_sorted_chassis_df)
}


MergeChassisDriveEvent <- function(chassis_df, drive_event_df) {
  # Validate that the dataframes contain the necessary columns
  if (!"time" %in% colnames(chassis_df)) {
    stop("The chassis dataframe must contain a 'time' column.")
  }
  if (!all(c("time", "event", "type") %in% colnames(drive_event_df))) {
    stop("The drive_event dataframe must contain 'time', 'event', and 'type' columns.")
  }

  # Step 1: Extract the required columns
  chassis_time_array <- chassis_df$time
  drive_event_time_array <- drive_event_df$time
  drive_event_event_array <- drive_event_df$event
  drive_event_type_array <- drive_event_df$type

  # Step 2: Initialize arrays to store DriveEvent and DriveEventType
  chassis_num_rows <- nrow(chassis_df)
  chassis_event_array <- rep(NA, chassis_num_rows)
  chassis_type_array <- rep(NA, chassis_num_rows)

  # Step 3: Match drive events to chassis timestamps
  for (i in seq_along(drive_event_time_array)) {
    drive_event_time <- drive_event_time_array[i]
    drive_event_event <- drive_event_event_array[i]
    drive_event_type <- drive_event_type_array[i]

    # Calculate absolute time differences
    abs_time_diff_array <- abs(chassis_time_array - drive_event_time)
    min_abs_time_diff_index <- which.min(abs_time_diff_array)

    # Assign the closest event and type to the corresponding chassis row
    chassis_event_array[min_abs_time_diff_index] <- drive_event_event
    chassis_type_array[min_abs_time_diff_index] <- drive_event_type
  }

  # Step 4: Add the DriveEvent and DriveEventType columns to the chassis dataframe
  chassis_df$DriveEvent <- chassis_event_array
  chassis_df$DriveEventType <- chassis_type_array

  # Return the modified chassis dataframe
  return(chassis_df)
}


DistanceToNearestDisengagement <- function(time_sorted_chassis_df) {
  # Validate that the dataframe contains the necessary columns
  if (!all(c("time", "speedMps", "TernaryDrivingModeTransition", "groupMetadataID") %in% colnames(time_sorted_chassis_df))) {
    stop("The dataframe must contain 'time', 'speedMps', 'TernaryDrivingModeTransition', and 'groupMetadataID' columns.")
  }

  # Step 1: Extract required columns
  chassis_time_array <- time_sorted_chassis_df$time * 1e-9  # Convert nanoseconds to seconds
  chassis_speedMps_array <- time_sorted_chassis_df$speedMps
  chassis_TernaryDrivingModeTransition_array <- time_sorted_chassis_df$TernaryDrivingModeTransition
  gmID <- time_sorted_chassis_df$groupMetadataID[1]

  # Step 2: Calculate delta time and cumulative distance
  chassis_deltatime_array <- diff(c(0, chassis_time_array))
  chassis_deltadistance_array <- chassis_deltatime_array * chassis_speedMps_array
  chassis_distance_array <- cumsum(chassis_deltadistance_array)

  # Step 3: Find disengagement indexes and distances
  chassis_disengagement_indexes <- which(chassis_TernaryDrivingModeTransition_array == -1)
  chassis_disengagement_distance_array <- chassis_distance_array[chassis_disengagement_indexes]

  # Step 4: Handle cases with no disengagements
  if (length(chassis_disengagement_indexes) == 0) {
    nan_list <- rep(NA, length(chassis_time_array))
    time_sorted_chassis_df$DistanceToNearestDisengagement <- nan_list
    time_sorted_chassis_df$NearestDisengagementID <- nan_list
    return(time_sorted_chassis_df)
  }

  # Step 5: Calculate distances to disengagements
  distance_to_disengagement_array_list <- lapply(chassis_disengagement_distance_array, function(disengagement_distance) {
    chassis_distance_array - disengagement_distance
  })
  distance_to_disengagement_array_matrix <- do.call(cbind, distance_to_disengagement_array_list)

  # Step 6: Find nearest disengagement for each time step
  chassis_DistanceToNearestDisengagement_list <- numeric()
  chassis_NearestDisengagementID_list <- character()

  for (colnum in seq_len(ncol(distance_to_disengagement_array_matrix))) {
    col <- distance_to_disengagement_array_matrix[, colnum]
    abs_col <- abs(col)
    min_abs_index <- which.min(abs_col)

    DistanceToNearestDisengagement <- col[min_abs_index]
    NearestDisengagementID <- paste0(gmID, "_", min_abs_index)

    chassis_DistanceToNearestDisengagement_list <- c(chassis_DistanceToNearestDisengagement_list, DistanceToNearestDisengagement)
    chassis_NearestDisengagementID_list <- c(chassis_NearestDisengagementID_list, NearestDisengagementID)
  }

  # Step 7: Add results to the dataframe
  time_sorted_chassis_df$DistanceToNearestDisengagement <- chassis_DistanceToNearestDisengagement_list
  time_sorted_chassis_df$NearestDisengagementID <- chassis_NearestDisengagementID_list

  # Return the updated dataframe
  return(time_sorted_chassis_df)
}


Acceleration_chassistime <- function(time_sorted_chassis_df) {
  # Validate that the dataframe contains the necessary columns
  if (!all(c("speedMps", "time") %in% colnames(time_sorted_chassis_df))) {
    stop("The dataframe must contain 'speedMps' and 'time' columns.")
  }

  # Step 1: Extract and calculate required arrays
  speedMps_array <- time_sorted_chassis_df$speedMps  # Speed in meters per second
  time_array <- diff(c(0, time_sorted_chassis_df$time)) * 1e-9  # Time difference in seconds

  # Step 2: Initialize acceleration list
  acceleration_list <- numeric(length(speedMps_array))  # Initialize with zeros

  # Step 3: Calculate acceleration for each time step
  for (index in 2:length(speedMps_array)) {
    acceleration <- (speedMps_array[index] - speedMps_array[index - 1]) / time_array[index]
    acceleration_list[index] <- acceleration
  }

  # Step 4: Add the acceleration column to the dataframe
  time_sorted_chassis_df$Acceleration <- acceleration_list

  # Return the modified dataframe
  return(time_sorted_chassis_df)
}


Acceleration_bestposetime <- function(time_sorted_merged_chassisbestpose_df) {
  # Validate that the dataframe contains the necessary columns
  if (!all(c("time_y", "time_x", "speedMps") %in% colnames(time_sorted_merged_chassisbestpose_df))) {
    stop("The dataframe must contain 'time_y', 'time_x', and 'speedMps' columns.")
  }

  # Step 1: Extract unique best pose times
  unique_best_pose_time_array <- unique(time_sorted_merged_chassisbestpose_df$time_y)

  # Step 2: Initialize acceleration list
  acceleration_list <- numeric()

  # Step 3: Iterate over each unique best pose time
  for (best_pose_time in unique_best_pose_time_array) {
    # Subset the dataframe for the current best pose time
    subset_df <- time_sorted_merged_chassisbestpose_df[time_sorted_merged_chassisbestpose_df$time_y == best_pose_time, ]

    # Extract necessary columns
    chassis_time_array_subset <- diff(c(0, subset_df$time_x)) * 1e-9  # Convert to seconds
    speedMps_array_subset <- subset_df$speedMps

    # Calculate acceleration for the subset
    if (length(speedMps_array_subset) > 1) {
      acceleration <- (tail(speedMps_array_subset, 1) - head(speedMps_array_subset, 1)) /
                      (tail(chassis_time_array_subset, 1) - head(chassis_time_array_subset, 1))
      acceleration_list <- c(acceleration_list, acceleration)
    } else {
      acceleration_list <- c(acceleration_list, NA)  # If only one entry, acceleration is undefined
    }
  }

  # Step 4: Add the acceleration column to the dataframe
  time_sorted_merged_chassisbestpose_df$Acceleration <- acceleration_list

  # Return the modified dataframe
  return(time_sorted_merged_chassisbestpose_df)
}


origin_dir <- function() {
  # List all directories in the '/home' path
  home_dir_list <- list.files("/home")

  # Iterate over each directory in the list
  for (dir in home_dir_list) {
    if (grepl("_linux", dir)) {
      # Construct the path
      path <- file.path("/home", dir, "Desktop", "TDMprivate")

      # Check if the path exists
      if (!dir.exists(path)) {
        stop("TDMprivate folder does not exist. TDMprivate folder must exist on Desktop. Notify Ryan or Vincent if this message appears.")
      } else {
        return(path)
      }
    }
  }

  # If no directory matches the condition
  stop("No '_linux' directory found in '/home'. Ensure the directory structure is correct.")
}


##
retrieve_metadata_df <- function() {
  # Define the path to the metadata CSV file
  path <- file.path(origin_dir(), "metadata", "metadata.csv")

  # Read the metadata CSV file
  if (!file.exists(path)) {
    stop("The metadata file does not exist at the specified path.")
  }
  
  metadata_df <- read.csv(path)

  # Return the dataframe
  return(metadata_df)
}


##
list_gmIDs <- function() {
  # Define the path to the data folder
  path <- file.path(origin_dir(), "data")

  # Check if the path exists
  if (!dir.exists(path)) {
    stop("The data folder does not exist.")
  }

  # List directories within the data folder
  gmID_list <- list.dirs(path, full.names = FALSE, recursive = FALSE)

  # Return the list of groupMetadataIDs
  return(gmID_list)
}


list_topics <- function() {
  # Get the list of groupMetadataIDs
  gmID_list <- list_gmIDs()

  # Check if gmID_list is not empty
  if (length(gmID_list) == 0) {
    stop("No groupMetadataIDs found.")
  }

  # Define the path to the first groupMetadataID's data folder
  path <- file.path(origin_dir(), "data", gmID_list[1])

  # Check if the path exists
  if (!dir.exists(path)) {
    stop(paste("The path does not exist:", path))
  }

  # List directories within the groupMetadataID's data folder
  topic_list <- list.dirs(path, full.names = FALSE, recursive = FALSE)

  # Replace underscores ('_') with slashes ('/') in the topic names
  topic_list <- gsub("_", "/", topic_list)

  # Return the list of topics
  return(topic_list)
}

retrieve_gmID_topic <- function(gmID, topic) {
  # Replace '/' with '_' in the topic name
  dir_friendly_topic <- gsub("/", "_", topic)

  # Construct the file path
  path <- file.path(origin_dir(), "data", gmID, dir_friendly_topic, paste0(gmID, dir_friendly_topic, ".csv"))

  # Check if the file exists
  if (!file.exists(path)) {
    stop(paste("The file does not exist at the specified path:", path))
  }

  # Read the CSV file into a dataframe
  gmID_topic_df <- read.csv(path)

  # Return the dataframe
  return(gmID_topic_df)
}

  # Define the list of gmIDs for the red route
red_route_gmID_list <- c(
  "2f95c748-f009-11ee-b966-fb353e7798cd",
  "65cfbfd6-f396-11ee-bb4e-fb353e7798cd",
  "f711e68e-f0e1-11ee-ba1f-fb353e7798cd",
  "171c50bc-f106-11ee-ba42-fb353e7798cd",
  "6d2ea45a-c839-11ee-a7fc-dd032dba19e8",
  "05c7c824-cab8-11ee-aa4d-1d66adf2f0c7",
  "96f7a614-f549-11ee-8afa-cb629b0d53e6",
  "cf831f42-f353-11ee-bb4e-fb353e7798cd",
  "f0eebb6a-f0dc-11ee-ba1e-fb353e7798cd",
  "9798fe24-f143-11ee-ba78-fb353e7798cd",
  "ce6465b6-f51b-11ee-8afa-cb629b0d53e6",
  "3d2d29ec-ef95-11ee-b966-fb353e7798cd",
  "d3698592-ef9d-11ee-b966-fb353e7798cd",
  "853ef120-cad3-11ee-909c-e1dc60cf66f9",
  "be857244-efc0-11ee-b966-fb353e7798cd",
  "f43b6a70-f01e-11ee-b966-fb353e7798cd",
  "fcc6fcd2-f013-11ee-b966-fb353e7798cd",
  "e7b934a8-ef1a-11ee-9385-ef789ffde1d3",
  "61b12e7a-f234-11ee-bb33-fb353e7798cd",
  "de933de8-f112-11ee-ba4d-fb353e7798cd",
  "8dbbbf1c-f0ef-11ee-ba29-fb353e7798cd",
  "fe973c9c-f53c-11ee-8afa-cb629b0d53e6",
  "ecebb942-f162-11ee-ba97-fb353e7798cd",
  "f755cf60-f132-11ee-ba6d-fb353e7798cd",
  "d24820c8-f197-11ee-babe-fb353e7798cd",
  "dd72fdec-f0cf-11ee-ba0d-fb353e7798cd",
  "286e019a-f204-11ee-bb07-fb353e7798cd",
  "41b67a28-f52f-11ee-8afa-cb629b0d53e6",
  "c0555ef0-f50f-11ee-8afa-cb629b0d53e6",
  "72a03d4a-efe9-11ee-b966-fb353e7798cd",
  "457dc5ee-f02a-11ee-b966-fb353e7798cd",
  "3151e9e2-eff3-11ee-b966-fb353e7798cd",
  "c25271be-f3a4-11ee-bb4e-fb353e7798cd",
  "2a61b8a8-f528-11ee-8afa-cb629b0d53e6",
  "01e65360-efd4-11ee-b966-fb353e7798cd",
  "1bbbfbae-c839-11ee-a7fc-dd032dba19e8",
  "f41cbd44-eff8-11ee-b966-fb353e7798cd",
  "aa5dbcd2-ef10-11ee-9385-ef789ffde1d3",
  "84d96f18-f214-11ee-bb13-fb353e7798cd",
  "94c53148-eeed-11ee-9385-ef789ffde1d3",
  "88a68dd8-eef9-11ee-9385-ef789ffde1d3",
  "85b6e70e-ef7a-11ee-b966-fb353e7798cd",
  "1b6aca0e-efdf-11ee-b966-fb353e7798cd",
  "8fa6fe80-c869-11ee-a7fc-dd032dba19e8",
  "51ef6da6-ca9f-11ee-909c-e1dc60cf66f9",
  "9189a2a8-f121-11ee-ba5b-fb353e7798cd",
  "8347b862-efad-11ee-b966-fb353e7798cd",
  "88dd6fbe-f224-11ee-bb21-fb353e7798cd",
  "817d6848-efb6-11ee-b966-fb353e7798cd",
  "fc211bb2-efca-11ee-b966-fb353e7798cd",
  "c338788a-d324-11ee-b437-336917683bb8",
  "b82476fe-f1f3-11ee-baff-fb353e7798cd",
  "7cbd932e-f244-11ee-bb3f-fb353e7798cd",
  "2462c9d0-eecd-11ee-9385-ef789ffde1d3",
  "3344a3c0-f502-11ee-8afa-cb629b0d53e6",
  "d21965e6-f0fa-11ee-ba37-fb353e7798cd",
  "c9c6856c-d33c-11ee-b437-336917683bb8",
  "d12cd1c4-caec-11ee-909c-e1dc60cf66f9",
  "43a1a35e-f362-11ee-bb4e-fb353e7798cd",
  "fd1ab258-efa7-11ee-b966-fb353e7798cd",
  "8437f77a-cab7-11ee-909c-e1dc60cf66f9",
  "211bdb36-f0da-11ee-ba1b-fb353e7798cd",
  "868de15e-f3b3-11ee-bb4e-fb353e7798cd",
  "35518ec4-f153-11ee-ba88-fb353e7798cd",
  "622bd2e8-f0e4-11ee-ba1f-fb353e7798cd",
  "7fb7b9c0-c881-11ee-a7fc-dd032dba19e8",
  "219f7eb8-ef87-11ee-b966-fb353e7798cd",
  "1ee938a2-f172-11ee-baa6-fb353e7798cd",
  "5a4bccf4-effe-11ee-b966-fb353e7798cd"
)

  # Define the list of gmIDs for the green route
green_route_gmID_list <- c(
  "40706f50-f03b-11ee-b96e-fb353e7798cd",
  "9df14b4e-f172-11ee-baa6-fb353e7798cd",
  "88b0613a-d35d-11ee-b437-336917683bb8",
  "7e3d64da-f12d-11ee-ba68-fb353e7798cd",
  "fa9cba86-f0f0-11ee-ba2a-fb353e7798cd",
  "7948628e-f20b-11ee-bb0f-fb353e7798cd",
  "a231c0b0-f142-11ee-ba76-fb353e7798cd",
  "a901fe40-f0fd-11ee-ba39-fb353e7798cd",
  "99b9f446-f1b2-11ee-bad3-fb353e7798cd",
  "c59a54e0-f179-11ee-baab-fb353e7798cd",
  "25641404-cb66-11ee-909c-e1dc60cf66f9",
  "14b6bc9c-f064-11ee-b998-fb353e7798cd",
  "5c7a9ab2-f13b-11ee-ba72-fb353e7798cd",
  "f8fd0fd8-f243-11ee-bb3f-fb353e7798cd",
  "4c88757c-f157-11ee-ba89-fb353e7798cd",
  "ba87f3ec-f07e-11ee-b9b4-fb353e7798cd",
  "58d78342-f24a-11ee-bb45-fb353e7798cd",
  "de493be2-f10f-11ee-ba4b-fb353e7798cd",
  "848e44a6-f134-11ee-ba6d-fb353e7798cd",
  "3d8020aa-cb7f-11ee-909c-e1dc60cf66f9",
  "c4fca7bc-f18e-11ee-bab8-fb353e7798cd",
  "c4146d46-f074-11ee-b9ac-fb353e7798cd",
  "fe0395f0-f1ea-11ee-baf9-fb353e7798cd",
  "271fee10-cb8b-11ee-909c-e1dc60cf66f9",
  "5fc763f6-f1ab-11ee-bacd-fb353e7798cd",
  "b3ee0dd8-f0d7-11ee-ba18-fb353e7798cd",
  "a08a8c7e-f1fb-11ee-bb05-fb353e7798cd",
  "7f824ea2-f05e-11ee-b993-fb353e7798cd",
  "96ceec56-f1cf-11ee-bae4-fb353e7798cd",
  "0f3cdf60-f1f6-11ee-bb00-fb353e7798cd",
  "53fad09e-f0f7-11ee-ba2f-fb353e7798cd",
  "f570c51c-f15d-11ee-ba91-fb353e7798cd",
  "cbdc93f4-f255-11ee-bb4e-fb353e7798cd",
  "d7cb9c92-f164-11ee-ba97-fb353e7798cd",
  "7a22a34c-f1f0-11ee-bafe-fb353e7798cd",
  "d454c586-f11c-11ee-ba55-fb353e7798cd",
  "837fc882-cb5a-11ee-909c-e1dc60cf66f9",
  "8e5c4fc2-f149-11ee-ba7f-fb353e7798cd",
  "c2f54552-f06f-11ee-b9a9-fb353e7798cd",
  "43abeb00-f206-11ee-bb07-fb353e7798cd",
  "25135418-f250-11ee-bb4a-fb353e7798cd",
  "f9c5e53e-f0ea-11ee-ba28-fb353e7798cd",
  "4cf81634-f238-11ee-bb34-fb353e7798cd",
  "d846a080-f115-11ee-ba51-fb353e7798cd",
  "1c74d294-f1e4-11ee-baf0-fb353e7798cd",
  "3c415ade-d353-11ee-b437-336917683bb8",
  "8adb6498-f04d-11ee-b981-fb353e7798cd",
  "de226278-f25a-11ee-bb4e-fb353e7798cd",
  "c14299be-f180-11ee-bab0-fb353e7798cd",
  "870cfd32-f1b9-11ee-bad5-fb353e7798cd",
  "3ed4aa16-f1d6-11ee-bae6-fb353e7798cd",
  "df6c3fb4-f200-11ee-bb07-fb353e7798cd",
  "5f7ce340-f1c8-11ee-bae0-fb353e7798cd",
  "b31aca98-cb95-11ee-909c-e1dc60cf66f9",
  "f12112ba-f1c0-11ee-bada-fb353e7798cd",
  "73bc30cc-f150-11ee-ba84-fb353e7798cd",
  "3a7dc9a6-f042-11ee-b974-fb353e7798cd",
  "dea29156-f123-11ee-ba5d-fb353e7798cd",
  "044d976e-f0e5-11ee-ba20-fb353e7798cd",
  "961fd9cc-f103-11ee-ba3f-fb353e7798cd",
  "bbbd0cc6-f0dc-11ee-ba1e-fb353e7798cd",
  "9736e77c-f187-11ee-bab6-fb353e7798cd",
  "5774dcde-f196-11ee-babe-fb353e7798cd",
  "ece2a8be-f047-11ee-b97d-fb353e7798cd",
  "3ec95686-f053-11ee-b988-fb353e7798cd",
  "51b74168-f19d-11ee-babf-fb353e7798cd",
  "bb4d37d4-f109-11ee-ba46-fb353e7798cd",
  "cf7148d8-f058-11ee-b98a-fb353e7798cd",
  "bf9157f0-f16b-11ee-ba9e-fb353e7798cd",
  "98692fde-f1a4-11ee-bac6-fb353e7798cd",
  "25e27b86-f06a-11ee-b9a3-fb353e7798cd",
  "a6539bd2-cb72-11ee-909c-e1dc60cf66f9",
  "5afabc8c-f035-11ee-b966-fb353e7798cd",
  "c1b320e2-f079-11ee-b9b0-fb353e7798cd",
  "d1a3a310-f091-11ee-b9ce-fb353e7798cd",
  "e9a1d768-f23d-11ee-bb39-fb353e7798cd",
  "c9be2042-f0de-11ee-ba1e-fb353e7798cd",
  "ed352100-cba0-11ee-909c-e1dc60cf66f9",
  "8b0593cc-cb4e-11ee-909c-e1dc60cf66f9",
  "236836f6-f1dd-11ee-bae8-fb353e7798cd",
  "072ef896-cbac-11ee-909c-e1dc60cf66f9"
)

  # Define the list of gmIDs for the blue route
blue_route_gmID_list <- c(
  "20f0b890-ec64-11ee-b297-3b0ad9d5d6c6",
  "39ba7438-d0d5-11ee-9435-f7e542e2436c",
  "ba28b352-ec8f-11ee-b297-3b0ad9d5d6c6",
  "cf6fdf3a-eaa3-11ee-b297-3b0ad9d5d6c6",
  "57d240d6-ea4d-11ee-b297-3b0ad9d5d6c6",
  "cb205756-ec43-11ee-b297-3b0ad9d5d6c6",
  "90101c36-a621-11ee-88ec-eb6a8d5269b4",
  "71a18322-ecab-11ee-b297-3b0ad9d5d6c6",
  "326699c2-ecd8-11ee-b297-3b0ad9d5d6c6",
  "75f83e28-eb77-11ee-b297-3b0ad9d5d6c6",
  "88180f82-ed4f-11ee-9385-ef789ffde1d3",
  "f6ac3c82-a445-11ee-88ec-eb6a8d5269b4",
  "c7c02bda-ebe0-11ee-b297-3b0ad9d5d6c6",
  "b224ef9c-ec10-11ee-b297-3b0ad9d5d6c6",
  "c335d84c-a45c-11ee-88ec-eb6a8d5269b4",
  "60546ef4-edaa-11ee-9385-ef789ffde1d3",
  "59c189d8-ed54-11ee-9385-ef789ffde1d3",
  "af10e22a-ebb1-11ee-b297-3b0ad9d5d6c6",
  "9830d896-d2dc-11ee-b437-336917683bb8",
  "5976b77a-a504-11ee-88ec-eb6a8d5269b4",
  "c0624e24-d9aa-11ee-a158-97f8443fd730",
  "8b6a6cfc-ed6d-11ee-9385-ef789ffde1d3",
  "e2079a78-dc1d-11ee-a158-97f8443fd730",
  "3a2a78cc-db21-11ee-a158-97f8443fd730",
  "feaf2ba8-d28d-11ee-b437-336917683bb8",
  "aa86a660-dc05-11ee-a158-97f8443fd730",
  "e6d7d384-db40-11ee-a158-97f8443fd730",
  "ed7f2038-ea1e-11ee-b297-3b0ad9d5d6c6",
  "04151804-ec20-11ee-b297-3b0ad9d5d6c6",
  "64bbe8e0-eb94-11ee-b297-3b0ad9d5d6c6",
  "64737d98-d312-11ee-b437-336917683bb8",
  "dc39aa14-db32-11ee-a158-97f8443fd730",
  "f671c05c-a5e4-11ee-88ec-eb6a8d5269b4",
  "d94ef300-ed60-11ee-9385-ef789ffde1d3",
  "20cbfe8c-ea2b-11ee-b297-3b0ad9d5d6c6",
  "58263e34-a45c-11ee-88ec-eb6a8d5269b4",
  "e9d67bf2-ec35-11ee-b297-3b0ad9d5d6c6",
  "f9d62032-db2a-11ee-a158-97f8443fd730",
  "82d39c74-ea59-11ee-b297-3b0ad9d5d6c6",
  "7228e03a-ebf0-11ee-b297-3b0ad9d5d6c6",
  "25d3bdc8-ecbc-11ee-b297-3b0ad9d5d6c6",
  "fc119dfc-eb67-11ee-b297-3b0ad9d5d6c6",
  "c9023e32-ed90-11ee-9385-ef789ffde1d3",
  "64875cc0-d054-11ee-9435-f7e542e2436c",
  "5fcc4fd8-ea71-11ee-b297-3b0ad9d5d6c6",
  "d1d090d4-ea7c-11ee-b297-3b0ad9d5d6c6",
  "559495ca-d270-11ee-b437-336917683bb8",
  "21376e38-ec01-11ee-b297-3b0ad9d5d6c6",
  "b76f33be-ea61-11ee-b297-3b0ad9d5d6c6",
  "3ea96640-ea37-11ee-b297-3b0ad9d5d6c6",
  "f0bcec4e-ed3e-11ee-9385-ef789ffde1d3",
  "17876fec-ea66-11ee-b297-3b0ad9d5d6c6",
  "e269948a-ed9d-11ee-9385-ef789ffde1d3",
  "a17c1280-ea10-11ee-b297-3b0ad9d5d6c6",
  "530de03a-ed79-11ee-9385-ef789ffde1d3",
  "68c289fa-dbd4-11ee-a158-97f8443fd730",
  "0f4f0a06-ea98-11ee-b297-3b0ad9d5d6c6",
  "7613801a-edcb-11ee-9385-ef789ffde1d3",
  "76683d3c-db18-11ee-a158-97f8443fd730",
  "80340ab8-d054-11ee-9435-f7e542e2436c",
  "6d62da08-ec9d-11ee-b297-3b0ad9d5d6c6",
  "7f09f6c6-a5b0-11ee-88ec-eb6a8d5269b4",
  "6af236d6-d98f-11ee-a158-97f8443fd730",
  "70060810-eb59-11ee-b297-3b0ad9d5d6c6",
  "3d2a80f0-ec81-11ee-b297-3b0ad9d5d6c6",
  "3343fd3c-eb87-11ee-b297-3b0ad9d5d6c6",
  "df8e3742-ec54-11ee-b297-3b0ad9d5d6c6",
  "286c70cc-d2f7-11ee-b437-336917683bb8",
  "e8a8b2be-edbf-11ee-9385-ef789ffde1d3",
  "baf0e4be-bede-11ee-835b-599066b5eb60",
  "513a670c-eea9-11ee-9385-ef789ffde1d3",
  "3441fc36-ecca-11ee-b297-3b0ad9d5d6c6",
  "2bc6ebb8-a529-11ee-88ec-eb6a8d5269b4",
  "154fab12-a43f-11ee-88ec-eb6a8d5269b4",
  "43914d48-ed85-11ee-9385-ef789ffde1d3",
  "ba6e1072-9524-11ee-956e-9da2d070324c",
  "4d0254fc-ec73-11ee-b297-3b0ad9d5d6c6",
  "2d35c522-eba2-11ee-b297-3b0ad9d5d6c6",
  "787d9684-d2c2-11ee-b437-336917683bb8",
  "3ce8a358-edd8-11ee-9385-ef789ffde1d3",
  "06cbdbc0-db4d-11ee-a158-97f8443fd730",
  "8c57e8ac-dbec-11ee-a158-97f8443fd730",
  "c8f54ac0-ebd2-11ee-b297-3b0ad9d5d6c6",
  "acd71bc0-ecf4-11ee-9385-ef789ffde1d3",
  "69ab88ec-dc17-11ee-a158-97f8443fd730",
  "19b7ebd0-d9b7-11ee-a158-97f8443fd730",
  "5240e750-ec30-11ee-b297-3b0ad9d5d6c6",
  "a7c98b32-ebc2-11ee-b297-3b0ad9d5d6c6",
  "721a9830-ece6-11ee-b297-3b0ad9d5d6c6",
  "47561998-d9c3-11ee-a158-97f8443fd730",
  "04115e66-ea91-11ee-b297-3b0ad9d5d6c6",
  "aef91c4a-ede5-11ee-9385-ef789ffde1d3",
  "a253145a-d2a6-11ee-b437-336917683bb8",
  "36663b02-ea87-11ee-b297-3b0ad9d5d6c6",
  "5f6573ba-ed2f-11ee-9385-ef789ffde1d3",
  "d62ee6e8-ed02-11ee-9385-ef789ffde1d3",
  "da853e0c-a10f-11ee-981c-d126ddbe9afa"
)


# Function to determine the route based on gmID
get_route <- function(gmID) {
  if (gmID %in% red_route_gmID_list) {
    return("Red")
  } else if (gmID %in% green_route_gmID_list) {
    return("Green")
  } else if (gmID %in% blue_route_gmID_list) {
    return("Blue")
  } else {
    stop(paste(gmID, "is not valid"))
  }
}

# Function to list whitelisted gmIDs
list_whitelisted_gmIDs <- function() {
  # List of all gmIDs
  gmIDs_set <- unique(list_gmIDs())
  
  # Blacklisted gmIDs set
  blacklisted_gmIDs_set <- c(
    '879e3fa2-f085-11ee-b9bd-fb353e7798cd', '900701bc-f0a6-11ee-b9e4-fb353e7798cd', '6f5b3612-f18d-11ee-bab8-fb353e7798cd', '471890c0-f0b9-11ee-b9f5-fb353e7798cd', '3b6d2a5c-f0c2-11ee-ba01-fb353e7798cd', 'a2ed8b42-f089-11ee-b9c3-fb353e7798cd', '787f70da-f036-11ee-b966-fb353e7798cd', '6458c26e-eab9-11ee-b297-3b0ad9d5d6c6', '00ff88b6-f0a3-11ee-b9e3-fb353e7798cd', '7d27535e-f0e6-11ee-ba21-fb353e7798cd', '23765aa8-eaf1-11ee-b297-3b0ad9d5d6c6', 'f93290be-eafe-11ee-b297-3b0ad9d5d6c6', '54a02cb8-eb4f-11ee-b297-3b0ad9d5d6c6', '2f2939cc-f228-11ee-bb28-fb353e7798cd', '48021fe0-f05c-11ee-b992-fb353e7798cd', '2837eb9c-9542-11ee-956e-9da2d070324c', '593d4b54-d0a9-11ee-9435-f7e542e2436c', 'd54c11ca-eae5-11ee-b297-3b0ad9d5d6c6', '6f887868-ed21-11ee-9385-ef789ffde1d3', '45ad3a9a-edb4-11ee-9385-ef789ffde1d3', '4ed017ee-ef05-11ee-9385-ef789ffde1d3', '81a5a96e-f0c6-11ee-ba06-fb353e7798cd', '05d0240e-eadb-11ee-b297-3b0ad9d5d6c6', '6c415180-f0bd-11ee-b9fa-fb353e7798cd', '662741a4-f38a-11ee-bb4e-fb353e7798cd', '60c57e4e-eb4a-11ee-b297-3b0ad9d5d6c6', 'cd11fc28-f21e-11ee-bb1c-fb353e7798cd', 'fa852f30-f210-11ee-bb10-fb353e7798cd', '3950298e-f1b4-11ee-bad3-fb353e7798cd', '2c8690b4-f09f-11ee-b9de-fb353e7798cd', '26af7004-f07a-11ee-b9b2-fb353e7798cd', '6c5f7416-f096-11ee-b9d4-fb353e7798cd', '38ac9526-f182-11ee-bab0-fb353e7798cd', '81acf35c-eac4-11ee-b297-3b0ad9d5d6c6', '86841630-d9d0-11ee-a158-97f8443fd730', '781c0bcc-eb21-11ee-b297-3b0ad9d5d6c6', '2bb03aaa-f0c7-11ee-ba06-fb353e7798cd', '906d3c4e-f0be-11ee-b9fb-fb353e7798cd', '878e1a02-f092-11ee-b9cf-fb353e7798cd', '606347dc-ed12-11ee-9385-ef789ffde1d3', 'cccc7d32-f1c0-11ee-bada-fb353e7798cd', '6daff50c-f041-11ee-b972-fb353e7798cd', 'ba80ba8c-f0ce-11ee-ba0d-fb353e7798cd', 'bf518644-f1a6-11ee-bac9-fb353e7798cd', '3a116996-93a9-11ee-956e-9da2d070324c', '56b8baa4-f0b5-11ee-b9f0-fb353e7798cd', 'bb52690a-f066-11ee-b99e-fb353e7798cd', 'a437811e-ccf4-11ee-9435-f7e542e2436c', '7aa336e6-f0b1-11ee-b9ed-fb353e7798cd', '986a0b90-f215-11ee-bb15-fb353e7798cd', 'ef63db62-f051-11ee-b986-fb353e7798cd', 'd1d69a76-f0b5-11ee-b9f0-fb353e7798cd', '57a2192c-f21a-11ee-bb17-fb353e7798cd', 'e9e14d3a-eb17-11ee-b297-3b0ad9d5d6c6', '0b72a836-f37e-11ee-bb4e-fb353e7798cd', '8be24d52-f0ca-11ee-ba0a-fb353e7798cd', 'd4b936f6-eb36-11ee-b297-3b0ad9d5d6c6', '599673dc-f070-11ee-b9a9-fb353e7798cd', 'cc0299e6-eb3e-11ee-b297-3b0ad9d5d6c6', 'b6227b56-f08d-11ee-b9c9-fb353e7798cd', 'fe729430-f232-11ee-bb32-fb353e7798cd', 'eb22edf4-f0ab-11ee-b9e9-fb353e7798cd', 'fc1e1b6a-f1e6-11ee-baf6-fb353e7798cd', '61b4e416-f1da-11ee-bae8-fb353e7798cd', '31b48540-f09b-11ee-b9da-fb353e7798cd', '240ebe64-f0d3-11ee-ba14-fb353e7798cd', 'bfde2aec-f370-11ee-bb4e-fb353e7798cd', 'b2f58080-f223-11ee-bb20-fb353e7798cd', '4bbe3c64-f088-11ee-b9c3-fb353e7798cd', 'b2bfb60c-f0ad-11ee-b9ea-fb353e7798cd', 'ede139be-f098-11ee-b9d8-fb353e7798cd', 'bcf44e58-eb0d-11ee-b297-3b0ad9d5d6c6', 'a6895b74-f1cd-11ee-bae2-fb353e7798cd', 'e640cf0a-f096-11ee-b9d5-fb353e7798cd', '1f70a4f0-f0e0-11ee-ba1e-fb353e7798cd', '41cd65b4-f0aa-11ee-b9e8-fb353e7798cd', '621c07b8-eaaf-11ee-b297-3b0ad9d5d6c6', 'dbff355c-f0a2-11ee-b9e3-fb353e7798cd', '23a7aa3e-f048-11ee-b97d-fb353e7798cd', '5c2ad8ec-f08c-11ee-b9c8-fb353e7798cd', '079f0d30-eb09-11ee-b297-3b0ad9d5d6c6', 'baec243e-eacf-11ee-b297-3b0ad9d5d6c6', '5230b9be-f083-11ee-b9b8-fb353e7798cd' 
    # Add all other blacklisted gmIDs here...
  )
  
  # Whitelist gmIDs by excluding blacklisted gmIDs
  whitelisted_gmIDs_set <- setdiff(gmIDs_set, blacklisted_gmIDs_set)
  
  return(whitelisted_gmIDs_set)
}


# Function to list blacklisted gmIDs
list_blacklisted_gmIDs <- function() {
  # List of all gmIDs
  gmIDs_set <- unique(list_gmIDs())
  
  # Predefined blacklisted gmIDs set
  blacklisted_gmIDs_set <- c(
    '879e3fa2-f085-11ee-b9bd-fb353e7798cd', '900701bc-f0a6-11ee-b9e4-fb353e7798cd', '6f5b3612-f18d-11ee-bab8-fb353e7798cd', '471890c0-f0b9-11ee-b9f5-fb353e7798cd', '3b6d2a5c-f0c2-11ee-ba01-fb353e7798cd', 'a2ed8b42-f089-11ee-b9c3-fb353e7798cd', '787f70da-f036-11ee-b966-fb353e7798cd', '6458c26e-eab9-11ee-b297-3b0ad9d5d6c6', '00ff88b6-f0a3-11ee-b9e3-fb353e7798cd', '7d27535e-f0e6-11ee-ba21-fb353e7798cd', '23765aa8-eaf1-11ee-b297-3b0ad9d5d6c6', 'f93290be-eafe-11ee-b297-3b0ad9d5d6c6', '54a02cb8-eb4f-11ee-b297-3b0ad9d5d6c6', '2f2939cc-f228-11ee-bb28-fb353e7798cd', '48021fe0-f05c-11ee-b992-fb353e7798cd', '2837eb9c-9542-11ee-956e-9da2d070324c', '593d4b54-d0a9-11ee-9435-f7e542e2436c', 'd54c11ca-eae5-11ee-b297-3b0ad9d5d6c6', '6f887868-ed21-11ee-9385-ef789ffde1d3', '45ad3a9a-edb4-11ee-9385-ef789ffde1d3', '4ed017ee-ef05-11ee-9385-ef789ffde1d3', '81a5a96e-f0c6-11ee-ba06-fb353e7798cd', '05d0240e-eadb-11ee-b297-3b0ad9d5d6c6', '6c415180-f0bd-11ee-b9fa-fb353e7798cd', '662741a4-f38a-11ee-bb4e-fb353e7798cd', '60c57e4e-eb4a-11ee-b297-3b0ad9d5d6c6', 'cd11fc28-f21e-11ee-bb1c-fb353e7798cd', 'fa852f30-f210-11ee-bb10-fb353e7798cd', '3950298e-f1b4-11ee-bad3-fb353e7798cd', '2c8690b4-f09f-11ee-b9de-fb353e7798cd', '26af7004-f07a-11ee-b9b2-fb353e7798cd', '6c5f7416-f096-11ee-b9d4-fb353e7798cd', '38ac9526-f182-11ee-bab0-fb353e7798cd', '81acf35c-eac4-11ee-b297-3b0ad9d5d6c6', '86841630-d9d0-11ee-a158-97f8443fd730', '781c0bcc-eb21-11ee-b297-3b0ad9d5d6c6', '2bb03aaa-f0c7-11ee-ba06-fb353e7798cd', '906d3c4e-f0be-11ee-b9fb-fb353e7798cd', '878e1a02-f092-11ee-b9cf-fb353e7798cd', '606347dc-ed12-11ee-9385-ef789ffde1d3', 'cccc7d32-f1c0-11ee-bada-fb353e7798cd', '6daff50c-f041-11ee-b972-fb353e7798cd', 'ba80ba8c-f0ce-11ee-ba0d-fb353e7798cd', 'bf518644-f1a6-11ee-bac9-fb353e7798cd', '3a116996-93a9-11ee-956e-9da2d070324c', '56b8baa4-f0b5-11ee-b9f0-fb353e7798cd', 'bb52690a-f066-11ee-b99e-fb353e7798cd', 'a437811e-ccf4-11ee-9435-f7e542e2436c', '7aa336e6-f0b1-11ee-b9ed-fb353e7798cd', '986a0b90-f215-11ee-bb15-fb353e7798cd', 'ef63db62-f051-11ee-b986-fb353e7798cd', 'd1d69a76-f0b5-11ee-b9f0-fb353e7798cd', '57a2192c-f21a-11ee-bb17-fb353e7798cd', 'e9e14d3a-eb17-11ee-b297-3b0ad9d5d6c6', '0b72a836-f37e-11ee-bb4e-fb353e7798cd', '8be24d52-f0ca-11ee-ba0a-fb353e7798cd', 'd4b936f6-eb36-11ee-b297-3b0ad9d5d6c6', '599673dc-f070-11ee-b9a9-fb353e7798cd', 'cc0299e6-eb3e-11ee-b297-3b0ad9d5d6c6', 'b6227b56-f08d-11ee-b9c9-fb353e7798cd', 'fe729430-f232-11ee-bb32-fb353e7798cd', 'eb22edf4-f0ab-11ee-b9e9-fb353e7798cd', 'fc1e1b6a-f1e6-11ee-baf6-fb353e7798cd', '61b4e416-f1da-11ee-bae8-fb353e7798cd', '31b48540-f09b-11ee-b9da-fb353e7798cd', '240ebe64-f0d3-11ee-ba14-fb353e7798cd', 'bfde2aec-f370-11ee-bb4e-fb353e7798cd', 'b2f58080-f223-11ee-bb20-fb353e7798cd', '4bbe3c64-f088-11ee-b9c3-fb353e7798cd', 'b2bfb60c-f0ad-11ee-b9ea-fb353e7798cd', 'ede139be-f098-11ee-b9d8-fb353e7798cd', 'bcf44e58-eb0d-11ee-b297-3b0ad9d5d6c6', 'a6895b74-f1cd-11ee-bae2-fb353e7798cd', 'e640cf0a-f096-11ee-b9d5-fb353e7798cd', '1f70a4f0-f0e0-11ee-ba1e-fb353e7798cd', '41cd65b4-f0aa-11ee-b9e8-fb353e7798cd', '621c07b8-eaaf-11ee-b297-3b0ad9d5d6c6', 'dbff355c-f0a2-11ee-b9e3-fb353e7798cd', '23a7aa3e-f048-11ee-b97d-fb353e7798cd', '5c2ad8ec-f08c-11ee-b9c8-fb353e7798cd', '079f0d30-eb09-11ee-b297-3b0ad9d5d6c6', 'baec243e-eacf-11ee-b297-3b0ad9d5d6c6', '5230b9be-f083-11ee-b9b8-fb353e7798cd'
    # Add all other blacklisted gmIDs here...
  )
  
  # Filter and return the intersected gmIDs
  blacklisted_gmIDs_set <- intersect(gmIDs_set, blacklisted_gmIDs_set)
  
  return(blacklisted_gmIDs_set)
}
