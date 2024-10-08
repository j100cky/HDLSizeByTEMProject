# import packages. 
if(!any(rownames(installed.packages()) == "pacman"))
  install.packages("pacman")

pacman::p_load(here,
               tidyverse,
               dplyr, 
               cowplot, 
               scales,
               corrplot
)

# organizing data from individual files.
# The following chunk was used to get data from yolov7 exports. It should have
# been done one time unless the YOLOv7 exports are changed for some reason (better model etc).
pixelRatio = 3.6



# # organize data from YOLOv7 exports.
# file_list = list.files("G:/My Drive/YOLOv7_exports/AD_project_with_image/", full.names = T)
# file_list_short = list.files("G:/My Drive/YOLOv7_exports/AD_project_with_image/", full.names = F)
# for(i in 1:length(file_list))
# {
#   data_path = list.files(path = paste0(file_list[i], "/labels"), pattern = "\\.txt", full.names = T)
#   # a shorter version for progress report
#   data_path_short = list.files(path = paste0(file_list[i], "/labels"), pattern = "\\.txt", full.names = F)
# 
#   # create a list of data frames.
#   data_list = lapply(seq_along(data_path), function(x) {
#     df = read.table(data_path[x], header = FALSE, sep = "")
#     df = df %>% mutate(image = x, particleID = V6, diameter = 2*(sqrt(V7/pi)/pixelRatio)) %>%
#       select(image, particleID, diameter)
#     df$image = as.factor(df$image)
#     colnames(df) = c("image", "particleID", "diameter")
#     print(paste0(file_list_short[i], ":", x,"/" ,length(data_path)))
#     return(df)
#   })
# 
#   # combine data into one data frame
#   combined_data <- bind_rows(data_list)
# 
#   # save the data frame for each subject.
#   saveFileName = paste0("G:/My Drive/MyUCD/ZivkovicLab/Projects_n_Experiments/",
#     "EM Projects/Manuscript_HDL sizing improvement method paper/Data analysis/",
#     "DataRaw/HDL_AD_project_yolov7_exports/", file_list_short[i], ".csv")
#   write.csv(x = combined_data, file = saveFileName)
#   print("data saved.")
# }
# 

# Find median particle diameter for each subject and combine them into one data frame.
# This should be done only once, too, unless the YOLOv7 exports are updated.
file_list = list.files(path = paste0("G:/My Drive/MyUCD/ZivkovicLab/Projects_n_Experiments/",
    "EM Projects/Manuscript_HDL sizing improvement method paper/Data analysis/",
    "DataRaw/HDL_AD_project_yolov7_exports/"), pattern = "\\.csv", full.names = T)


# obtain the allparticle data.
# loop through each csv file.
i = 1
data_allparticles = data.frame(subject = character(), diameter = numeric())
for(file in file_list)
{
  # read csv file
  data = read.csv(file)
  # clean up the data frame.
  # remove diameter < 7 nm or > 13
  data = data %>% filter(diameter > 7) %>%
    filter(diameter < 13)
  # extrace file name
  fileName = basename(file)
  fileName = unlist(strsplit(fileName, split = "_"))[1]
  # append to data_allparticles
  data_allparticles = rbind(data_allparticles, data.frame(subject = fileName, diameter = data$diameter))
  # report progress
  print(paste0(i, "/", length(file_list)))
  i=i+1
}



# obtain the median particle data.
# create the empty data frame.
result_df = data.frame(subject = character(), medianDiameter = numeric())
i = 1
for(file in file_list)
{
  # read csv file
  data = read.csv(file)
  # clean up the data frame.
  # remove diameter < 7 nm or > 15
  data = data %>% filter(diameter > 7) %>%
    filter(diameter < 13)
  # calculate median diameter.
  medianDiameter = median(data$diameter)
  # extrace file name
  fileName = basename(file)
  fileName = unlist(strsplit(fileName, split = "_"))[1]
  # add to the result_df
  result_df = rbind(result_df, data.frame(subject = fileName, medianDiameter = medianDiameter))
  print(paste0(i, "/", length(file_list)))
  i=i+1
}



# Get subject description data.
dataPath = "G:/My Drive/MyUCD/ZivkovicLab/Projects_n_Experiments/EM Projects/Manuscript_HDL sizing improvement method paper/Data analysis/DataRaw/"
data_subject = read.csv(paste0(dataPath, "AD_subject_desciption_PON1_updated.csv"))
data_subject$subject = as.character(data_subject$subject)
data_subject$group = factor(data_subject$group, levels = c("Control", "MCI", "AD"))
data_subject_selected = data_subject %>% select(subject, group, apoe_genotype, bmi, mean_index, mean_lcat, vmfin, executive, semantic, spatial, cdrsum, totalwmh, meanPON1)
# combine the median diameter with the subject description data.
data_combined = left_join(result_df, data_subject, by = "subject")
data_combined = data_combined %>%
  select(subject, medianDiameter, group, apoe_genotype)
data_combined <- na.omit(data_combined)
write.csv(x = data_combined, file = paste0(dataPath, "subject_HDL_median_diameter.csv"))


# combine the allparticle data with subject description data.
fnGetAllParticlesSubgrouping = function(data = data_allparticles ,digit = 2)
{
  # data is supposed to be data_allParticles
  data$subject = as.character(data$subject)
  data = left_join(data, data_subject_selected, by = "subject")
  data = na.omit(data)
  data$diameter = round(data$diameter, digit)
  data <- data %>%
    mutate(
      HDL_subgroup = case_when(
        between(diameter, 7.4, 7.8) ~ "7.4 nm - 7.8 nm (H1P)",
        between(diameter, 7.8, 8.7) ~ "7.8 nm - 8.7 nm (H2P)",
        between(diameter, 8.7, 9.5) ~ "8.7 nm - 9.5 nm (H3P)",
        between(diameter, 9.5, 10.3) ~ "9.5 nm - 10.3 nm (H4P)",
        between(diameter, 10.3, 10.8) ~ "10.3 nm - 10.8 nm (H5P)",
        between(diameter, 10.8, 12.0) ~ "10.8 nm - 12.0 nm (H6P)",
        between(diameter, 12.0, 13.0) ~ "12.0 nm - 13.0 nm (H7P)",
        TRUE ~ "Other"
      )
    )
  write.csv(x = data,
            file = "G:/My Drive/MyUCD/ZivkovicLab/Projects_n_Experiments/EM Projects/Manuscript_HDL sizing improvement method paper/Data analysis/DataRaw/All_subject_HDL_diameter.csv")
  
  return(data)
}



# create a dataset for HDL subgroup abundance.
data_allparticles_copy = fnGetAllParticlesSubgrouping(data_allparticles, 2)
data_allparticles_copy = data_allparticles_copy %>% group_by(subject, HDL_subgroup) %>%
  summarize(count = n()) %>%
  group_by(subject) %>%
  mutate(percent_abundance = count / sum(count) * 100)
data_allparticles_copy$HDL_subgroup = factor(data_allparticles_copy$HDL_subgroup,
  levels = c("7.4 nm - 7.8 nm (H1P)", "7.8 nm - 8.7 nm (H2P)", "8.7 nm - 9.5 nm (H3P)",
             "9.5 nm - 10.3 nm (H4P)", "10.3 nm - 10.8 nm (H5P)", "10.8 nm - 12.0 nm (H6P)",
             "12.0 nm - 13.0 nm (H7P)"))
data_allparticles_copy$subject = as.character(data_allparticles_copy$subject)
data_allparticles_copy = left_join(data_allparticles_copy, data_subject_selected, by = "subject")
# save the subgroup abundance dataset.
write.csv(x = data_allparticles_copy,
          file = "G:/My Drive/MyUCD/ZivkovicLab/Projects_n_Experiments/EM Projects/Manuscript_HDL sizing improvement method paper/Data analysis/DataRaw/All_subject_subgroup_abundance.csv")



# create a copy of dataset for HDL subgroup abundance with 1 decimal place.
data_1_decimal = fnGetAllParticlesSubgrouping(data_allparticles, 1)
data_1_decimal$diameter = round(data_1_decimal$diameter, 1)
data_1_decimal = data_1_decimal %>% group_by(subject, HDL_subgroup) %>%
  summarize(count = n()) %>%
  group_by(subject) %>%
  mutate(percent_abundance = count / sum(count) * 100)
data_1_decimal$HDL_subgroup = factor(data_1_decimal$HDL_subgroup,
                                              levels = c("7.4 nm - 7.8 nm (H1P)", "7.8 nm - 8.7 nm (H2P)", "8.7 nm - 9.5 nm (H3P)",
                                                         "9.5 nm - 10.3 nm (H4P)", "10.3 nm - 10.8 nm (H5P)", "10.8 nm - 12.0 nm (H6P)",
                                                         "12.0 nm - 13.0 nm (H7P)"))
data_1_decimal$subject = as.character(data_1_decimal$subject)
data_1_decimal = left_join(data_1_decimal, data_subject_selected, by = "subject")
# save the subgroup abundance dataset.
write.csv(x = data_1_decimal,
          file = "G:/My Drive/MyUCD/ZivkovicLab/Projects_n_Experiments/EM Projects/Manuscript_HDL sizing improvement method paper/Data analysis/DataRaw/All_subject_subgroup_abundance_1_decimal.csv")



# create a copy of dataset for HDL subgroup abundance with 0 decimal place.
data_0_decimal = fnGetAllParticlesSubgrouping(data_allparticles, 0)
data_0_decimal$diameter = round(data_0_decimal$diameter, 1)
data_0_decimal = data_0_decimal %>% group_by(subject, HDL_subgroup) %>%
    summarize(count = n()) %>%
    group_by(subject) %>%
    mutate(percent_abundance = count / sum(count) * 100)
data_0_decimal$HDL_subgroup = factor(data_0_decimal$HDL_subgroup,
                                     levels = c("7.4 nm - 7.8 nm (H1P)", "7.8 nm - 8.7 nm (H2P)", "8.7 nm - 9.5 nm (H3P)",
                                                "9.5 nm - 10.3 nm (H4P)", "10.3 nm - 10.8 nm (H5P)", "10.8 nm - 12.0 nm (H6P)",
                                                "12.0 nm - 13.0 nm (H7P)"))
data_0_decimal$subject = as.character(data_0_decimal$subject)
data_0_decimal = left_join(data_0_decimal, data_subject_selected, by = "subject")
# save the subgroup abundance dataset.
write.csv(x = data_0_decimal,
          file = "G:/My Drive/MyUCD/ZivkovicLab/Projects_n_Experiments/EM Projects/Manuscript_HDL sizing improvement method paper/Data analysis/DataRaw/All_subject_subgroup_abundance_0_decimal.csv")



# # make data frame for correlation plot.
# data_spread = spread(data_subgroup_abundance, key = HDL_subgroup, value = percent_abundance)
# data_spread = select(data_spread,-count, -"Other")
# # create a data frame with subjects.
# data = data.frame(subject = unique(data_spread$subject))
# for(i in 2:ncol(data_spread))
# {
#   print(i)
#   col_tmp = data_spread[,c(1,i)]
#   col_tmp = col_tmp[complete.cases(col_tmp),]
#   data = left_join(data, col_tmp, by = "subject") # join the two data frames together.
# }
# data[is.na(data)] <- 0 # set NAs to 0 to represent 0%.
# # join subject description into the new data frame.
# data$subject = as.character(data$subject) # change subject to characters.
# data_subject_selected = data_subject %>% # define selected variables from the subject description.
#   select(subject, group, apoe_genotype, bmi, mean_index, mean_lcat, vmfin, executive,
#          semantic, spatial, cdrsum, totalwmh, meanPON1)
# data = left_join(data, data_subject_selected, by = "subject") # joining subject description.
# # save data frame.
# write.csv(x = data,
#           file = "G:/My Drive/MyUCD/ZivkovicLab/Projects_n_Experiments/EM Projects/Manuscript_HDL sizing improvement method paper/Data analysis/DataRaw/HDL_subgroup_abundance_for_correloplot.csv")


# # Make RMSD matrices.
# data_allParticles = read.csv("G:/My Drive/MyUCD/ZivkovicLab/Projects_n_Experiments/EM Projects/Manuscript_HDL sizing improvement method paper/Data analysis/DataRaw/All_subject_HDL_diameter.csv",
#                              check.names = FALSE)
# 
# subjects_ = unique(data_allParticles$subject)
# nbins = 80
# increment = (15-7)/nbins
# A = matrix(NA, nrow = length(subjects_), ncol = nbins)
# rownames(A) = subjects_
# colnames(A) = c(seq(0.1, 8, by = 0.1))
# for(x in 1:length(subjects_)){
#   lower = 7
#   diameters = dplyr::filter(data_allParticles[,2:6], subject == subjects_[x])
#   for(y in 1:nbins){
#     upper = lower + increment
#     a_xy = sum(diameters > lower & diameters <= upper)
#     A[x,y] = a_xy
#     lower = lower + increment
#   }
#   print(subjects_[x])
# }
# # find abundance
# B = matrix(NA, nrow = length(subjects_), ncol = nbins)
# rownames(B) = subjects_
# for(i in 1:nrow(A))
# {
#   totalParticle = sum(A[i,])
#   B[i,] = A[i,]/totalParticle
#   print(i)
# }
# # calculate RMSD
# data_similarity = matrix(NA, ncol = length(subjects_), nrow = length(subjects_))
# rownames(data_similarity) = subjects_
# colnames(data_similarity) = subjects_
# for(n in 1:length(subjects_)){
#   for(m in 1:length(subjects_)){
#     sum_var = sum((B[n,] - B[m,])^2)
#     RMSD = sqrt(sum_var/nbins)
#     similarity = 1/RMSD
#     data_similarity[n,m] = similarity
#     print(n)
#   }
# }
# # saving similarity data.
# write.csv(x = data_similarity,
#           file = "G:/My Drive/MyUCD/ZivkovicLab/Projects_n_Experiments/EM Projects/Manuscript_HDL sizing improvement method paper/Data analysis/DataRaw/HDL_diameter_similarity_matrix.csv")










# find similarity data for the same sample from differnet images. 

# Specify the path to the folder containing CSV files
folder_path <- "G:/My Drive/MyUCD/ZivkovicLab/Projects_n_Experiments/EM Projects/Manuscript_HDL sizing improvement method paper/Data analysis/DataRaw/HDL_AD_project_yolov7_exports"

# Get a list of CSV files in the folder
csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
short_filenames = unlist(lapply(csv_files, function(x){unlist(strsplit(basename(x), "_"))[1]}))

# Loop through each CSV file and read it
for (k in 1:length(csv_files)) {
  csv_file = csv_files[k]
  
  # Read the CSV file (adjust parameters as needed)
  data <- read.csv(csv_file)
  # combine data from every 4 images into 1 since it was how they were cropped.
  data = data %>% mutate(image_original = rep(0, nrow(data)))
  data_original_image = data.frame()
  images_ = unique(data$image)
  
  # check if the sample has at least 2 original images. 
  if(length(images_) < 8) {next}
  
  for(i in 1:floor(length(images_)/4))
  {
    temp = filter(data, image > (i-1)*4 & image <= i*4)
    temp$image_original = i
    data_original_image = rbind(data_original_image, temp)
  }
  data = data_original_image

  # find number of particles fall between each increment.
  images_ = unique(data$image_original)
  nbins = 80
  increment = (15-7)/nbins
  A = matrix(NA, nrow = length(images_), ncol = nbins)
  rownames(A) = images_
  colnames(A) = c(seq(0.1, 8, by = 0.1))
  for(x in 1:length(images_)){
    lower = 7
    diameters = dplyr::filter(data, image_original == images_[x])
    for(y in 1:nbins){
      upper = lower + increment
      a_xy = sum(diameters > lower & diameters <= upper)
      A[x,y] = a_xy
      lower = lower + increment
    }
  }
  # find abundance
  B = matrix(NA, nrow = length(images_), ncol = nbins)
  rownames(B) = images_
  for(i in 1:nrow(A))
  {
    totalParticle = sum(A[i,])
    B[i,] = A[i,]/totalParticle
  }
  # calculate RMSD
  data_similarity = matrix(NA, ncol = length(images_), nrow = length(images_))
  rownames(data_similarity) = images_
  colnames(data_similarity) = images_
  for(n in 1:length(images_)){
    for(m in 1:length(images_)){
      sum_var = sum((B[n,] - B[m,])^2)
      RMSD = sqrt(sum_var/nbins)
      similarity = 1-RMSD
      data_similarity[n,m] = similarity
    }
  }
  
 
  # create file name
  save_path = "G:/My Drive/MyUCD/ZivkovicLab/Projects_n_Experiments/EM Projects/Manuscript_HDL sizing improvement method paper/Data analysis/DataRaw/Same_sample_similarity_matrices/"
  save_file_name = paste0(save_path, short_filenames[k], ".csv")
  # saving similarity data.
  write.csv(x = data_similarity,
            file = save_file_name)
  
  # print progress
  print(paste0(short_filenames[k], ":", 100*k/length(csv_files)))
}









# find similarity data for the psl beads from differnet images. 

# Specify the path to the folder containing CSV files
folder_path <- "G:/My Drive/MyUCD/ZivkovicLab/Projects_n_Experiments/EM Projects/Manuscript_HDL sizing improvement method paper/Data analysis/DataRaw/PS_beads/"
csv_files = list.files(folder_path, pattern = ".txt", full.names = T)

# read all yolov7-export txt files into the data_list list. 
img_size = 512 # used to convert yolov7-export from fraction to pixel number. 

data_list = lapply(seq_along(csv_files), function(x) {
  df = read.table(csv_files[x], header = FALSE, sep = "")
  df = df %>% mutate(image = x) %>% 
    select(image, V8)
  df$image = as.factor(df$image)
  colnames(df) = c("image", "diameter")
  return(df)
})
# create a big data frame from the list of yolov7-exports. 
scale = 2.33
data = bind_rows(data_list)
data$diameter = data$diameter/(3.6/scale) # adjust size by the difference in pixel ratio between the two types of images. 
data$image = as.numeric(data$image)

# combine data from every 16 images into 1 since it was how they were cropped.
data = data %>% mutate(image_original = rep(0, nrow(data)))
data_original_image = data.frame()
images_ = unique(data$image)

for(i in 1:floor(length(images_)/16))
{
  temp = filter(data, image > (i-1)*16 & image <= i*16)
  temp$image_original = i
  data_original_image = rbind(data_original_image, temp)
}
data = data_original_image

# find number of particles fall between each increment.
images_ = unique(data$image_original)
nbins = 80
increment = (15-7)/nbins
A = matrix(NA, nrow = length(images_), ncol = nbins)
rownames(A) = images_
colnames(A) = c(seq(0.1, 8, by = 0.1))
for(x in 1:length(images_)){
  lower = 7
  diameters = dplyr::filter(data, image_original == images_[x])
  for(y in 1:nbins){
    upper = lower + increment
    a_xy = sum(diameters > lower & diameters <= upper)
    A[x,y] = a_xy
    lower = lower + increment
  }
}
# find abundance
B = matrix(NA, nrow = length(images_), ncol = nbins)
rownames(B) = images_
for(i in 1:nrow(A))
{
  totalParticle = sum(A[i,])
  B[i,] = A[i,]/totalParticle
}

# calculate RMSD
data_similarity = matrix(NA, ncol = length(images_), nrow = length(images_))
rownames(data_similarity) = images_
colnames(data_similarity) = images_
for(n in 1:length(images_)){
  for(m in 1:length(images_)){
    sum_var = sum((B[n,] - B[m,])^2)
    RMSD = sqrt(sum_var/nbins)
    similarity = 1-RMSD
    data_similarity[n,m] = similarity
  }
}

# saving similarity data.
write.csv(x = data_similarity,
          file = "G:/My Drive/MyUCD/ZivkovicLab/Projects_n_Experiments/EM Projects/Manuscript_HDL sizing improvement method paper/Data analysis/DataRaw/PS_beads_similarity_index.csv")
