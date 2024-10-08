# import packages. 
if(!any(rownames(installed.packages()) == "pacman"))
  install.packages("pacman")

pacman::p_load(here,
               tidyverse,
               dplyr, 
               cowplot, 
               scales,
               corrplot,
               ggpubr
)



# import data
# import data for subgroup abundance
data_subgroup_abundance = read.csv("G:/My Drive/MyUCD/ZivkovicLab/Projects_n_Experiments/EM Projects/Manuscript_HDL sizing improvement method paper/Data analysis/DataRaw/All_subject_subgroup_abundance.csv")
data_subgroup_abundance = na.omit(data_subgroup_abundance)
data_subgroup_abundance$HDL_subgroup = factor(data_subgroup_abundance$HDL_subgroup, 
                                              levels = c("7.4 nm - 7.8 nm (H1P)", "7.8 nm - 8.7 nm (H2P)", "8.7 nm - 9.5 nm (H3P)", 
                                                         "9.5 nm - 10.3 nm (H4P)", "10.3 nm - 10.8 nm (H5P)", "10.8 nm - 12.0 nm (H6P)", 
                                                         "12.0 nm - 13.0 nm (H7P)"))
data_subgroup_abundance$group = factor(data_subgroup_abundance$group, 
                                       level = c("Control", "MCI", "AD"))
# import data for subgroup abundance for 1 decimal place. This is requested by Reviewers.
data_subgroup_abundance_1dp = read.csv("G:/My Drive/MyUCD/ZivkovicLab/Projects_n_Experiments/EM Projects/Manuscript_HDL sizing improvement method paper/Data analysis/DataRaw/All_subject_subgroup_abundance_1_decimal.csv")
data_subgroup_abundance_1dp = na.omit(data_subgroup_abundance_1dp)
data_subgroup_abundance_1dp$HDL_subgroup = factor(data_subgroup_abundance_1dp$HDL_subgroup, 
                                              levels = c("7.4 nm - 7.8 nm (H1P)", "7.8 nm - 8.7 nm (H2P)", "8.7 nm - 9.5 nm (H3P)", 
                                                         "9.5 nm - 10.3 nm (H4P)", "10.3 nm - 10.8 nm (H5P)", "10.8 nm - 12.0 nm (H6P)", 
                                                         "12.0 nm - 13.0 nm (H7P)"))
data_subgroup_abundance_1dp$group = factor(data_subgroup_abundance_1dp$group, 
                                       level = c("Control", "MCI", "AD"))
# # import data for subgroup abundance for 0 decimal place. This is requested by Reviewers.
# data_subgroup_abundance_0dp = read.csv("G:/My Drive/MyUCD/ZivkovicLab/Projects_n_Experiments/EM Projects/Manuscript_HDL sizing improvement method paper/Data analysis/DataRaw/All_subject_subgroup_abundance_0_decimal.csv")
# data_subgroup_abundance_0dp = na.omit(data_subgroup_abundance_0dp)
# data_subgroup_abundance_0dp$HDL_subgroup = factor(data_subgroup_abundance_0dp$HDL_subgroup, 
#                                                   levels = c("7.4 nm - 7.8 nm (H1P)", "7.8 nm - 8.7 nm (H2P)", "8.7 nm - 9.5 nm (H3P)", 
#                                                              "9.5 nm - 10.3 nm (H4P)", "10.3 nm - 10.8 nm (H5P)", "10.8 nm - 12.0 nm (H6P)", 
#                                                              "12.0 nm - 13.0 nm (H7P)"))
# data_subgroup_abundance_0dp$group = factor(data_subgroup_abundance_0dp$group, 
#                                            level = c("Control", "MCI", "AD"))





# statistical analysis

# A function to generate a table of statistical analysis. This can be used for 
# plotting significance as well. 
fnStatAnalysis_overall = function(data = data_subgroup_abundance, apoE = "")
{
    # filter data if a specific apoE genotype is indicated.
    if(apoE == "E3E3")
    {
        data = filter(data, apoe_genotype == "E3E3")
        print("analyzing data with E3E3 genotype only.")
    }
    else if(apoE == "E3E4")
    {
        data = filter(data, apoe_genotype == "E3E4")
        print("analyzing data with E3E4 genotype only.")
    }
    
    # Summarize the mean, SEM, and max of abundance percentage for each HDL subgroup 
    # stratified by diagnosis.
    data_summarized = data %>% 
        group_by(HDL_subgroup, group) %>% 
        summarize(Mean = mean(percent_abundance), 
                  SD = sd(percent_abundance)
                  )
    # construct the vectors needed for the result data frame.
    subgroup = c()
    group1 = c()
    mean1 = c()
    sem1 = c()
    max = c()
    group2 = c()
    mean2 = c()
    sem2 = c()
    pValue = c()
    # find all unique HDL subgroups.
    uniqueGroup = unique(data$HDL_subgroup)
    # populate the vectors with specific values.
    for(currentGroup in uniqueGroup)
    {
        # filter that specific data.
        temp = filter(data, HDL_subgroup == currentGroup)
        # update max
        max = c(max, rep(max(temp$percent_abundance), 4))
        # perform pairwise comparisons.
        pairwise_result = pairwise.t.test(temp$percent_abundance, 
                                          temp$group, 
                                          p.adjust.method = "BH")
        # fill in values for each data frame vector with the pairwise comparison results.
        for(i in 1:length(dimnames(pairwise_result$p.value)))
        {
            for(j in 1:length(dimnames(pairwise_result$p.value)[[i]]))
            {
                g1 = dimnames(pairwise_result$p.value)[[1]][j]
                group1 = c(group1, g1)
                mean1 = c(mean1, filter(data_summarized, 
                                        HDL_subgroup == currentGroup, 
                                        group == g1
                                        )$Mean
                          )
                sem1 = c(sem1, filter(data_summarized, 
                                      HDL_subgroup == currentGroup, 
                                      group == g1
                                      )$SD
                         )
                g2 = dimnames(pairwise_result$p.value)[[2]][i]
                group2 = c(group2, g2)
                mean2 = c(mean2, filter(data_summarized, 
                                        HDL_subgroup == currentGroup, 
                                        group == g2
                                        )$Mean
                          )
                sem2 = c(sem2, filter(data_summarized, 
                                      HDL_subgroup == currentGroup, 
                                      group == g2
                                      )$SD
                         )
                pValue = c(pValue, pairwise_result$p.value[2*i+j-2])
                subgroup = c(subgroup, currentGroup)
            }
        }
        # # show pairwise comparison results for each subgroup.
        # print(currentGroup)
        # print(pairwise_result$p.value)
        # show anova for each subgroup. 
        # result_anova <- aov(percent_abundance ~ group, data = temp)
        #print(summary(result_anova))
    }
    result = data.frame(subgroup = subgroup,
                        group1 = group1,
                        mean1 = mean1,
                        sem1 = sem1,
                        group2 = group2,
                        mean2 = mean2,
                        sem2 = sem2,
                        max = max,
                        pValue = pValue)
    return(result)
}



# call function
print(fnStatAnalysis_overall())
# call function with the 1 decimal place data. The Reviewer asked this.
testResult = fnStatAnalysis_overall(data_subgroup_abundance_1dp)
print(testResult)
# call function with the 1 decimal place data, on E3E3 genotype.
testResult = fnStatAnalysis_overall(data = data_subgroup_abundance_1dp, apoE="E3E3")
print(testResult)



# A function to modify the statistical analysis data frame to prepare it for plotting. 
fnGenerateTableForPlotting = function(stats_data = testResult)
{
    # Add significance annotations manually
    # Convert group1 and group2 to numeric for x- and y-axis positioning
    levels = c("Control", "MCI", "AD")
    subgroup_levels = c("7.4 nm - 7.8 nm (H1P)", "7.8 nm - 8.7 nm (H2P)", 
                        "8.7 nm - 9.5 nm (H3P)", "9.5 nm - 10.3 nm (H4P)", 
                        "10.3 nm - 10.8 nm (H5P)", "10.8 nm - 12.0 nm (H6P)", 
                        "12.0 nm - 13.0 nm (H7P)")
    stats_data <- stats_data %>%
        mutate(
            group1_num = match(group1, levels),
            group2_num = match(group2, levels),
            subgroup_num = match(subgroup, subgroup_levels),
            # Construct the position for the star between two groups. 
            # The position is 0.125 less or more from the whole number (subgroup_num)
            # The factor to account for this shift is calculated by logic: if mean of groups is 1.5, 
            # the factor is -0.125; if 2.0, factor is 0; if 2.5, factor is 0.125.
            factor = case_when(
                (group1_num + group2_num) / 2 == 1.5 ~ -0.125,
                (group1_num + group2_num) / 2 == 2.0 ~ 0,
                (group1_num + group2_num) / 2 == 2.5 ~ 0.125
            ),
            # the correct x_position of the star is calculated by subgroup number (1, 2, 3, ...) + 
            # the factor
            star_x_position = subgroup_num+factor,
            # Find the left and right bound of the lines.They are 0.125 less or 
            # more than the star position. When star_x_position is at the midpoint, 
            # group1 and group2 position need further calculations
            group1_x_position = ifelse(factor==0, star_x_position - 0.25, star_x_position - factor),
            group2_x_position = ifelse(factor==0, star_x_position + 0.25, star_x_position + factor),
            # y_position is calculated by the max plus a power of the group_number 
            # average (the higher group number, the further right the star will 
            # show on the figure), divide by 2, and plus 1 (raised by 1 to leave 
            # room for the lines)
            y_position = max + 1+ (1*(group1_num + group2_num)^2)/2
        )
    return(stats_data)
}



# plotting



# A function to plot Boxplots by HDL subgroups with the ability to draw 
# significance notations. 
fnPlotSubgroupAbundanceDifference = function(data = data_subgroup_abundance,
                                             apoE = "",
                                             filename="HDL_subgroup_abundance.png", 
                                             axisTitleSize = 16,
                                             axisTextSize = 12,
                                             height = 4.5,
                                             width = 4.5,
                                             legendX = 0.80,
                                             legendY = 0.80,
                                             figureTitle = "",
                                             unit = "in",
                                             dpi = 300)
{
    # Args: 
    #   data: should be the data_subgroup_abundance data frame that contains 
    #       1) abundance of each HDL subgroup, 2) the diagnostic group, 3) 
    #       the apoe genotype, and 4) other relevant characteristics.
    #   apoE: used to indicate whether the user wants to subset the data frame by
    #       a specific apoe genotype. 
    #   others are plotting parameters.
    
    # filter the data if needed. 
    if(apoE == "E3E3")
    {
        data = filter(data, apoe_genotype == "E3E3")
    }
    else if(apoE == "E3E4")
    {
        data = filter(data, apoe_genotype == "E3E4")
    }
    # plot.
    plot1 = ggplot(data = data, aes(x = HDL_subgroup, y = percent_abundance, 
                                    fill = group)) + 
        geom_boxplot(alpha = 0.5) + 
        labs(x = "HDL subgroups", 
             y = "HDL subgroup abundance (%)", 
             fill = "diagnosis\ngroup", 
             title = figureTitle) + 
        scale_x_discrete(labels = label_wrap_gen(width = 4), 
                         expand = expansion(mult = c(0, 0.2))) + 
        scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + 
        theme_bw()+
        theme(legend.position = c(legendX, legendY), 
              legend.box.background = element_rect(color = "black"), 
              axis.title = element_text(size = axisTitleSize), 
              text = element_text(size = axisTextSize, color = "black")
              )
    # Add significance annotations manually
    # First, get the test results for the data based on apoE passed by the function.
    testResults = fnStatAnalysis_overall(data, apoE)
    # Then, get the plot_ready of the testResults data frame.
    testResults_plot_ready = fnGenerateTableForPlotting(testResults)
    # Now loop through the filtered significant rows and annotate
    for (i in 1:nrow(testResults_plot_ready)) {
        # get each row
        row <- testResults_plot_ready %>% slice(i)
        stars = ""
        # if the p-value is true, move on to the next row.
        if(is.na(row$pValue)){next}
        # evaluate p-values
        if(row$pValue > 0.05)
        {
            next
        }
        else if(row$pValue > 0.01)
        {
            stars = "*"
        }
        else if(row$pValue > 0.005)
        {
            stars = "**"
        }
        else if(row$pValue > 0.001)
        {
            stars = "***"
        }
        else{
            stars = "****"
        }
        plot1 <- plot1 + 
            # plotting the stars
            annotate("text", 
                x = row$star_x_position,  # x-position calculated earlier.
                y = row$y_position,  # y-position calculated earlier.
                label = stars, 
                size = axisTextSize/3, 
                color = "black"
                ) + 
            # plotting the lines.
            annotate("segment", 
                     x = row$group1_x_position, y = row$y_position-0.5, 
                     xend = row$group2_x_position, yend = row$y_position-0.5
                     )
    }
    # # for working out the exact position the stars should be during testing. 
    # plot1 + 
    #     geom_vline(xintercept = seq(0, 8, by=1), color = "black", size = 1.2) + 
    #     geom_vline(xintercept = seq(0, 8, by=0.1), color = "red")
    
    print(plot1)
    # saving plot.
    file_path = "G:/My Drive/MyUCD/ZivkovicLab/Projects_n_Experiments/EM Projects/Manuscript_HDL sizing improvement method paper/Data analysis/Figures/"
    filepath = paste0(file_path, filename)
    ggsave(filepath, plot = plot1, height = height, width = width, unit = unit, dpi = dpi)
    return(testResults_plot_ready)
}



# # call the function to get figures.
# fnPlotSubgroupAbundanceDifference()
# get another figure for 1 decimal place. The Reviewer requested it.
a = fnPlotSubgroupAbundanceDifference(data = data_subgroup_abundance_1dp,
                                  filename = "HDL_subgroup_abundance_1dp.png", 
                                  legendX = 0.86)
# # get another figure for 0 decimal place. The reviewer requested it.
# fnPlotSubgroupAbundanceDifference(data = data_subgroup_abundance_0dp, 
#                                   filename = "HDL_subgroup_abundance_0dp.png")
# get another figure for 1 decimal place, for E3E3.
b = fnPlotSubgroupAbundanceDifference(data = data_subgroup_abundance_1dp, 
                                  filename = "HDL_subgroup_abundance_1dp_E3E3.png",
                                  apoE="E3E3", 
                                  legendX = 0.85, 
                                  legendY = 0.78, 
                                  figureTitle = "APOE E3E3")
# get another figure for 1 decimal place, for E3E4.
c = fnPlotSubgroupAbundanceDifference(data = data_subgroup_abundance_1dp, 
                                  filename = "HDL_subgroup_abundance_1dp_E3E4.png", 
                                  apoE="E3E4", 
                                  legendX = 0.85, 
                                  legendY = 0.75,
                                  figureTitle = "APOE E3E4")





