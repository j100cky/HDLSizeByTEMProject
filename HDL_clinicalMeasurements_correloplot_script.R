# import packages. 
if(!any(rownames(installed.packages()) == "pacman"))
    install.packages("pacman")

pacman::p_load(here,
               tidyverse,
               dplyr, 
               cowplot, 
               scales,
               corrplot, 
               RColorBrewer
)



# important paths.
rawDataDirectory = "G:/My Drive/MyUCD/ZivkovicLab/Projects_n_Experiments/EM Projects/Manuscript_HDL sizing improvement method paper/Data analysis/DataRaw/"
processedDataDirectory = "G:/My Drive/MyUCD/ZivkovicLab/Projects_n_Experiments/EM Projects/Manuscript_HDL sizing improvement method paper/Data analysis/DataProcessed/"
plotDirectory = "G:/My Drive/MyUCD/ZivkovicLab/Projects_n_Experiments/EM Projects/Manuscript_HDL sizing improvement method paper/Data analysis/Figures/"



# import and reshape data.
data_corr = read.csv(paste0(processedDataDirectory, "subject_description_corr_plot_ready.csv"),
                     check.names = F, 
                     row.names = 1)
data_corr$subject = as.character(data_corr$subject)
# remove the subject column.
data_corr = data_corr[,2:ncol(data_corr)]
data_corr_e3 = filter(data_corr, apoe_genotype == "E3E3")
data_corr_e4 = filter(data_corr, apoe_genotype == "E3E4")
data_corr_e3 = select(data_corr_e3, 
                      c("7.4 nm - 7.8 nm (H1P)", "7.8 nm - 8.7 nm (H2P)", "8.7 nm - 9.5 nm (H3P)", 
                        "9.5 nm - 10.3 nm (H4P)", "10.3 nm - 10.8 nm (H5P)","10.8 nm - 12.0 nm (H6P)",
                        "12.0 nm - 13.0 nm (H7P)", "bmi", "mean_index", "mean_lcat", "meanPON1", 
                        "vmfin","executive", "semantic" , "spatial", "cdrsum", ,"totalwmh", ))
data_corr_e4 = select(data_corr_e4, 
                      c("7.4 nm - 7.8 nm (H1P)", "7.8 nm - 8.7 nm (H2P)", "8.7 nm - 9.5 nm (H3P)", 
                        "9.5 nm - 10.3 nm (H4P)", "10.3 nm - 10.8 nm (H5P)","10.8 nm - 12.0 nm (H6P)",
                        "12.0 nm - 13.0 nm (H7P)", "bmi", "mean_index", "mean_lcat", "meanPON1", 
                        "vmfin","executive", "semantic" , "spatial", "cdrsum", ,"totalwmh", ))



# functions
fn_plot_correlation = function(data, 
                               plotTitle = "Overall",
                               titleSize = 1,
                               axisTitleSize = 1,
                               axisTextSize = 0.8,
                               width = 6, 
                               height = 6, 
                               units = "in", 
                               res = 300)
{
    # create a correlation matrix from data frame. 
    corMatrix = cor(data, use = "complete.obs", method = "spearman")
    write.csv(corMatrix, 
              file = paste0(processedDataDirectory, "correlation_matrix_", plotTitle, ".csv"))
    # get p-value.
    p_values = cor.mtest(corMatrix, method = "spearman")$p
    p_values_vector <- as.vector(p_values)
    # Get adjust P_value from raw p values. 
    pAdj_vector <- p.adjust(p_values_vector, method = "BH")
    pAdj_matrix <- matrix(pAdj_vector, ncol = dim(p_values))
    # set the col and row names of the adj p.value matrix. 
    colnames(pAdj_matrix) = colnames(corMatrix)
    rownames(pAdj_matrix) = colnames(corMatrix)
    # generate some colors
    my_colors = brewer.pal(9, "Spectral")
    # Open device
    png(file = paste0(plotDirectory, "correlation_plot_", plotTitle, ".png"), 
        width = width, 
        height = height, 
        units = units, 
        res = res)
    # plotting
    corrplot(corMatrix, 
             method = "color", 
             p.mat = pAdj_matrix,
             sig.level = 0.05, 
             insig = "label_sig", 
             tl.col = "black", 
             tl.cex = axisTextSize,
             cl.cex = axisTextSize,
             cl.align.text = "l" ,
             cl.ratio = 0.2,
             pch.cex = 1.5,
             mar = c(0, 0, 3, 0),
             is.corr = F, 
             addCoef.col = NULL, 
             col = colorRampPalette(my_colors)(100), 
             title = plotTitle
    )
    # close the device.
    dev.off()
}



# generate correloplot for E3E3
fn_plot_correlation(data_corr_e3, 
                    plotTitle = "E3E3")
# generate correloplot for E3E4
fn_plot_correlation(data_corr_e4, 
                    plotTitle = "E3E4")


