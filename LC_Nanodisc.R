CombineLCData <- function(){
        # load relevant libraries
        library(tidyverse)
        
        # have user select data and set working directory to that folder
        directory <- getwd()
        setwd(directory)
        
        files <- list.files(pattern = ".TXT") # generate file list (only *.txt)
        filename <<- basename(getwd()) # get filename from dir name
        
        df <- data.frame() # create emtpy data frame to fill with LC data
        
        # interate through each data file
        df <- lapply(files, function(i){
                dat <- read_delim(i, skip = 76, delim = "\t") # read in data
                name <- unlist(gsub(pattern = ".TXT", '', i)) # split filename for labeling
                name <- strsplit(name, "_") %>% unlist()
                dat$Run <- head(name, 1) # add run name to df
                dat$Channel <- tail(name, 1) # add channel to df
                dat$Volume <- 50 * dat$`Time(min)` / 1000
                dat
        })
        
        df <- bind_rows(df) # combine rows of data into df
        
        df$`Step(sec)` <- NULL # remove superfluous column
        df$Sequence <- filename
        # rename columns
        names(df) <- c("Time", "Absorbance", "Run", "Channel", 
                       "Volume", "Sequence")
        write_csv(df, paste0(filename, ".csv")) # save file
}

PlotLCData <- function() {
        library(ggthemes)
        theme_set(theme_few())
        
        dat <- read_csv(paste0(filename, ".csv"))
        
        fig.1 <- ggplot(dat, aes(Volume, Absorbance, color = Run)) +
                geom_line(size = 1) +
                facet_grid(.~Channel) +
                labs(x = "Volume (mL)", y = "Absorbance (mAU)") +
                theme(legend.position="bottom")
        
        fig.1a <- fig.1 + xlim(0.5, 1.6)
        
        ggsave(fig.1, filename = paste0(filename, "_All.png"),
               width = 8, height = 6)
        ggsave(fig.1a, filename = paste0(filename, "_All_2.png"),
               width = 8, height = 6)
        
        dat.nostd <- filter(dat, !grepl("Std", Run))
        
        fig.2 <- ggplot(dat.nostd, aes(Volume, Absorbance, color = Run)) +
                geom_line(size = 1) +
                facet_grid(.~Channel) +
                labs(x = "Volume (mL)", y = "Absorbance (mAU)") +
                theme(legend.position="bottom")
        
        fig.2a <- fig.2 + xlim(0.75, 1.25)
        
        ggsave(fig.2, filename = paste0(filename, "_NoStd.png"),
               width = 8, height = 6)
        ggsave(fig.2a, filename = paste0(filename, "_NoStd_2.png"),
               width = 8, height = 6)
        
        # Monomer #1
        dat.mono1 <- filter(dat, grepl("Mono1|Empty", Run))
        
        fig.3 <- ggplot(dat.mono1, aes(Volume, Absorbance, color = Run)) +
                geom_line(size = 1) +
                facet_grid(.~Channel) +
                labs(x = "Volume (mL)", y = "Absorbance (mAU)") +
                theme(legend.position="bottom")
        
        fig.3a <- fig.3 + xlim(0.75, 1.25) + ylim(-1, 10)
        
        ggsave(fig.3, filename = paste0(filename, "_Mono1.png"),
               width = 8, height = 6)
        ggsave(fig.3a, filename = paste0(filename, "_Mono1_2.png"),
               width = 8, height = 6)
        
        # Monomer #2
        dat.mono2 <- filter(dat, grepl("Mono2|Empty", Run))
        
        fig.4 <- ggplot(dat.mono2, aes(Volume, Absorbance, color = Run)) +
                geom_line(size = 1) +
                facet_grid(.~Channel) +
                labs(x = "Volume (mL)", y = "Absorbance (mAU)") +
                theme(legend.position="bottom")
        
        fig.4a <- fig.4 + xlim(0.75, 1.25) + ylim(-1, 10)
        
        ggsave(fig.4, filename = paste0(filename, "_Mono2.png"),
               width = 8, height = 6)
        ggsave(fig.4a, filename = paste0(filename, "_Mono2_2.png"),
               width = 8, height = 6)
        
        
        dat.rams <- filter(dat, grepl("RAMS|Empty MSP1E3D1 F1", Run) & Channel == 2)
        
        fig.5 <- ggplot(dat.rams, aes(Volume, Absorbance, color = Run)) +
                geom_line(size = 1) +
                labs(x = "Volume (mL)", y = "Absorbance (mAU)") +
                theme(legend.position="bottom")
        
        fig.5a <- fig.5 + xlim(0.7, 1.7)
        
        ggsave(fig.5, filename = paste0(filename, "_Rams.png"),
               width = 8, height = 6)
        ggsave(fig.5a, filename = paste0(filename, "_Rams_2.png"),
               width = 8, height = 6)

}

Go <- function(){
        CombineLCData()
        PlotLCData()
}