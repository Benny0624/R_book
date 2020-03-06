# Load data
movies2 <- read.csv("Section6-Homework-Data.csv")
head(movies2)

# Set proper column names
colnames(movies2) <- c("DayOfWeek", "Director", "Genre", "MovieTitle", 
                       "ReleaseData", "Studio", "AdjustedGrossMillions", 
                       "BudgetMillions", "GrossMillions", "IMDbRating", 
                       "MovieLensRating", "OverseasMillions", 
                       "OverseasPercentage", "ProfitMillions", 
                       "ProfitPercentage", "RuntimeMinutes", "USMillions", 
                       "GrossPercentageUS")

# Inspect data
head(movies2)
tail(movies2)
str(movies2)
summary(movies2)
# AdjustedGrossMillions, GrossMillions, OverseasMillions,  ProfitMillions are 
# read as factors!
# This appears to be due to the presence of commas in the numbers.

# Remove commas and convert to numeric values:
movies2$AdjustedGrossMillions <- as.numeric(gsub(pattern = ",", replacement = "", x = as.character(movies2$AdjustedGrossMillions)))
movies2$GrossMillions <- as.numeric(gsub(pattern = ",", replacement = "", x = as.character(movies2$GrossMillions)))
movies2$OverseasMillions <- as.numeric(gsub(pattern = ",", replacement = "", x = as.character(movies2$OverseasMillions)))
movies2$ProfitMillions <- as.numeric(gsub(pattern = ",", replacement = "", x = as.character(movies2$ProfitMillions)))
str(movies2)
summary(movies2)

# Subset desired genres and studios
moviesSub <- movies2[movies2$Genre %in% c("action", "adventure", "animation", "comedy", "drama"), ]
moviesSub <- moviesSub[moviesSub$Studio %in% c("Buena Vista Studios", "Fox", "Paramount Pictures", "Sony", "Universal", "WB"), ]
str(moviesSub)
summary(moviesSub)

# Check available fonts
windowsFonts()
# Comic Sans MS not available!

# Install additional fonts
install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device = "win")

# Make plot
library(ggplot2)
dev.new()
p <- ggplot(data = moviesSub, aes(x = Genre, y = GrossPercentageUS))
p + 
  geom_jitter(aes(size = BudgetMillions, colour = Studio)) + 
  geom_boxplot(alpha = 0.7, outlier.colour = NA) +
  
  xlab("Genre") +
  ylab("Gross % US") +
  ggtitle("Domestic Gross % by Genre") +
  labs(size = "Budget $M") +
  
  theme(text = element_text(family = "Comic Sans MS"),
        axis.title.x = element_text(colour = "Blue", size = 30),
        axis.title.y = element_text(colour = "Blue", size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        
        plot.title = element_text(colour = "Black",
                                  size = 40,
                                  hjust = 0.5))
sessionInfo()
# R version 3.4.4 (2018-03-15)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
# 
# Matrix products: default
# 
# attached base packages:
#     [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#     [1] extrafont_0.17 ggplot2_2.2.1 
# 
# loaded via a namespace (and not attached):
#     [1] Rcpp_0.12.15     digest_0.6.15    grid_3.4.4       plyr_1.8.4       Rttf2pt1_1.3.6   gtable_0.2.0     magrittr_1.5     scales_0.5.0    
# [9] pillar_1.2.1     stringi_1.1.6    rlang_0.2.0      reshape2_1.4.3   lazyeval_0.2.1   extrafontdb_1.0  labeling_0.3     tools_3.4.4     
# [17] stringr_1.3.0    munsell_0.4.3    compiler_3.4.4   colorspace_1.3-2 tibble_1.4.2
