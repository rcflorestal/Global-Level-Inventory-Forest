##----------------------------------------------------------------------------##
##                          Forest Inventory Analysis                         ##
##----------------------------------------------------------------------------##
#'
#'@author Robson Cruz
#'
#'@description Global Forest Inventory Analysis
#'
#'@param plot, species names, scientific names, DBH, Basal Area (G), 
#'stem height (H), .....
#'
#'@return 
#'Statistic analysis of global forest inventory, 
#'plots and tables.


# Close all previously created figures
graphics.off()

# Load packages
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(hrbrthemes)
library(ggalt)
library(DT)
library(jpeg)
library(lattice)
library(plotly)
library(sjmisc)
library(leaflet)

#---------------------------Read data Sets-------------------------------------#
# Read The List of Endangered Species, according to the Ordinance of the       #
# Brazilian Ministry of Environment (Portaria MMA 443/2014)                    #
#------------------------------------------------------------------------------#
#
port <- read.csv2(
        'D:/Robson/home_office/Global-Level-Forest-Inventory/Data/attachment_443.csv', 
        header = TRUE, skip = 1
)

# Show Ordinance of the Brazilian Ministry of Environment 
as_tibble(port)

# Read the forest inventory sheet
FI <- read.csv2(
        'D:/Robson/home_office/Global-Level-Forest-Inventory/Data/IFpoa2.csv'
)

# Show variables of forest inventory
as_tibble(FI)

# Read Infrastructure Sheet
infra <- read.csv2(
  'D:/Robson/home_office/Global-Level-Forest-Inventory/Data/poa2_infra.csv',
                   header = TRUE)

# Show variables of infrastructure
as_tibble(infra)

#------------------------------Exploratory Data--------------------------------#
summary(FI[c(6:11)])  ## Summary Statics


## Summary by UT (plot)
FI_summ_ut <- FI %>%
        group_by(UT) %>%
        summarize(
                N = n(), DBH_mean = mean(DBH), DBH_med = median(DBH), 
                DBH_sd = sd(DBH), g_mean = mean(G), g_med = median(G), 
                g_sd = sd(G), H_mean = mean(H), H_med = median(H), 
                H_sd = sd(H), Vol_mean = mean(vol), Vol_med = median(vol), 
                Vol_sd = sd(vol)
        ) %>%
        mutate(UT = as.factor(UT))

head(FI_summ_ut, 14)

## Summary by specie
FI_summ_sp <- FI %>%
        group_by(Scientific_Name) %>%
        summarize(
        N = n(), 
        DBH_mean = mean(DBH), DBH_med = median(DBH), 
        DBH_sd = sd(DBH),
        g_mean = mean(G), g_med = median(G), g_sd = sd(G),
        H_mean = mean(H), H_med = median(H), H_sd = sd(H),
        Vol_mean = mean(vol), Vol_med = median(vol), Vol_sd = sd(vol)
        )

head(FI_summ_sp, 14)

## Box plot and violin plot
dbh_boxplot <- FI %>%  ## DBH Box plot
        ggplot(aes(x = as.factor(UT), y = DBH)) +
        geom_violin(aes(color = as.factor(UT), alpha = 0.8)) +
        scale_color_brewer(palette = 'Paired') +
        scale_fill_brewer(palette = 'Paired') +
        #stat_summary(fun = mean, geom = 'point', shape = 23, size = 2) +
        geom_boxplot(width = 0.3, outlier.size = -1) +
        scale_y_continuous(breaks = seq(40, 300, by = 30)) +
        #geom_text(data = IF_upa3_dap, aes(label = avg)) +
        labs(
        title = 'Distribution of DBH by Plot', 
        x = 'Plot', y = 'DBH (cm)'
        ) +
        theme(
        legend.position = 'none',
        plot.title = element_text(hjust = 0.5)
        ) 

ggplotly(dbh_boxplot)

height_boxplot <- FI %>%  ## Height Box plot
        ggplot(aes(x = as.factor(UT), y = H)) +
        geom_violin(aes(color = as.factor(UT), alpha = 0.8)) +
        scale_color_brewer(palette = 'Spectral') +
        scale_fill_brewer(palette = 'Spectral') +
        #stat_summary(fun = mean, geom = 'point', shape = 23, size = 2) +
        geom_boxplot(width = 0.2, outlier.size = -1) +
        #scale_y_continuous(breaks = seq(40, 350, by = 30)) +
        #geom_text(data = IF_upa3_dap, aes(label = avg)) +
        labs(
            title = 'Distribution of Height by Plot', 
            x = 'UT', y = 'Height (m)'
        ) +
        theme(
            legend.position = 'none',
            plot.title = element_text(hjust = 0.5)
        )

ggplotly(height_boxplot)

# Kernel density plot to view the distribution of DBH variable.
#
# Automatically defined window:
# plot(density(FI$DBH), # returns the density data
#              ylab = 'Density', 
#              main = 'Diameter at Breast Height - FLONA Caxiuanã UMF 2 UPA 2')

# Manually defined Windows (2.5, 4, e 6)
plot(density(
        FI$DBH, bw = 2.5, na.rm = TRUE), 
     ylab = 'Density', xlab = 'DBH (cm)', 
     main = 'Kernel Density Estimates of Diameter at Breast Height \n in FLONA Caxiuanã UMF 2 UPA 2', 
     col = 'red'
)
lines(density(FI$DBH, bw = 4), col = 'green')
lines(density(FI$DBH, bw = 6), col='blue')
legend(x = 250, y = 0.03, 
       legend = c('BW:', 'red: 2.5', 'green: 4.0', 'blue: 6.0'))

#dev.off()

###------------------------Horizontal Forest Structure-----------------------###
FI_Horiz <- FI %>%
	group_by(Scientific_Name) %>%
	count(Scientific_Name, UT, name = 'N') %>%
	spread(key = UT, value = N) %>%
	
	## Count the occurrence of each specie per UT (plot)
	row_count(count = NA) %>%
	mutate(n_UT = sum(abs(rowcount - length(unique(FI$UT))))) %>%
	
	## Set absolute density
	mutate(DA = n_UT / sum(infra$AEM)) %>%
	
	## Set absolute frequency
	mutate(FA = n_UT / 9) %>%
	select(-c(2:11))

n_sp <- FI %>% group_by(Scientific_Name) %>% summarise(n = n())

FI_Horiz <- FI_Horiz %>%
	left_join(n_sp, by = 'Scientific_Name') 

## Set relative density
FI_Horiz <- FI_Horiz %>%
	select(n, n_UT, DA, FA) %>%
	mutate(DR = DA / sum(FI_Horiz$DA) * 100) %>%
	
	## Set relative frequency
	mutate(FR = FA / sum(FI_Horiz$FA) * 100)


## Set basal area per specie
g_upa <- FI %>%
	group_by(Scientific_Name) %>%
	summarise(Gsp = sum(G))

FI_Horiz <- FI_Horiz %>%
	left_join(g_upa, by = 'Scientific_Name')

## Set total basal area
FI_G <- FI %>%
	select(Scientific_Name, G) %>%
	summarize(SumG = sum(G)) 

## Set Dominance (DO)
FI_Horiz <- FI_Horiz %>%
	mutate(DO = Gsp / sum(infra$AEM)) %>%
	mutate(DOR = (DO / (FI_G$SumG / sum(infra$AEM)) * 100)) %>%
	
	## Set the Coverage Value (VC) 
	mutate(VC = DR + DOR, VCR = VC /2) %>%
	
	## Set the Importance 'Value (VI)
	mutate(VI = FR + DR + DOR)


FI_Horiz <- FI_Horiz %>%
	mutate(VIR = (VI / sum(FI_Horiz$VI)) * 100)

head(FI_Horiz)

write.csv2(
        FI_Horiz,
        'D:/Robson/home_office/Global-Level-Forest-Inventory/output/Hoz_Structure.csv',
        row.names = FALSE
)

#-----------------Create Diameter at breast height Classes (DBH)---------------#
class <- vector()   ## Sets the class center vector
class2 <- vector()  ## Sets the class range vector

# Define the limits of DBH
classMin <- trunc(min(FI$DBH))     # Sets Minimum diameter
classMax <- round(max(FI$DBH),-2)  # Sets Maximum diameter
classAmp <- 10                     # Sets Class width
nClasses <- (classMax - classMin) / classAmp # Number of diameter classes

# Looping to DBH
d <- classMin 

for(i in 1:nClasses){
        d = d + classAmp   ## Upper Class Limits
        li = d - classAmp  ## Lower Class Limits
        sup = d
        classL = (li + sup) / 2
        class[which(FI$DBH >= li & FI$DBH < sup)] = paste(classL, sep="")
        class2[which(FI$DBH >= li & FI$DBH < sup)] = paste(li, "-", sup, sep="")
}

# Define a limit maximums of class
maxClass <- 150
class2[which(as.numeric(class) > maxClass)] = paste(">", maxClass, sep = "")
class[which(as.numeric(class) > maxClass)] = paste(">", maxClass, sep = "")

# Remove the object j created on looping  
rm(i)

# Convert vectors to factors
FI$class = as.factor(class)
FI$class2 = as.factor(class2)

rm(d, li, sup, class)

# Display levels of class
levels(FI$class)
levels(FI$class2)

# Sort Class Levels
# FI$class <- ordered(FI_port$class,
                      # levels=c("45", "55", "65", "75", "85", "95", "105", "115", 
                      #          "125",  "135",  "145",  "155", "165", "175", 
                      #          "185",  "195", ">200"))

FI$class2 <- ordered(FI$class2, 
                     levels = c("40-50", "50-60",  "60-70", "70-80", "80-90", 
                                "90-100", "100-110", "110-120", "120-130",
                                "130-140", "140-150", ">150"))  

# Plot Diameter Class Distribution for all tree species
FI %>% filter(DBH >= 50) %>%
        group_by(class2, DBH) %>%
        summarize(N = n()) %>%
        ggplot(aes(x = class2)) +
        geom_bar(
                stat = 'identity', aes(y = N), fill = 'steelblue'
        ) +
        scale_y_continuous(
                labels = scales::label_number(big.mark = '.')
        ) +
        theme(
                axis.text.x = element_text(angle = 90),
                plot.title = element_text(hjust = 0.5)
        ) +
        labs(
                title = 'Nº  of  Trees',
                x = 'Class of Diameter at Breast Height (DBH) in cm.',
                y = 'Nº de Árvores'
        )

# Save the FI with diameter class
write.csv2(
        FI, 
        file = 'D:/Robson/home_office/Global-Level-Forest-Inventory/output/IF_umf2_upa2_CLASS.csv', 
        na='NA',
        row.names = FALSE)

#--------------Join the FI with the list of endangered species-----------------#
FI_port <- FI %>%
        left_join(port, by = 'Scientific_Name') %>% ## endangered species
        left_join(infra, by = 'UT')                 ## infrastructure
        
        
# NA values replaced by "Nao Protegida"
FI_port$Status[which(is.na(FI_port$Status))] <- "Not Protected"

FI_port <- FI_port %>%
  select(-note)

FI_port <- FI_port %>%
        mutate(
                Status = recode(
                        Status, 'Vulneravel' = 'Vulnerable',
                        'Em Perigo' = 'Endangered'
                )
        )

as_tibble(FI_port)

# Save FI only endangered species      
write.csv2(
        FI_port, 
        file = 'D:/Robson/home_office/Global-Level-Forest-Inventory/output/if_endangered.csv', 
        na = 'NA',
        row.names = FALSE)

#-------------------------------List of species-------------------------------##
list_Species <- FI_port %>%
            select(Name, Scientific_Name, Status, Destination) %>%
            group_by(Scientific_Name) %>%
            mutate(Abate = sum(Destination == "Cut"),
                   Remanescentes = sum(Destination == "Remaining"),
                   Total = Abate + Remanescentes) %>%
            distinct(Scientific_Name, .keep_all = TRUE)

as_tibble(list_Species)

## Save the list of species
write.csv2(list_Species,
           "D:/Robson/home_office/Global-Level-Forest-Inventory/output/listSpecies.csv",
           row.names = TRUE
)

###---------------------------Use of Species---------------------------------###
species_use <- list_Species %>% 
  mutate(Use = (ifelse(Abate == 0, 'No Commercial', 'Commercial')))

FI_port <- FI_port %>%
        left_join(species_use[-c(1, 3, 4, 5, 6, 7)], by = 'Scientific_Name')

write.csv2(FI_port,
           "D:/Robson/home_office/Global-Level-Forest-Inventory/output/FI_complete.csv",
           row.names = FALSE
)

#---------------------------List of species to be cut-------------------------##
autex <- FI_port %>%
        select(Name, Scientific_Name, Status, Destination, vol) %>%
        filter(Destination == "Cut") %>%
        group_by(Scientific_Name) %>%
        mutate(count_cut = sum(Destination == "Cut"),
               vol_total = sum(vol)) %>%
        group_by(Scientific_Name, Destination) %>%
        ungroup() %>%
        distinct(Scientific_Name, .keep_all = TRUE) %>%
        arrange(Scientific_Name) %>%
        select(-c(3:5))

as_tibble(autex)

## Save the List of species to be cut
write.csv2(autex,
           "D:/Robson/home_office/Global-Level-Forest-Inventory/output/autex.csv",
           row.names = TRUE)

#-----------------------Total Volume and Basal Area----------------------------#
FI_vol <- FI_port %>%
  group_by(UT) %>%
  summarize(vol_sum = sum(vol),
            vol_avg = mean(vol),
            G_sum = sum(G),
            G_avg = mean(G))

#----------------------Cutting Volume and Basal Area---------------------------#
cutVol <- FI_port[which(FI_port$Destination == "Cut"), ]
tapply(cutVol$vol, cutVol$UT, mean)
sapply(split(cutVol$vol, cutVol$UT), sum)
sapply(split(cutVol$G, cutVol$UT), sum)

#----------------------------Basal Area by DBH---------------------------------#
G_DBH <- aggregate.data.frame(FI_port$G, list(FI_port$class2), sum) 
names_G <- c("DBH", "Basal_Area")
colnames(G_DBH) <- names_G
datatable(G_DBH, rownames = FALSE,
          options = list(pageLength = 12, autoWidth = FALSE),
          colnames = c("DBH (cm)", "Basal Area (m2)"))

#---------------------Filter Trees Suitable for Cutting------------------------#
FI_filter <- FI_port %>%
        filter(DBH >= 50 & QF <= 2)
as_tibble(FI_filter)  

#-------------------------Plot Basal Area by DBH-------------------------------#
FI_filter %>%
        select(class2, G) %>%
        filter(!is.na(class2)) %>%
        group_by(class2) %>%
        summarize(sum_G = sum(G)) %>%
        ggplot(aes(x = class2, 
                   y = sum_G,
                   fill = sum_G)) +
        geom_bar(stat = "identity",
                 position = position_dodge(0.8)) +
        theme(plot.title = element_text(color = "black",
                                        size = 14,
                                        face = "bold",
                                        hjust = 0.5),
              plot.subtitle = element_text(color = "black",
                                           hjust = 0.5),
              axis.title.x = element_text(color = "black",
                                          size = 13),
              axis.title.y = element_text(color = "black",
                                          size = 13),
              plot.caption = element_text(color = "blue", face = "italic"),
              legend.position = c(0.9, 0.7)) +
        labs(title = "Basal Area per DBH",
             subtitle = "UPA 2 UMF 2 FLONA Caxiuanã",
             x = "Diamter at Breast Height (DBH - cm)",
             y = "Basal Area (m²)",
             caption = "Source: Florest Inventory POA 2020 Benevides Madeiras LTDA.",
             fill = "Basal Area")
        

## Graphic Basal Area per Plot
FI_g <- FI_port %>%
        select(UT, G) %>%
        summarize(G_avg = sum(G) / 14)  ## Get total mean basal area

FI_port %>%
        select(UT, G) %>%
        group_by(UT) %>%
        summarize(sum_G = sum(G)) %>%
        ggplot(aes(x = as.factor(UT), y = sum_G, fill = sum_G > FI_g$G_avg)) +
        geom_bar(position = "dodge",
                 stat = "identity") +
        geom_hline(aes(yintercept = FI_g$G_avg, color = '628,94m²')) +
        scale_fill_manual(
          name = '', 
          values = c("#778899", "#4682B4"),
          labels = c('TRUE' = 'Above Average', 'FALSE' = 'Below Average')
        ) +
        theme(
          plot.title = element_text(
            color = "black", size = 14, face = "bold", hjust = 0.5
          ), legend.position = "bottom"
        ) +
        labs(
          title = "Basal Area per Plot", 
          y = "Basal Area (m²)", 
          x = "Plot", 
          color = 'Average Basal Area'
        )
    
#-----------------------Plot Select cut trees per DBH-------------------------##
### All Commercial Species per DBH ###
destAll <- FI_filter %>%
        filter(Use == 'Commercial') %>%
        select(Scientific_Name, DBH, Destination, class2) %>%
        filter(!is.na(class2)) %>%
        group_by(class2, Destination) %>%
        summarize(N = n())

total <- FI_filter %>%
        filter(Use == 'Commercial') %>%
        count(class2, name = 'N') %>%
        mutate(Destination = 'Total')

sel <- bind_rows(destAll, total)
#sel <- transform(sel, Destination = reorder(Destination, -N))

sel %>% 
        ggplot(aes(x = class2, y = N, group = Destination, color = Destination)) +
        # geom_bar(stat = 'identity', position = 'dodge', aes(fill = Destination)) +
        #geom_line() +
        geom_smooth(se = F, method = NULL, span = 2) +
        # geom_smooth(se = F, method = 'gam') +
        theme(
                plot.title = element_text(color = "black", 
                                          size = 14, 
                                          face = "bold", 
                                          hjust = 0.5),
                axis.text.x = element_text(angle = 50),
                axis.title.x = element_text(size = 13),
                axis.title.y = element_text(size = 13)
        ) +
        labs(
                title = "Trees Select for Cutting in UPA 2 UMF 2 FLONA Caxiuanã", 
                x = 'DAP (cm)', 
                y = 'Nº de Árvores'
        ) +
        theme(legend.position = c(0.9, 0.7))
sel

## species that are without remaining trees by DBH classes
dest <- FI_filter %>%
        filter(Use == 'Commercial') %>%
        filter(!is.na(class2)) %>%
        group_by(Scientific_Name, class2, Destination) %>%
        summarize(N = n()) %>%
        spread(key = Destination, value = N, fill = 0) %>%
        mutate(Total = Cut + Remaining)
## Filter all species that are without remaining trees by DBH classes
rem <- dest %>%
        filter(Remaining == 0) %>%
        select(Scientific_Name)

## Plot the selection of cut by DBH classes
specie_plot <- dest %>%
        group_by(Scientific_Name, class2, Cut, Remaining, Total) %>%
  	filter(Scientific_Name %in% rem$Scientific_Name) %>%
  	pivot_longer(cols = c(3:5), values_to = 'N') %>%
	ggplot(aes(x = class2, y = N, group = name)) +
	geom_bar(stat = 'identity', position = 'dodge', aes(fill = name)) +
	facet_wrap(~Scientific_Name, scales = 'free_y') +
	theme(
	        plot.title = element_text(color = "black", size = 13, 
	                                  face = "bold", hjust = 0.5), 
	        plot.subtitle = element_text(color = "black", hjust = 0.5), 
	        axis.text.x = element_text(angle = 90, size = 9), 
	        axis.title.x = element_text(color = "black", size = 9), 
	        axis.title.y = element_text(color = "black", size = 12),
	        plot.caption = element_text(color = "blue", face = "italic"), 
	        strip.text = element_text(face = 'italic'), 
	        legend.title = element_blank(), 
	        legend.position = 'top'
	) + 
        labs(
                title = "Species Without Remaining Trees in UPA 2 UMF 2 FLONA Caxiuanã", 
                x = 'DHB (cm)', y = 'Number of Trees', 
                caption = 'Source: Florest Inventory POA 2020 Benevides Madeiras LTDA.'
        )

specie_plot

#--------------------------Criterion 3 to 4 trees------------------------------#
crit_3.4 <- FI_filter %>%
        select(UT, Scientific_Name, Destination, Status, AEM) %>%
        group_by(UT, Scientific_Name) %>%
        mutate(Cut = sum(Destination == "Cut"), 
               Remaining = sum(Destination == "Remaining"),
               Total = Cut + Remaining,
               Crit = if_else(Status == "Nao Protegida", 
                              ceiling(3*AEM/100), 
                              ceiling(4*AEM/100)),
               Analysis = if_else(Remaining >= Crit | Cut == 0,
                                  "Atende", 
                                  "Nao Atende")) %>%
        distinct(AEM, .keep_all = TRUE) %>%
        select(-c(AEM,Destination))
        
as_tibble(crit_3.4)

write.csv2(
        crit_3.4, 
        "D:/Robson/home_office/Global-Level-Forest-Inventory/output/crit_3_4_trees.csv",
        row.names = FALSE
)

datatable(head(crit_3.4), 
          filter = 'top', options = list(pageLength = 10, 
                                         autoWidth = TRUE),
          callback = JS('table.page("next").draw(false);'))

  
#--------------------------Criterion 10 to 15 percent--------------------------#
crit_10.15 <- FI_filter %>%
        select(Scientific_Name, Destination, Status, AEM) %>%
        group_by(Scientific_Name) %>%
        mutate(Cut = sum(Destination == "Cut"), 
               Remaining = sum(Destination == "Remaining"),
               Total = Cut + Remaining,
               PercRem = round(Remaining / Total * 100),
               Crit = if_else(Status == "Nao Protegida", (10), (15)),
               Analysis = if_else(PercRem >= Crit | Cut == 0,
                            "Ok", 
                            "Not")) %>%
        distinct(Scientific_Name, .keep_all = TRUE) %>%
        select(-c(AEM,Destination)) %>%
        filter(Cut != 0)

as_tibble(crit_10.15)

write.csv2(crit_10.15, 
           "D:/Robson/home_office/Global-Level-Forest-Inventory/output/crit_10_15_trees.csv",
           row.names = FALSE)


## Table 10 - 15 %
datatable(head(crit_10.15), class = 'cell-border stripe')

## Chart 10 - 15%
## Lollipop Chart 3
crit_10.15 %>%
        filter(PercRem < 100) %>%
        ggplot(aes(x = PercRem, y = Scientific_Name, color = Analysis)) +
        geom_point(size = 3) +
        geom_segment(aes(x = Crit, xend = PercRem,
                         y = Scientific_Name, 
                         yend = Scientific_Name), color = 'steelblue') +
        geom_text(color = 'purple', size = 3, hjust = -0.25,
                  aes(label = paste0(PercRem, '%'))) +
        scale_x_continuous(breaks =  seq(10, 100, 5)) +
        labs(
                x = '', 
                y = NULL, 
                title = 'Percentage of Raiminig Trees',
                subtitle = 'POA 2020 - Haverst 2020-2021 - UMF 2 FLONA Caxiuanã',
                caption = 'Source: Florest Inventory of POA 2020 Benevides Madeiras LTDA.'
        ) +
        theme(
                plot.title = element_text(
                        size = 13, hjust = 0.5, face = "bold"),
                plot.subtitle = element_text(size = 11, hjust = 0.5),
                plot.caption = element_text(color = "blue", face = "italic"),
                plot.background = element_rect(fill = "#f7f7f7"), 
                panel.background = element_rect(fill = "#D8D8D8"), #f7f7f7
                panel.grid.minor = element_blank(),
                #panel.grid.major.y = element_blank(),
                panel.grid.major.x = element_line(),
                axis.ticks.x = element_blank(),
                axis.text.x = element_blank(),
                legend.position = "top",
                panel.border = element_blank()
        )

## Plot vol ~ UT and Status
FI_port %>% 
        select(UT, vol, Status) %>%
        group_by(UT, vol) %>%
        ggplot() +
        geom_jitter(aes(x = UT, y = vol, color = vol)) +
        facet_grid(cols = vars(Status)) +
        theme(
                plot.title = element_text(hjust = 0.5),
                strip.text = element_text(face = 'italic'),
                legend.position = 'bottom'
        ) +
        labs(
                title = 'Volume as a function of plot and Ecological Status',
                x = 'Plot',
                y = 'Volume (m³)'
        ) 

## Density Plot by Stem Quality and DBH
FI_port %>% 
        select(QF, class2) %>%
        mutate(
                QF = recode(
                        QF, '1' = 'Stem Quality 1', 
                        '2' = 'Stem Quality 2', 
                        '3' = 'Stem Quality 3'
                )
        ) %>%
        #group_by(QF, DBH) %>%
        ggplot(aes(x = class2, color = QF)) +
        geom_density() +
        facet_grid(cols = vars(QF)) +
        theme(
                plot.title = element_text(hjust = 0.5),
                axis.text.x = element_text(angle = 90),
                strip.text = element_text(face = 'italic'),
                legend.position = 'none'
        ) +
        labs(
                title = 'Density Plot by Stem Quality and DBH',
                x = 'Plot',
                y = 'DBH (cm)'
        ) 

## Density Plot by Stem Quality and Height
FI_port %>% 
        select(QF, H) %>%
        mutate(
                QF = recode(
                        QF, '1' = 'Stem Quality 1', 
                        '2' = 'Stem Quality 2', 
                        '3' = 'Stem Quality 3'
                )
        ) %>%
        ggplot(aes(x = H, color = QF)) +
        geom_density() +
        facet_grid(cols = vars(QF)) +
        theme(
                plot.title = element_text(hjust = 0.5),
                strip.text = element_text(face = 'italic'),
                legend.position = 'none'
        ) +
        labs(
                title = 'Density Plot by Stem Quality and Height',
                x = 'Plot',
                y = 'Height (m)'
        ) 

## Map of plot
# map <- FI_filter %>%
#         filter(UT == 1) %>%
#         mutate(Status = str_replace(Status, "^Em Perigo", "Endangered")) %>%
#         mutate(Status = str_replace(Status, "^Nao Protegida", "Not Endangered")) %>%
#         mutate(Status = str_replace(Status, "^Vulneravel", "Vulnerable ")) %>%
#         ggplot() +
#         geom_point(
#                 aes(x = East, y = North,
#                     color = Destination,
#                     text = paste('Scientific_Name:', '<i>', Scientific_Name, 
#                                  '</i>','<br>',
#                                  'Volume(m3):', vol, '<br>',
#                                  'Status:',Status))
#                 ) +
#         theme(
#                 plot.title = element_text(hjust = 0.5),
#                 plot.caption = element_text(face = "italic", size = 7),
#                 axis.title.y = element_text(angle = 90)
#                 ) +
#         labs(
#                 title = "Selection of Trees in plot 1.",
#                 caption = "DATUM SIRGAS2000, MC-51, UTM Zone 22.") +
#         guides(y = guide_axis(angle = 90)) +
#         theme_bw()
# 
# ggplotly(map)

## Read inventory trees with geographical coordinates
trees <- read.csv2(
        'D:/Robson/home_office/Global-Level-Forest-Inventory/Data/arvs_upa2_umf2_cxn.csv'
)

cut <- trees %>%
        filter(Destination == 'Cut')

rem <- trees %>%
        filter(Destination == 'Remaining')

app <- rgdal::readOGR(
        'D:/Robson/home_office/Global-Level-Forest-Inventory/Data/app.shp',
        verbose = FALSE, use_iconv = TRUE, encoding = 'UTF-8'
)

ut <- rgdal::readOGR(
        'D:/Robson/home_office/Global-Level-Forest-Inventory/Data/ut_upa2_umf2_cxn.shp',
        verbose = FALSE, use_iconv = TRUE, encoding = 'UTF-8'
)

pal <- colorFactor(
        palette = c('red', 'green'),
        domain = trees$Destination
)

map <- leaflet(trees) %>%
        addTiles(group = 'OSM') %>%
        #setView(lat = -1.75404558, lng = -51.79793383, zoom = 10) %>%
        addProviderTiles('OpenTopoMap', group = 'Topo') %>%
        addProviderTiles('Esri.WorldImagery', group = 'Satellite') %>%
        fitBounds(
                lat1 = -1.73007302, lng1 = -51.77978728,
                lat2 = -1.76975667, lng2 = -51.81636691
        ) %>%
        # Load the plot polygons
        addPolygons(
                data = ut, stroke = TRUE, group = 'Plot',
                color = 'black', fillColor = 'transparent'
        ) %>%
        
        # Load river influence area 
        addPolygons(
                data = app, stroke = TRUE, color = 'green', group = 'River Area',
                fillColor = 'green',
                label = ~paste0(formatC(hectare, digits = 4), ' ha')
        ) %>%
        # Load de localization of the trees to cut down
        addCircles(
                data = cut, group = 'Harvest',
                color = ~pal(Destination),
                radius = 2, weight = 4,
                popup = ~paste0(
                        sep = ' ',
                        '<b>Plot: </b>', UT,'<br>',
                        '<b>Nº ', N, '<br>',
                        '<b>Scientific_name: </b>', 
                        '<i>',Scientific_Name,'</i>', 
                        '<br>', '<b>DBH: </b>', formatC(
                                DBH, digits = 3),
                        ' cm', '<br>',
                        '<b>Height: ', H, ' m', '<br>',
                        '<b>Vol: </b>', formatC(
                                vol, digits = 4),
                        ' m³', '<br>',
                        '<b>Destination: ', Destination
                )
        ) %>%
        # Load de localization of the trees that 
        # will not be cut down
        addCircles(
                data = rem, group = 'Remaining',
                color = ~pal(Destination),
                radius = 2, weight = 4,
                popup = ~paste0(
                        sep = ' ',
                        '<b>Plot: </b>', UT,'<br>',
                        '<b>Nº ', N, '<br>',
                        '<b>Scientific_name: </b>', 
                        '<i>',Scientific_Name,'</i>', 
                        '<br>', '<b>DBH: </b>', formatC(
                                DBH, digits = 3),
                        ' cm', '<br>',
                        '<b>Height: ', H, ' m', '<br>',
                        '<b>Vol: </b>', formatC(
                                vol, digits = 4),
                        ' m³', '<br>',
                        '<b>Destination: ', Destination
                )
        ) %>%
        
        # Set a map localization
        addMiniMap() %>%
        # Set legend of the trees
        addLegend("bottomright", pal = pal, values = ~Destination,
                  title = 'Trees',
                  #labFormat = labelFormat(prefix = ""),
                  opacity = 1
        ) %>%
        
        # Set map providers and allow the user to select them
        addLayersControl(
                baseGroups = c('OSM (default)', 'Topo',
                               'Satellite'), 
                overlayGroups = c('Harvest', 'Remaining', 
                                  'Plot', 'River Area'),
                options = layersControlOptions(collapsed = FALSE))

map
