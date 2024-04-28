#Workshop 10 - Data Visualistaion 3
install.packages('tidyverse')
install.packages('patchwork')
library(tidyverse)
library(RColorBrewer)
library(patchwork) 

#2.Summary Tables - seperating genus from species and renaming opis to opris

beetles <- read.csv("dung_beetles.csv") %>%
  rename_with(tolower, everything()) %>%
  select(!starts_with("X")) %>%
  rename_with( ~ gsub("opis", "opris", .x), matches("Copis")) %>%
  pivot_longer(matches("_"),
               values_to = "count",
               names_to = "spp") %>%
  separate_wider_delim("spp", "_",
                       names = c("genus",
                                 "species"))

#3. Group By/Summarise
#We can start to look at some broad patterns in the data. What about seasonality?
#Not only can we use ‘mutate’ to make calculations ‘horizontally’ across columns, but we can also make calculations ‘vertically’ by grouping rows together and making summary statistics for each group.
?group_by
beetles %>%
  group_by(month)
#If you run the first of these, ‘group_by’, the output looks almost the same, but this time the table has added an underlying flag to say that it can be grouped by month. # Groups:   month [11]

#It is when you add this to a summarize function that an operation such as ‘sum’ can be applied to those groups.
total_pcm <- beetles %>%
  group_by(month) %>%
  summarize(total = sum(count))

#Now try plotting that with ggplot as a bar plot, you will need to reorder the months so they're in proper order
total_pcm$month <- factor(total_pcm$month, 
                          levels=month.name) #using factor and levels, you can reorder the months 

ggplot(total_pcm, aes(x = month, y = total)) + 
  geom_col()
#There could be something there, but it’s not clear. Maybe we need to look at each genus separately to see if it’s dominated by one signal?

#3. Heatmaps
#A bar plot isn’t the best option to use here, so we’re going to make a simple heatmap. Heatmaps are an excellent way to look at the interaction of three variables - particularly if were looking at occurrence data like we are here.
#We are going to to do this using geom_raster

#group the beetles table by genus name as well as month. Plot all of these together using geom_raster
genus_mean_pcm <- beetles %>%
  group_by(genus, month) %>%
  summarize(mean_count = mean(count))

genus_mean_pcm$month <- factor(genus_mean_pcm$month,levels=month.name)

ggplot(genus_mean_pcm, aes(x = month,
                           y = genus,
                           fill = mean_count
)) + geom_raster() +
  theme(axis.text.x=element_text(angle=45,hjust=1))

#Setting scale limits
#OK, now we’re starting to get a good overview of the number of beetles per month but the signal is dominated by the January bonanza of ‘yvescambefortius’.
#To deal with this in a quick-and-dirty way, let’s just cut that scale off at ten. You’ll need to set up a scale for ‘fill’ with a top limit of 10. This will remove all values over ten.
#Tip:To make it look nice, set the missing values to the default color for ‘max’ (this is a light blue with the hex code: #56B1F7)
#limit your scales so your plot looks like this:
ggplot(genus_mean_pcm, aes(x = month,
                           y = genus,
                           fill = mean_count
)) + geom_raster() +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_fill_gradient(limits=c(0,10),na.value ="#56B1F7")

#4. Axes
#OK, let’s look at a new data set.
#https://ourworldindata.org/guinea-worm-path-eradication
#This comes from ‘our world in data’ and concerns the campaign to eliminate Guinea Worm, a devastating parasitic disease that was largely neglected by the western world until it became the primary focus of the Carter Center who have been working to eliminate it for the last four decades.
#Data is given for individual countries, as well as the worldwide total. Lets take a look at the totals first:
guinea_world <- read.csv("number-of-reported-guinea-worm-dracunculiasis-cases.csv") %>% 
  rename(cases = Reported.cases.of.Guinea.worm.disease,
         country = Entity) %>%
  rename_with(tolower) %>% 
  filter(country == "World")

#Plot this with both a geom_line and geom_point, so that it looks like this:
ggplot(guinea_world,aes(year,cases)) + 
  geom_line() + 
  geom_point() +
  ggtitle("Guinea worm worldwide cases 1986-2021")

#Log Scales
#Woohoo! this is going well. Guinea worm cases have plummeted to the point where they are… low? how low?
#This is a problem you’ll encounter a lot; a y-axis with extremely large ranges which cannot easily be shown on the same scale. One answer to this is to use a logarithmic scale - instead of scaling linearly, this means each point up the y axis will be a factor of the one before (e.g. 10, 100, 1000…).
#Look through the available y axis scales and replace the default scale so your plot looks like this:x
ggplot(guinea_world,aes(year,cases)) + 
  geom_line() + 
  geom_point() +
  scale_y_log10() +
  ggtitle("Guinea worm worldwide cases 1986-2021")  

#Limits
#OK, looking better, but that log scale seems to stop at 10; that could be confusing.
#Use the limits variable in your log scale to make your scale run from 1 to 1 million (note, log scales cannot show zero - as there is no defined logarithm of zero)
ggplot(guinea_world,aes(year,cases)) + 
  geom_line() + 
  geom_point() +
  scale_y_log10(limits=c(1,1e6)) +
  ggtitle("Guinea worm worldwide cases 1986-2021")

#Breaks
#ggplot has defaulted to using scientific notation for this log scale so it reads as, eg, 1e+03 instead of 1000. This is perfectly correct, but we can make this a little more “human readable” by setting ‘breaks’ for our scale: this tells ggplot where to put in an axis marker and, if we give it labels, what to call them.
#Take a look at the help page for your log scale and fix it so that the breaks are at 10, 1000, 1000000, and the labels contain thousand markers (“1,000,000”)
#your final plot should look like this:
ggplot(guinea_world,aes(year,cases)) + 
  geom_line() + 
  geom_point() +
  scale_y_log10(limits=c(1,1e6),
                breaks=c(10,100,1000,
                         10000,100000,1000000),
                labels=c("10","100","1,000",
                         "10,000","100,000","1,000,000")) +
  ggtitle("Guinea worm worldwide cases 1986-2021")

#5. Finding a Story to Tell
#Now let’s continue this exploratory analysis by looking at case counts in the countries themselves.
#Nb: I’m going to manually replace one of the longer country names with an abbreviation to make it easier to plot

guinea_countries <- read.csv("number-of-reported-guinea-worm-dracunculiasis-cases.csv") %>% 
  rename(cases = Reported.cases.of.Guinea.worm.disease,
         country = Entity) %>%
  rename_with(tolower) %>%
  mutate(country = gsub("Central African Republic",
                        "C.A.R.",
                        country)) %>% 
  filter(country != "World")

#We’re going to limit this to the countries that have any cases of guinea worm. I’ve pulled these out of the table for you and ordered them by total cases. Let’s use them to order a factor of the country names
endemic_countries <- c("Angola","C.A.R.","Kenya","Yemen",                   
                       "Pakistan","Chad","Cameroon","Senegal","Ethiopia",
                       "Mauritania","Cote d'Ivoire","Mali","Togo","India",
                       "Benin","Niger","Burkina Faso","Uganda","Ghana",
                       "Sudan","South Sudan","Nigeria")

guinea_countries <- guinea_countries %>% filter(country %in% endemic_countries)

guinea_countries$country <- factor(guinea_countries$country,levels=endemic_countries)

#Extra Credit:
#how would you get these countries from the table using dplyr and tidyr?

endemic_countries <- guinea_countries %>% 
  group_by(country) %>% 
  summarize(cases=sum(cases)) %>% 
  filter(cases>0) %>% 
  arrange(cases,desc=F) %>%
  pull(country)

#First plot these with a bar plot, using geom_col, coloured by country:
ggplot(guinea_countries,aes(year,cases,group=country,fill=country)) + 
  geom_col() + 
  ggtitle("Guinea worm worldwide cases 1986-2021")

#It looks like these case counts are dominated by a few countries, but it is hard to see with all those colours.
#Let’s look at just the high burden countries, those with more than 100,000 cases ever.
high_burden_countries <- c("Burkina Faso","Ghana","Niger","Nigeria",
                           "South Sudan","Sudan","Uganda")


