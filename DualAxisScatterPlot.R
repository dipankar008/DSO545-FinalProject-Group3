
library(ggplot2)
library(dplyr) 
data= read.csv("WHR.csv")

# Changing Variable Names 

data$Country = as.character(data$Country)
data$Region = as.character(data$Region)
# data$Happiness.Rank=as.numeric(data$Happiness.Rank)

colnames(data)[colnames(data) == "Trust..Government.Corruption."] = "Trust"
colnames(data)[colnames(data) == "Economy..GDP.per.Capita."] = "Economy"
colnames(data)[colnames(data) == "Health..Life.Expectancy."] = "Health"

#Renameing three columns

colnames(data)

Print <- function(x)
{
  
if(x == "Trust"){
#########################  Trust ##################################################################

#select partial data 
trust <- data[,c(5,6,12)]   

#create two datasets and combine to a data frame
rank_score <- data.frame( x = data$Happiness.Score,
                          y = data$Happiness.Rank)
rank_trust <- data.frame (x = data$Trust,
                          y = data$Happiness.Rank)

rank_score$z  <- "Happyness Score"
rank_trust$z  <- "Trust"

rank_trust2 <- within(rank_trust, { x = x*10+2.8 })   #manipulating x axis
score_trust <- rbind(rank_score, rank_trust2)

category_colors <-  c("Happyness Score"="pink", "Trust"="darkblue")

ggplot(score_trust, 
       aes(x=x, 
           y= factor(desc(y)), 
           group=z, 
           color=z)
       ) +
  ggtitle("The Relationship between Trust in Government and Happiness\n")+
  labs(x="Happiness Score",
       y="Happiness Rank"
       )+
geom_point() +
scale_x_continuous(name="Happyness Score",limits = c(2.8,7.8), 
                   breaks = seq(2.5,7.5, by = 0.5),  
   sec.axis = sec_axis(~(.-2.8)/10,
                       breaks = seq(0,0.5,by = 0.05), 
                       name="Trust"
                       )
                    ) +
  scale_color_manual(name=" ", values = category_colors) +
  theme(
    axis.title.x =  element_text(color = "pink3"),
    axis.text.x = element_text(color = "pink3"),
    axis.title.x.top =  element_text(color = category_colors["Trust"]),
    axis.text.x.top = element_text(color = category_colors["Trust"]),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_line( color = "grey61", size = 1, linetype = "solid"), legend.position = "bottom"
  ) 
} else if(x == "Family"){

############################## Family ##################################################

#select partial data 
family <- data[,c(5,6,9)]   

#create two datasets and combine to a data frame
rank_score <- data.frame( x = data$Happiness.Score,
                          y = data$Happiness.Rank)
rank_family <- data.frame (x = data$Family,
                          y = data$Happiness.Rank)

rank_score$z  <- "Happyness Score"
rank_family$z  <- "Family"

rank_family2 <- within(rank_family, { x = x*(5/1.65)+2.8 })   #manipulating x axis according to X axis' range
score_family <- rbind(rank_score, rank_family2)

category_colors <-  c("Happyness Score"="pink", "Family"="darkgreen")

ggplot(score_family, 
       aes(x=x, 
           y= factor(desc(y)), 
           group=z, 
           color=z)
) +
  ggtitle("The Relationship between Family/Social Support and Happiness\n")+
  labs(x="Happiness Score",
       y="Happiness Rank"
  )+
  geom_point() +
  scale_x_continuous(name="Happyness Score",limits = c(2.8,7.8), 
                     breaks = seq(2.5,7.5, by = 0.5),  
                     sec.axis = sec_axis(~(.-2.8)*1.65/5,
                                         breaks = seq(0,1.65,by = 0.15), 
                                         name="Family/Social Support"
                     )
  ) +
  scale_color_manual(name=" ", values = category_colors) +
  theme(
    axis.title.x =  element_text(color = "pink3"),
    axis.text.x = element_text(color = "pink3"),
    axis.title.x.top =  element_text(color = category_colors["Family"]),
    axis.text.x.top = element_text(color = category_colors["Family"]),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_line( color = "grey61", size = 1, linetype = "solid"), legend.position = "bottom"
  )
} else if( x == "Economy"){

############################## Economy ##################################################
#select partial data 
Economy <- data[,c(5,6,8)]   

#create two datasets and combine to a data frame
rank_score <- data.frame( x = data$Happiness.Score,
                          y = data$Happiness.Rank)
rank_economy <- data.frame (x = data$Economy,
                          y = data$Happiness.Rank)

rank_score$z  <- "Happyness Score"
rank_economy$z  <- "GDP per Capita"

rank_economy2 <- within(rank_economy, { x = x*5/1.88+2.8 })   #manipulating x axis
score_economy <- rbind(rank_score, rank_economy2)

category_colors <-  c("Happyness Score"="pink", "GDP per Capita"="orange")

ggplot(score_economy, 
       aes(x=x, 
           y= factor(desc(y)), 
           group=z, 
           color=z)
) +
  ggtitle("The Relationship between Economic Development and Happiness\n")+
  labs(x="Happiness Score",
       y="Happiness Rank"
  )+
  geom_point() +
  scale_x_continuous(name="Happyness Score",limits = c(2.8,7.8), 
                     breaks = seq(2.5,7.5, by = 0.5),  
                     sec.axis = sec_axis(~(.-2.8)*1.88/5,
                                         breaks = seq(0,1.88,by = 0.15), 
                                         name="Economy - GDP per Capita"
                     )
  ) +
  scale_color_manual(name=" ", values = category_colors) +
  theme(
    axis.title.x =  element_text(color = "pink3"),
    axis.text.x = element_text(color = "pink3"),
    axis.title.x.top =  element_text(color = category_colors["GDP per Capita"]),
    axis.text.x.top = element_text(color = category_colors["GDP per Capita"]),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_line( color = "grey61", size = 1, linetype = "solid"), legend.position = "bottom"
  )
} else if(x == "Freedom") {


############################## Freedom ##################################################

#select partial data 
freedom <- data[,c(5,6,11)]   

#create two datasets and combine to a data frame
rank_score <- data.frame( x = data$Happiness.Score,
                          y = data$Happiness.Rank)
rank_freedom <- data.frame (x = data$Freedom,
                            y = data$Happiness.Rank)

rank_score$z  <- "Happyness Score"
rank_freedom$z  <- "Freedom"

rank_freedom2 <- within(rank_freedom, { x = x*5/0.67+2.8 })   #manipulating x axis
score_freedom <- rbind(rank_score, rank_freedom2)

category_colors <-  c("Happyness Score"="pink", "Freedom"="forestgreen")

ggplot(score_freedom, 
       aes(x=x, 
           y= factor(desc(y)), 
           group=z, 
           color=z)
) +
  ggtitle("The Relationship between Freedom and Happiness\n")+
  labs(x="Happiness Score",
       y="Happiness Rank"
  )+
  geom_point() +
  scale_x_continuous(name="Happyness Score",limits = c(2.8,7.8), 
                     breaks = seq(2.5,7.5, by = 0.5),  
                     sec.axis = sec_axis(~(.-2.8)*0.67/5,
                                         breaks = seq(0,0.67,by = 0.05), 
                                         name="Freedom"
                     )
  ) +
  scale_color_manual(name=" ", values = category_colors) +
  theme(
    axis.title.x =  element_text(color = "pink3"),
    axis.text.x = element_text(color = "pink3"),
    axis.title.x.top =  element_text(color = category_colors["Freedom"]),
    axis.text.x.top = element_text(color = category_colors["Freedom"]),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_line( color = "grey61", size = 1, linetype = "solid"), legend.position = "bottom"
  )

} else if(x == "Generosity"){

############################## Generosity ##################################################

##select partial data 
generosity <- data[,c(5,6,13)]   

#create two datasets and combine to a data frame
rank_score <- data.frame( x = data$Happiness.Score,
                          y = data$Happiness.Rank)
rank_generosity <- data.frame (x = data$Generosity,
                            y = data$Happiness.Rank)

rank_score$z  <- "Happyness Score"
rank_generosity$z  <- "Generosity"

rank_generosity2 <- within(rank_generosity, { x = x*5/0.84+2.8 })   #manipulating x axis
score_generosity <- rbind(rank_score, rank_generosity2)

category_colors <-  c("Happyness Score"="pink", "Generosity"="goldenrod3")

ggplot(score_generosity, 
       aes(x=x, 
           y= factor(desc(y)), 
           group=z, 
           color=z)
) +
  ggtitle("The Relationship between Generosity and Happiness\n")+
  labs(x="Happiness Score",
       y="Happiness Rank"
  )+
  geom_point() +
  scale_x_continuous(name="Happyness Score",limits = c(2.8,7.8), 
                     breaks = seq(2.5,7.5, by = 0.5),  
                     sec.axis = sec_axis(~(.-2.8)*0.84/5,
                                         breaks = seq(0,0.85,by = 0.1), 
                                         name="Generosity"
                     )
  ) +
  scale_color_manual(name=" ", values = category_colors) +
  theme(
    axis.title.x =  element_text(color = "pink3"),
    axis.text.x = element_text(color = "pink3"),
    axis.title.x.top =  element_text(color = category_colors["Generosity"]),
    axis.text.x.top = element_text(color = category_colors["Generosity"]),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_line( color = "grey61", size = 1, linetype = "solid"), legend.position = "bottom"
  )
} else {

############################## Health ##################################################

#select partial data 
health <- data[,c(5,6,10)]   

#create two datasets and combine to a data frame
rank_score <- data.frame( x = data$Happiness.Score,
                          y = data$Happiness.Rank)
rank_health <- data.frame (x = data$Health,
                            y = data$Happiness.Rank)

rank_score$z  <- "Happyness Score"
rank_health$z  <- "Health"

rank_health2 <- within(rank_health, { x = x*5+2.8 })   #manipulating x axis
score_health <- rbind(rank_score, rank_health2)

category_colors <-  c("Happyness Score"="pink", "Health"="darkslategray")


ggplot(score_health, 
       aes(x=x, 
           y= factor(desc(y)), 
           group=z, 
           color=z)
) +
  ggtitle("The Relationship between Health and Happiness\n")+
  labs(x="Happiness Score",
       y="Happiness Rank"
  )+
  geom_point() +
  scale_x_continuous(limits = c(2.8,7.8), 
                     breaks = seq(2.5,7.5, by = 0.5),
                     labels =  seq(2.5,7.5, by = 0.5),
                     sec.axis = sec_axis(~(.-2.8)/5,
                                         breaks = seq(0,1,by = 0.1), 
                                         labels = seq(0,1,by = 0.1),
                                         name="Health")
                     )+
  scale_color_manual(name=" ", values = category_colors) +
  theme(
    axis.title.x =  element_text(color = "pink3"),
    axis.text.x = element_text(color = "pink3"),
    axis.title.x.top =  element_text(color = category_colors["Health"]),
    axis.text.x.top = element_text(color = category_colors["Health"]),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_line( color = "grey61", size = 1, linetype = "solid")
  )


}}

