# Libraries
library(tidyverse)
library(grid)
library(gridExtra)
library(lattice)
library(png)

setwd(paste(getwd(),"GitHub","Dragon Warrior 7", "planner", sep="\\"))
monster_info <- read_csv("monster_info.csv")
casino <- read_csv("casino.csv")

plot_aster<-function(data)
{
  # ----- This section prepare a dataframe for labels ---- #
  # Get the name and the y position of each label
  label_data=data
  
  # calculate the ANGLE of the labels
  number_of_bar=nrow(label_data)
  angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  
  # calculate the alignment of labels: right or left
  # If I am on the left part of the plot, my labels have currently an angle < -90
  label_data$hjust<-ifelse( angle < -90, 1, 0)
  
  # flip angle BY to make them readable
  label_data$angle<-ifelse(angle < -90, angle+180, angle)
  # ----- ------------------------------------------- ---- #
  
  resColors = c(
    'Fireball'='brown1',
    'Flame'='brown4',
    'Explosion'='chocolate1',
    'Cold'='deepskyblue',
    'Wind'='aquamarine',
    'Electric'='darkgoldenrod1',
    'Death'='black',
    'Banish'='bisque',
    'Def Down'='cyan',
    'Steal MP'='blueviolet',
    'Sleep'='thistle1',
    'Surround'='lightsteelblue2',
    'Confuse'='magenta',
    'Mute'='dimgrey',
    'Fire Breath'='tomato2',
    'Cold Breath'='slateblue1',
    'Poison'='olivedrab',
    'Paralysis'='yellow'
    )
  
  # Start the plot
  aster = ggplot(data, aes(x=as.factor(id), y=value, fill=resistance)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    
    # This add the bars with a blue color
    geom_bar(stat="identity") + 
    
    # Add a gradient
    #scale_fill_gradient2(low='white', mid='snow3', high=resColors, space='Lab', name=NULL, midpoint=1, guide = FALSE) +
    scale_fill_manual(values = resColors, guide=FALSE) +
    
    # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
    ylim(-1,4) +
    
    # Custom the theme: no axis title and no cartesian grid
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()
    ) +
    
    # This makes the coordinate polar instead of cartesian.
    coord_polar(start = 0) +
    
    # Add the labels, using the label_data dataframe that we have created before
    geom_text(data=label_data, aes(x=id, y=value+1, label=resistance, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE )
  return(aster)
}

plot_bar<-function(data)
{
  bar = ggplot(data, aes(x=as.factor(id), y=value, fill=value)) + 
    
    coord_flip() +
    
    geom_bar(stat="identity")+
    
    scale_fill_gradient2(low='red', mid='snow3', high='darkgreen', space='Lab', name=NULL, guide=FALSE)+
    
    ylim(-1,1)+
    
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank()
    ) +
    
    geom_text(data=data, aes(x=id, y=-1, label=characteristic, hjust=0), color="black", fontface="bold", alpha=0.6, size=2.5, inherit.aes = FALSE)
  return(bar)
}

plot_table<-function(data)
{
  return(tableGrob(table_data, rows = NULL))
  
}

heart_grid <- function(data, monster)
{
  heart <- textGrob("Heart")
  
  if(data[1,]["chest"] == "TRUE")
  {
    chest <- rasterGrob(readPNG("sprites\\chest.png"))
  }
  else
  {
    chest <- rasterGrob(readPNG("sprites\\no_chest.png"))
  }
  
  if(data[1,]["casino"] == "TRUE")
  {
    cas <- rasterGrob(readPNG("sprites\\casino.png"))
    
    probdf <- data.frame(
      location = colnames(casino[3:6]),
      value = t(casino[which(tolower(casino$Heart) == monster),][3:6]*100)
    )
    
    #prevent reordering
    probdf$location <- factor(probdf$location, levels=probdf$location)

    prob <- ggplot(probdf, aes(x=location, y=value)) + 
      geom_bar(stat='identity', fill="darkgreen") + 
      ylim(0,11) + 
      labs(y = "% chance", x="Tokens/attempt") +
      coord_flip()
  }
  else
  {
    cas <- rasterGrob(readPNG("sprites\\no_casino.png"))
    prob <- rasterGrob(readPNG("sprites\\empty.png"))
  }

  return(arrangeGrob(heart,chest,cas,prob,ncol=2))
}

geneological_test <- function(col, data)
{
  if(is.na(data[1,][col]))
  {
    return(rasterGrob(readPNG("sprites\\empty.png")))
  }
  else
  {
    return(rasterGrob(readPNG(paste("sprites\\",tolower(data[1,][col]),".png",sep=""))))
  }
}

child_grid <- function(data)
{
  if(is.na(data[1,]["c1"]))
  {
    child <- rasterGrob(readPNG("sprites\\empty.png"))
  }
  else
  {
    child <- textGrob("Leads to")  
  }
  c1 <- geneological_test("c1", data)
  c2 <- geneological_test("c2", data)
  c3 <- geneological_test("c3", data)
  
  return(arrangeGrob(child,c1,c2,c3, ncol=2))
}

parent_grid <- function(data)
{
  if(is.na(data[1,]["p1"]))
  {
    parnt <- rasterGrob(readPNG("sprites\\empty.png"))
  }
  else
  {
    parnt <- textGrob("Requires")  
  }
  p1 <- geneological_test("p1", data)
  p2 <- geneological_test("p2", data)
  p3 <- geneological_test("p3", data)
  
  return(arrangeGrob(parnt,p1,p2,p3, ncol=2))
}

for(idx in seq(nrow(monster_info)))
{
  # Create dataset
  aster_data=data.frame(
    id=seq(1:18), 
    resistance=colnames(monster_info[2:19]), 
    value=t(monster_info[idx,colnames(monster_info[2:19])])
    )
  
  bar_data=data.frame(
    id=seq(1:7),
    characteristic=colnames(monster_info[20:26]),
    value=t(monster_info[idx,colnames(monster_info[20:26])])
  )
  
  table_data=data.frame(
    rank=colnames(monster_info[27:34]),
    battles=t(monster_info[idx,colnames(monster_info[35:42])]),
    skills=t(monster_info[idx,colnames(monster_info[27:34])])
  )
  
  heart_data=data.frame(
    monster_info[idx,43:44]
  )
  
  child_data=data.frame(
    monster_info[idx,45:47]
  )
  
  parent_data=data.frame(
    monster_info[idx,48:50]
  )

  monster=tolower(monster_info[idx,]["Monster"])  
  
  aster = plot_aster(aster_data)
  bar = plot_bar(bar_data)
  tbl = plot_table(table_data)
  heart = heart_grid(heart_data, monster = monster)
  pnt = parent_grid(parent_data)
  child = child_grid(child_data)
  
  pic = rasterGrob(readPNG(paste("sprites\\",monster,".png", sep="")))
  
  lay = rbind(c(1,7,7,2), c(3,4,5,6))
  if(!(is.na(monster_info[idx,]["Mastery Bonus"])))
  {
    g <- arrangeGrob(pic, tbl, heart, pnt, child, bar, aster, 
                     layout_matrix = lay, 
                     top = textGrob(monster_info[idx,]["Monster"], 
                                   gp=gpar(fontface="bold")),
                     bottom = textGrob(paste("Mastery Bonus:", monster_info[idx,]["Mastery Bonus"], sep=" "),
                                       gp=gpar(fontface="bold"))
                     )
  }
  else
  {
    g <- arrangeGrob(pic, tbl, heart, pnt, child, bar, aster, 
                     layout_matrix = lay, 
                     top = textGrob(monster_info[idx,]["Monster"], 
                                    gp=gpar(fontface="bold")
                      ))
  }

  ggsave(file=paste("visualized data\\",monster, ".png", sep=""), plot=g, bg="transparent")
}

