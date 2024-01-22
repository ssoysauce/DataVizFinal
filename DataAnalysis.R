library(tidyverse)

Housing = read.csv("Housing.csv")

View(Housing)

summary(Housing)

# install.packages("janitor")
library(janitor)
clean_names(Housing)

ggplot(data = Housing, mapping = aes(x = bedrooms, y = price)) +
         geom_point()

#Price variable is too big. Downsizing to thousands of dollars by dividing
#values by 1000

Housing <- Housing |> 
  mutate(price_k = price / 1000)

ggplot(data = Housing, mapping = aes(x = bedrooms, y = price_k)) +
  geom_point()


Housing |> 
  slice(which(row_number() %% 20 == 1)) |> 
  ggplot(mapping = aes(x = price_k)) +
    geom_bar()
#just trying to get a feel of how the data is spread visually

ggplot(data = Housing, mapping = aes(x = area, y = price_k)) +
  geom_point(aes(color = bedrooms, size = bathrooms)) +
  geom_smooth(color = "black") +
  scale_color_gradientn(colors=c("blue", "yellow"))

housing_pivot <- Housing |> 
  pivot_longer(cols = c(bedrooms, bathrooms, stories),
               names_to = "Additions")
ggplot(data = housing_pivot, aes(y = price_k,
                                 x = value,
                                 col = Additions,
                                 group = Additions)) +
  geom_point()

Housing$price_level <- ifelse(Housing$price <= 3430000, 1,
                              ifelse(Housing$price <= 4340000, 2,
                                     ifelse(Housing$price <= 5740000, 3, 4)))

Housing |> 
  ggplot(aes(x = price_level)) +
  geom_bar(aes(fill = furnishingstatus))
#shows that whether a house is furnished or not
#definitely plays a role in its price


Housing |> 
  ggplot(aes(x = price_level)) +
  geom_bar(aes(fill = hotwaterheating))

Housing |> 
  ggplot(aes(x = price_level)) +
  geom_bar(aes(fill = airconditioning))

Housing |> 
  ggplot(aes(x = price_level)) +
  geom_bar(aes(fill = prefarea))

Housing |> 
  ggplot(aes(x = price_level)) +
  geom_bar(aes(fill = guestroom))


#Lets try some animation!!!!
#install.packages("gganimate")
#install.packages("gifski")

library(gganimate)

price_air_graph <- Housing |> 
  ggplot(aes(x = airconditioning, fill = airconditioning)) +
  geom_bar()

price_air_graph

price_air_graph <- Housing |> 
  ggplot(aes(x = airconditioning, fill = airconditioning)) +
  geom_bar() +
  theme_bw()

price_air_graph.animation <- price_air_graph +
  # gganimate specific bits:
  transition_time(
    as.integer(price_level)) +
  labs(title = "Price Level: {frame_time}", caption= "Price Level 1: <$3430000 \n Price Level 2: $3430000 - $4340000 \n Price Level 3: $4340000 - $5740000 \n Price Level 4: $5740000+ ") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))+
  ease_aes('sine-in-out')

p <- animate(price_air_graph.animation,
        height = 500,
        width = 800,
        fps = 30,
        duration = 10,
        end_pause = 60,
        res = 100,
        renderer = gifski_renderer())

anim_save("price-air-graph.gif", p)

# Success. Updating previous graphs to be animations as well so I can put them
# all one page and have it be cool.

price_furnish_graph <- Housing |> 
  ggplot(aes(x = furnishingstatus, fill = furnishingstatus)) +
  geom_bar() +
  theme_bw()

price_furnish_graph.animation <- price_furnish_graph +
  # gganimate specific bits:
  transition_time(
    as.integer(price_level)) +
  labs(x = "Furnished") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))+
  ease_aes('sine-in-out')

p <- animate(price_furnish_graph.animation,
             height = 500,
             width = 800,
             fps = 30,
             duration = 10,
             end_pause = 60,
             res = 100,
             renderer = gifski_renderer())


anim_save("price-furnish-graph.gif", p)

#hot water heating

price_hotwater_graph <- Housing |> 
  ggplot(aes(x = hotwaterheating, fill = hotwaterheating)) +
  geom_bar() +
  theme_bw()

price_hotwater_graph.animation <- price_hotwater_graph +
  # gganimate specific bits:
  transition_time(
    as.integer(price_level)) +
  labs(x = "Hot Water Heating") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))+
  ease_aes('sine-in-out')

p <- animate(price_hotwater_graph.animation,
             height = 500,
             width = 800,
             fps = 30,
             duration = 10,
             end_pause = 60,
             res = 100,
             renderer = gifski_renderer())


anim_save("price-hotwater-graph.gif", p)

#preferred area

price_prefarea_graph <- Housing |> 
  ggplot(aes(x = prefarea, fill = prefarea)) +
  geom_bar() +
  theme_bw()

price_prefarea_graph.animation <- price_prefarea_graph +
  # gganimate specific bits:
  transition_time(
    as.integer(price_level)) +
  labs(x = "In a Preferred Area") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))+
  ease_aes('sine-in-out')

p <- animate(price_prefarea_graph.animation,
             height = 500,
             width = 800,
             fps = 30,
             duration = 10,
             end_pause = 60,
             res = 100,
             renderer = gifski_renderer())


anim_save("price-prefarea-graph.gif", p)

# guest room

price_guestroom_graph <- Housing |> 
  ggplot(aes(x = guestroom, fill = guestroom)) +
  geom_bar() +
  theme_bw()

price_guestroom_graph.animation <- price_guestroom_graph +
  # gganimate specific bits:
  transition_time(
    as.integer(price_level)) +
  labs(x = "Has a Guest Room") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))+
  ease_aes('sine-in-out')

p <- animate(price_guestroom_graph.animation,
             height = 500,
             width = 800,
             fps = 30,
             duration = 10,
             end_pause = 60,
             res = 100,
             renderer = gifski_renderer())


anim_save("price-guestroom-graph.gif", p)

#mainroad

price_mainroad_graph <- Housing |> 
  ggplot(aes(x = mainroad, fill = mainroad)) +
  geom_bar() +
  theme_bw()

price_mainroad_graph.animation <- price_mainroad_graph +
  # gganimate specific bits:
  transition_time(
    as.integer(price_level)) +
  labs(x = "On a Main Road") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))+
  ease_aes('sine-in-out')

p <- animate(price_mainroad_graph.animation,
             height = 500,
             width = 800,
             fps = 30,
             duration = 10,
             end_pause = 60,
             res = 100,
             renderer = gifski_renderer())


anim_save("price-mainroad-graph.gif", p)

#basement

price_basement_graph <- Housing |> 
  ggplot(aes(x = basement, fill = basement)) +
  geom_bar() +
  theme_bw()

price_basement_graph.animation <- price_basement_graph +
  # gganimate specific bits:
  transition_time(
    as.integer(price_level)) +
  labs(x = "Basement") +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))+
  ease_aes('sine-in-out')

p <- animate(price_basement_graph.animation,
             height = 500,
             width = 800,
             fps = 30,
             duration = 10,
             end_pause = 60,
             res = 100,
             renderer = gifski_renderer())


anim_save("price-basement-graph.gif", p)

#Exporting Data
write.csv(Housing, "HousingFinal.csv", row.names=TRUE)

#install.packages('rgl')
library(rgl)

mycolors <- c('royalblue1', 'darkcyan', 'oldlace')

Housing |> 
  plot_ly(x = ~price_level, y = ~area, z= ~stories)
