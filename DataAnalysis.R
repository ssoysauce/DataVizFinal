library(tidyverse)

Housing = read.csv("Housing.csv")

View(Housing)

summary(Housing)

install.packages("janitor")
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

