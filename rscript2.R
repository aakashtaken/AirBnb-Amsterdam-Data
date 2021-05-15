list(rm=ls())


list.files()

library(data.table)
library(ggplot2)
library(dplyr)
library(stringr)
library(reshape2)


l20 <- fread("listings2020.csv.gz")
l21 <- fread("listings2021.csv.gz")
l20$id <- paste0(l20$id,"20")
l21$id <- paste0(l21$id, "21")

fact_listings <- rbind(l20,l21, fill=TRUE)


l20 <- read.csv("listings2020.csv")
l21 <- read.csv("listings2021.csv")
l20$id <- paste0(l20$id,"20")
l21$id <- paste0(l21$id, "21")
view_listings <- rbind(l20,l21)
rm(l20)
rm(l21)

df <- left_join(view_listings, fact_listings[,c("id","accommodates","amenities", "review_scores_rating","last_scraped")], by = "id")
df$year <- as.integer(substr(df$last_scraped,1,4))

dim_amenities <- df[,c("id","amenities")]


columns <- str_split(str_replace_all(dim_amenities[,2], pattern = c('"|\\[|\\]|\\{|\\}'), replacement = ""), pattern = ',')
columns <- unique(do.call(c, columns))

columnsdim <- data.frame(matrix(ncol=611,nrow=37103))
colnames(columnsdim) <- columns

t <- cbind(dim_amenities, columnsdim)

# for(c in 3:length(t)){
#   for(i in 1:nrow(t)){
#     #paste(i,colnames(t[c]),grepl(colnames(t[c]),t[i,c("amenities")]))
#     t[i,c] <- grepl(colnames(t[c]),t[i,c("amenities")])
#   }
# }
# 

for(c in 3:length(t)){
  t[c]<- sapply(colnames(t[c]), grepl, t$amenities)
}

amenities <- melt(t, id.vars = c("id", "amenities"),
                  variable.name = "variable", 
                  value.name = "value")

rm(columns)
rm(columnsdim)
rm(t)
rm(dim_amenities)




# Total number of listings
length(df$id)

library(formattable)

customGreen0 = "#DeF7E9"

customGreen = "#71CA97"

customRed = "#ff7f7f"

# total number of listings per neighbourhood by room type & average price

il1<- left_join(dcast(df %>% group_by(neighbourhood,year) %>% 
                        summarise(`Average Price` = round(mean(price),0)
                        ), neighbourhood ~ year, value = `Average Price`),
  dcast(
    df %>% group_by(neighbourhood,year,room_type) %>% summarise(total = n()),
    neighbourhood ~ room_type+year, value = "total"),
  by = "neighbourhood")

il1[is.na(il1)] <- 0

il1$`2021_abs`<- il1$`2021`-il1$`2020`
il1$`Entire home/apt` <- il1$`Entire home/apt_2021`-il1$`Entire home/apt_2020`
il1$`Hotel room` <- il1$`Hotel room_2021`-il1$`Hotel room_2020`
il1$`Private room` <- il1$`Private room_2021` - il1$`Private room_2020`
il1$`Shared room` <- il1$`Shared room_2021`-il1$`Shared room_2020`

il1$`Variance in Total Listings`<- rowSums(il1[,c("Entire home/apt","Hotel room","Private room","Shared room")])
il1 <- il1 %>% arrange((`Variance in Total Listings`))
formattable(il1, 
            align =c("l","c","c","c","c", "c", "c", "c", "c","c"), 
            list(`neighbourhood` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")) ,
              `2021`= formatter("span", style = ~ style(color = ifelse(`2021` >`2020`, "green", "red")),
                                ~ icontext(ifelse(`2021` >`2020`,"arrow-up", "arrow-down"), `2021`)),
              area(col = c("Entire home/apt","Hotel room","Private room","Shared room")) ~ color_tile("pink", "transparent"),
              `Entire home/apt_2020` = FALSE,
              `Entire home/apt_2021` = FALSE,
              `Hotel room_2020` = FALSE,
              `Hotel room_2021` = FALSE,
              `Private room_2020` = FALSE,
              `Private room_2021` = FALSE,
              `Shared room_2020` = FALSE,
              `Shared room_2021` = FALSE,
              `2021_abs` = FALSE,
              `2020` = FALSE,
              `Variance in Total Listings` = FALSE
              ))








