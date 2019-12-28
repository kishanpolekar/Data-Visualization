library(ggplot2)
library(dplyr)
library(gridExtra)
df = read.csv(file = "CompleteDataset.csv", stringsAsFactors = FALSE)
df = tbl_df(df)
df <- select(df, ID, X, Name, Age, Nationality, Overall, Club, Value, Wage, Preferred.Positions)

df$Preferred.Positions <- gsub(" ", "", substr(df$Preferred.Positions, 1, 3))

x <- as.factor(df$Preferred.Positions)
levels(x) <- list(GK  = c("GK"), 
                  DEF = c("LWB", "LB", "CB", "RB", "RWB"), 
                  MID = c("LW","LM","CDM","CM","CAM","RM","RW"), 
                  FWD = c("CF", "ST"))
df <- mutate(df, Position = x)
head(df)

g_age <- ggplot(data = df, aes(Age))
g_age + 
    geom_histogram(col="red", aes(fill = ..count..)) + ggtitle("Distribution based on Age")

countries_count <- count(df, Nationality)
top_10_countries <- top_n(countries_count, 10, n)
top_10_country_names <- top_10_countries$Nationality

country <- filter(df, Nationality == top_10_country_names)
ggplot(country, aes(x = Nationality)) + 
    geom_bar(col = "red", aes(fill = ..count..)) + ggtitle("Distribution based on Nationality of Players (Top 10 Countries)")

g_age + 
    geom_density(data=country, col="red", aes(fill = Nationality), alpha=0.5) + facet_grid(.~country$Nationality) +
    ggtitle("Distribution based on Age and Nationality")

pos_ctry<-ggplot(country, aes(x=Position, y=..count..))
pos_ctry + 
    geom_bar(col="red", aes(fill=Nationality)) +
    ggtitle("Distribution of Positions based on Country")

group_clubs <- group_by(df, Club)
top_10_valuable_clubs <- top_n(club_value, 10)
top_10_valuable_clubs$Club <-as.factor(top_10_valuable_clubs$Club)
ggplot(top_10_valuable_clubs, aes(x=Club, y=total_val)) + 
    geom_bar(stat="identity", col = "red", aes(fill=total_val)) + ggtitle("Distribution based on Club Value (Top 10 Clubs)")
