# Credit : superdatascience.com


# ----- Data
movies <- read.csv("Movie_Ratings.csv")
colnames(movies) <- c("Film", "Genre", "CriticRating", "AudienceRating",
                      "BudgetMillions", "Year")

str(movies)
summary(movies)

movies$Year <- factor(movies$Year)
movies$Film <- factor(movies$Film)
movies$Genre <- factor(movies$Genre)


# ----- Visualisation
library(ggplot2)

# Critic Rating vs Audience Rating
q <- ggplot(data = movies, aes(x = CriticRating, y = AudienceRating, 
                               colour = Genre, size = BudgetMillions))
q + geom_point(aes(size = CriticRating))

# Color Scale
q + geom_point(aes(colour = BudgetMillions))

# Budget vs Audience Rating by Genre
q + geom_point(aes(x = BudgetMillions)) + xlab("Budget Millions $$$")

# Histogram of Budget Distribution by Genre
s <- ggplot(data = movies, aes(x = BudgetMillions))
s + geom_histogram(binwidth = 10, aes(fill = Genre), colour = "black")

# Density Chart of Budget Distribution by Genre
s + geom_density(aes(fill = Genre), position = "stack")

# Genre vs Audience Rating
u <- ggplot(data = movies, aes(x = Genre, y = AudienceRating,
                               color = Genre))
u + geom_jitter() + geom_boxplot(size = 1.2, alpha = 0.5)

# Genre vs Critic Rating
u2 <- ggplot(data = movies, aes(x = Genre, y = CriticRating,
                               color = Genre))
u2 + geom_jitter() + geom_boxplot(size = 1.2, alpha = 0.5)

# Budget Distribution by Genre
v <- ggplot(data = movies, aes(x = BudgetMillions))
v + geom_histogram(binwidth = 10, aes(fill= Genre), colour = 'black') +
  facet_grid(Genre~., scales = "free")

# Scatterplots
w <- ggplot(data = movies, aes(x = CriticRating, y = AudienceRating,
                               colour = Genre))

# Critic Rating vs Audience Rating Evolution by Year
w + geom_point(size = 3) +
  facet_grid(.~Year)

# Critic Rating vs Audience Rating by Genre
w + geom_point(size = 3) +
  facet_grid(Genre~.)

# Critic Rating vs Audience Rating by Genre and Year
w + geom_point(aes(size = BudgetMillions)) +
  facet_grid(Genre~Year) + geom_smooth() +
  coord_cartesian(ylim = c(0, 100))


# ----- Theme
o <- ggplot(data = movies, aes(x = BudgetMillions))
h <- o + geom_histogram(binwidth = 10, aes(fill = Genre), colour = 'black')

# Movie Budget Distribution
h + xlab("Money Axis") +
  ylab("Number of Movies") +
  ggtitle("Movie Budget Distribution") +
  theme(axis.title.x = element_text(colour = "darkgreen", size = 30),
        axis.title.y = element_text(colour = "red", size = 30),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        
        
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 20),
        legend.position = c(1, 1),
        legend.justification = c(1, 1),
        
        plot.title = element_text(colour = "darkblue", size = 40, family = "serif"))


# ----- New Data
mv <- read.csv("Box_Office.csv")

mv$Day.of.Week <- factor(mv$Day.of.Week)
mv$Director <- factor(mv$Director)
mv$Genre <- factor(mv$Genre)
mv$Movie.Title <- factor(mv$Movie.Title)
mv$Release.Date <- factor(mv$Release.Date)
mv$Studio <- factor(mv$Studio)

# Filter Data
f1 <- (mv$Genre == "action") | (mv$Genre == "adventure") | (mv$Genre == "animation") |
  (mv$Genre == "comedy") | (mv$Genre == "drama")
f2 <- (mv$Studio == "Buena Vista Studios") | (mv$Studio == "Fox") | 
  (mv$Studio == "Paramount Pictures") | (mv$Studio == "Sony") | (mv$Studio == "Universal") |
  (mv$Studio == "WB")
mvf <- mv[f1 & f2,]


# ----- Visualisation
p <- ggplot(data = mvf, aes(x = Genre, y = Gross...US))

# Domestic Gross % in US by Genre
plot <- p + geom_jitter(aes(size = Budget...mill., colour = Studio), alpha = 0.4) + geom_boxplot(alpha = 0.7)
plot + xlab("Genre") +
  ylab("Gross % US") +
  ggtitle("Domestic Gross % by Genre") +
  theme(axis.title.x = element_text(colour = "blue", size = 20),
        axis.title.y = element_text(colour = "blue", size = 20),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        
        plot.title = element_text(size = 25, hjust = 0.5))