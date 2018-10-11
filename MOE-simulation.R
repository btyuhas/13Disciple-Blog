library(ggplot2)

#these are the variables you play with
gamecount = 250

good_game_damage <- 2200  #your raw average combined
good_game_variation <- 400 # shots?

bad_game_damage <- 500     # average combined of a bad match
bad_game_variation <- 300  # shots?

potato_rate <- 20#% of games you have a bad game

val95 = 2283 #the 95% MoE Value
val85 = 2043 #the 85% MoE Value
val65 = 1562 #the 65% MoE Value

#this section simulates the potato rate and the two damage distributions from the above variables
#set.seed(180)
gamequality <- runif(gamecount, min = 1, max=100)
x <- ifelse(gamequality > potato_rate, rnorm(gamecount, good_game_damage, sd=good_game_variation),
            (pmax(rnorm(gamecount, bad_game_damage, sd=bad_game_variation), 0)))

#this is the actual moe value creation
moe <- c(0)
for(i in 2:gamecount){
  moe <- c(moe, moe[i-1] + 0.01980198*(x[i]-moe[i-1]))
}

#this section makes some string variables for display on the graph
dmglabel = paste("Avg Dmg =", good_game_damage,"+/-", good_game_variation, sep=" ")
potatopct = paste("Bad Game % =", potato_rate, sep = " ")
potatolabel = paste("Bad Game Avg =", bad_game_damage, "+/-", bad_game_variation, sep=" ")

#and this makes a data frame and graphs it
df <- data.frame(games=x, moe=moe)
ggplot(df, aes(x=1:gamecount)) + 
  geom_line(aes(y=games)) +
  geom_line(aes(y=moe), col='red') +
  ggtitle('Marks of Excellence Progression') +
  xlab('Number of Games') +
  ylab('Damage + Assisted') +
  geom_hline(aes(yintercept = val95), color='purple', linetype='dashed')+
  geom_hline(aes(yintercept = val85), color='green', linetype='dashed')+
  geom_hline(aes(yintercept = val65), color='yellow', linetype='dashed')+
  annotate("text", x=(gamecount*.6), y=1000, label=dmglabel)+
  annotate("text", x=(gamecount*.6), y=800, label=potatopct)+
  annotate("text", x=(gamecount*.6), y=600, label=potatolabel)