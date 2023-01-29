require(deSolve)

SEIR <- function(time, current_state, params){
  
  with(as.list(c(current_state, params)),{
    N <- S+E+I+R
    dS <- -(beta*S*I)/N
    dE <- (beta*S*I)/N - sigma*E
    dI <- sigma*E - gamma*I - mu*I
    dR <- gamma*I
    dM <- mu*I
    
    return(list(c(dS, dE, dI, dR, dM)))
  })
}


params <- c(beta=0.5, sigma=0.25, gamma=0.2, mu=0.001)

initial_state <- c(S=999999, E=1, I=0, R=0, M=0)

times <- 0:365

model <- ode(initial_state, times, SEIR, params)

summary(model)


# matplot
matplot(model, type='l', lty=1, main='SEIR model', xlab='Time')

legend <- colnames(model)[2:6]

legend('right', legend=legend, col=2:6, lty = 1)


# Wide format
model_df <- as.data.frame(model)

require(ggplot2)

theme_set(
  theme_minimal() +
    theme(legend.position='right') 
)

theme_update(plot.title = element_text(size=11, face='bold', hjust=0.5))

ggplot(model_df, aes(x=time)) +
  geom_line(aes(y=S, color='S')) +
  geom_line(aes(y=E, color='E')) +
  geom_line(aes(y=I, color='I')) +
  geom_line(aes(y=R, color='R')) +
  geom_line(aes(y=M, color='M')) +
  ggtitle('SEIR model') +
  scale_color_manual(values = c( '#52854C', '#FFDB6D', '#D16103', '#4E84C4', '#293352'))

# Long format
require(tidyr)

model_long <- model_df %>% gather(variable, value, -time)

require(wesanderson)

theme_update(plot.title = element_text(size=11, face='bold', hjust=0.5))

ggplot(model_long, aes(x=time, y=value, color=variable)) +
  geom_line() +
  ggtitle('SEIR model') +
  scale_color_manual(values=wes_palette('Moonrise3', 5))


# Time step at which the peak of number of infections occurred.
infections <- as.data.frame(model)$I

peak <- max(infections)

match(peak, infections)
# `match()` returns the first position of the first occurrence of a vaalue in a
# vector.



# Intervention methods
SEIR_lockdown <- function(time, current_state, params){
  
  with(as.list(c(current_state, params)),{
    
    beta = ifelse(
      (time <= start_lockdown || time >= end_lockdown),
      0.5, 0.1
    )
    
    N <- S+E+I+R
    dS <- -(beta*S*I)/N
    dE <- (beta*S*I)/N - sigma*E
    dI <- sigma*E - gamma*I - mu*I
    dR <- gamma*I
    dM <- mu*I
    
    return(list(c(dS, dE, dI, dR, dM)))
  })
}
# `ifelse(test_expression, true_expression, false_expression)` is a vectorized
# version of the if-else statement, which means that it can handle a vector of 
# test expressions an return a vector of results; is useful when performing an
# action on multiple variables or elements of a vector based on a certain 
# condition.

params <- c(
  sigma=0.25,
  gamma=0.2,
  mu=0.001,
  start_lockdown=90,
  end_lockdown=150
)

initial_state <- c(S=999999, E=1, I=0, R=0, M=0)

times <- 0:365

model <- ode(initial_state, times, SEIR_lockdown, params)

summary(model)


# Plot with intervention
matplot(
  model, 
  type="l",
  lty=1, 
  main="SEIR model (with intervention)", 
  xlab="Time"
)

legend <- colnames(model)[2:6]

legend("right", legend=legend, col=2:6, lty = 1)

# Time step at which the peak of number of infections occurred.
infections <- as.data.frame(model)$I

peak <- max(infections)

match(peak, infections)
