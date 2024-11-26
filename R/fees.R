library(tidyverse)
fees <- data.frame(P=seq(from=0.01, to=0.99, by=0.01))
contracts <- 100
fees <- fees |> mutate(F=ceiling((contracts * 0.07 * P * (1-P))*100)/(100*contracts))
fees <- fees |> mutate(U=(1/(P+F)-1))
fees <- fees |> mutate(S=F/P)



ggplot(fees, aes(x=P, y=F)) + geom_line() +theme_bw() +scale_y_continuous(n.breaks = 12) +
  labs(y='Average fee per contract',title='Kalshi fees', subtitle=paste(contracts, "contracts")) +scale_x_continuous(n.breaks = 10)

ggplot(fees, aes(x=P, y=S)) + geom_line() +theme_bw() +scale_y_continuous(n.breaks = 12) +
  labs(y='Average % fee',title='Kalshi fees as % of P', subtitle=paste(contracts, "contracts")) +scale_x_continuous(n.breaks = 10)

ggplot(fees[11:99,], aes(x=P, y=U*100)) + geom_line() +theme_bw() +scale_y_continuous(n.breaks = 12) +
  labs(y='Return %',title='Upside after fees', subtitle=paste(contracts, "contracts")) +scale_x_continuous(n.breaks = 10)


