# NFL In Game Win Probability Model

An experiment to build a play by play win probability model.

Inspired by a [win probability model article](https://medium.com/@technocat79/building-a-basic-in-game-win-probability-model-for-the-nfl-54600e57fe1c) at Medium.

## Description

Uses logistic regression to model wins at each point of the game based on the score, field position, home field advantage, distance to the first down, time remaining, etc.

## Chart

![In Game Win Probability](out/wp-2020_16_LA_SEA.png)

The chart displays the output of the model (black), the built-in nflfastR win probability model (blue), scoring events (lines at bottom), and other reference lines (50% probability, times of quarters, etc.).

The nflfastR is more accurate because it also uses:

- Expected points
- Other attributes

My accuracy:

![In Game Win Probability](out/accuracy.png)

## Reference

- [nflfastR package](https://www.nflfastr.com/index.html)
- [Description of nflfastR approach](https://www.opensourcefootball.com/posts/2020-09-28-nflfastr-ep-wp-and-cp-models/)
- [Earlier article about using nflscrapR for a win probability model](https://medium.com/@technocat79/building-a-basic-in-game-win-probability-model-for-the-nfl-54600e57fe1c)
