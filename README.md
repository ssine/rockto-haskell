# rockto-haskell

A remake of retro chip-8 game *[rockto](https://johnearnest.github.io/chip8Archive/play.html?p=rockto)* in terminal with haskell.

Collaborators (in alphabetical order): [dofup](https://github.com/dofup), [shijiesun-edu](https://github.com/shijiesun-edu), [ssine](https://github.com/ssine), [ZJHCSE](https://github.com/ZJHCSE).

<img alt="rockto" src="assets/rockto.gif" width="500"/>


The goal is to collect all parcels in the map and get to the exit.

Game rules:

1. A map is a m * n grid, where each square is one of
   - brick
   - parcel
   - wall, blocks anything
   - scaffold, supports bricks and parcels above
   - exit
2. Brick or parcel falls down if there is nothing supporting, and kills the player on hit
3. Brick can be pushed left or right by player if there is nothing on the other side
4. Player collects the parcel when their positions overlap
5. Player destroys the scaffold when their positions overlap


Getting Started
---------------

```
$ make build
$ make run
```
