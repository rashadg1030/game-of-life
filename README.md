# game-of-life

![game-of-life logo](https://user-images.githubusercontent.com/52359514/60774904-1617e680-a0e9-11e9-9898-f9a950514874.png)

Play [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life). 

## Prerequisites

Must have `ghc` installed. Instructions [here](https://downloads.haskell.org/~ghc/6.8.3/docs/html/users_guide/installing-bin-distrib.html)

Must have `stack` installed

Run the command `curl -sSL https://get.haskellstack.org/ | sh` to install it

Must have `libsdl2-dev` installed

Run the command `sudo apt-get install libsdl2-dev` to install it

### Windows 

Not sure how to get this running on a Windows system. Feel free to contrubite to this section if you can.

## Getting Started

1. Clone the repository

2. From the base folder, open up a terminal

3. Run the command `stack build`

4. Run the command `stack exec game-of-life`

## How to Play

Use `Tab` to play or pause the simulation.

When the simulation is paused, you can edit the grid.

Use the arrow keys to navigate the red cursor around the the grid.

Press `Enter` to bring a cell to life.

Press `Backspace` to kill a cell.

Enjoy!
