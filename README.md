# 2048Haskell

```haskell
┏━━Score━━┓    ┏━━━━━━━━━━━━━━━━2048━━━━━━━━━━━━━━━━┓    ┏━━━━━Commands━━━━━┓
┃         ┃    ┃┏━━━━━━━┓┏━━━━━━━┓┏━━━━━━━┓┏━━━━━━━┓┃    ┃ Left           ← ┃
┃   256   ┃    ┃┃       ┃┃       ┃┃       ┃┃       ┃┃    ┃ Right          → ┃
┃         ┃    ┃┃  256  ┃┃   8   ┃┃   4   ┃┃  16   ┃┃    ┃ Down           ↓ ┃
┗━━━━━━━━━┛    ┃┃       ┃┃       ┃┃       ┃┃       ┃┃    ┃ Restart        r ┃
               ┃┗━━━━━━━┛┗━━━━━━━┛┗━━━━━━━┛┗━━━━━━━┛┃    ┃ Quit    q or esc ┃
               ┃┏━━━━━━━┓┏━━━━━━━┓┏━━━━━━━┓┏━━━━━━━┓┃    ┗━━━━━━━━━━━━━━━━━━┛
 GAME OVER     ┃┃       ┃┃       ┃┃       ┃┃       ┃┃
               ┃┃   4   ┃┃  64   ┃┃  16   ┃┃   8   ┃┃
               ┃┃       ┃┃       ┃┃       ┃┃       ┃┃
               ┃┗━━━━━━━┛┗━━━━━━━┛┗━━━━━━━┛┗━━━━━━━┛┃
               ┃┏━━━━━━━┓┏━━━━━━━┓┏━━━━━━━┓┏━━━━━━━┓┃
               ┃┃       ┃┃       ┃┃       ┃┃       ┃┃
               ┃┃   2   ┃┃  16   ┃┃   8   ┃┃   4   ┃┃
               ┃┃       ┃┃       ┃┃       ┃┃       ┃┃
               ┃┗━━━━━━━┛┗━━━━━━━┛┗━━━━━━━┛┗━━━━━━━┛┃
               ┃┏━━━━━━━┓┏━━━━━━━┓┏━━━━━━━┓┏━━━━━━━┓┃
               ┃┃       ┃┃       ┃┃       ┃┃       ┃┃
               ┃┃  16   ┃┃   8   ┃┃   4   ┃┃   2   ┃┃
               ┃┃       ┃┃       ┃┃       ┃┃       ┃┃
               ┃┗━━━━━━━┛┗━━━━━━━┛┗━━━━━━━┛┗━━━━━━━┛┃
               ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
```

## Run:

```sh
cabal run 2048Haskell
```

You will then be greeted by a screen on which you can select the game mode:

```
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━Select mode (type key)━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃ h                                                        Human Player (YOU!) ┃
┃ m                                                            Monte Carlo Bot ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛

```

## Rules:

Board is a 4x4 grid with 16 square tiles. Possible moves: Left <kbd>←</kbd>, Right <kbd>→</kbd>, Up <kbd>↑</kbd>, Down <kbd>↓</kbd>

1. **+2/+4**: At each step a new tile that is a random multiple of 2 will be placed in the board.
2. **move**: When you select one of the moves, all tiles will move as far as possible in that direction in the grid.
3. **x+x=2x**: If a tile is moved to a tile of the same value, then they will combine into 1 tile with the value of their sum.
4. **2048:** The objective is to get a highest tile value. You win by reaching the tile `2048`.
