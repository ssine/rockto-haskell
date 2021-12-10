
# rockto-haskell

A remake of retro chip-8 game *[rockto](https://johnearnest.github.io/chip8Archive/play.html?p=rockto)* in terminal with haskell.

Collaborators (in alphabetical order): [dofup](https://github.com/dofup), [shijiesun-edu](https://github.com/shijiesun-edu), [ssine](https://github.com/ssine), [ZJHCSE](https://github.com/ZJHCSE).

<img alt="rockto" src="assets/rockto.gif" width="500"/>


The goal is to collect all parcels in the map and get to the exit.

Game rules:

1. A map is a m * n grid, where each square is one of
   - rock
   - parcel
   - wall, blocks anything
   - scaffold, supports rocks and parcels above
   - exit
2. Rock or parcel falls down if nothing supporting, and kills the player on hit
3. Rock can be pushed left or right by the player if there is nothing on the other side
4. The player collects the parcel when their positions overlap
5. The player destroys the scaffold when their positions overlap


# Getting Started

Try it in docker:

```bash
docker run -it sineliu/rockto
```

or build it locally:

```bash
make build
make run
```

# Milestone

## Architecture

Our project is developed on top of the [Brick](https://github.com/jtdaugherty/brick) framework. With it, we only need to care about the main processing logic and display styles.

The implementation is composed of four main components:
- **Event Handling**: serves as a dispatcher. It calls state transform functions with different parameters according to users' actions. The overall game progress like restarting or quitting is also handled by it.
- **State Transformation**: controls the state update logic. The main game logic is implemented in it. Validity checking and score monitoring also happened here.
- **Rendering**: draws and updates the user interface according to the current state. It designs the layout and generates the specification of UI.
- **Resource Management**: provides the maps and initial states for the game. It loads predefined settings from files and sets the goal for each round of the game.

Underneath, we have unified data modeling, type declaration, and an assembly of reusable helper functions, making the development of each part as well as their interactions concise and efficient.


A quick look at the current project layout:
```txt
Rockto
├── app                     -- interaction related logic
│    ├── Main.hs            -- app definition, event handlers
│    └── UI.hs              -- rendering logic
└── src                     -- pure processing logic
|    ├── Rockto
|    |     ├── Types.hs     -- type declaration
|    |     ├── Tick.hs      -- state transform
|    |     ├── Resource.hs  -- load game setting
|    │     └── Utils.hs     -- helper functions
|    └── Rockto.hs
├── rockto.cabal
...
```

Key functions:
```hs
handleEvent :: GSt -> BrickEvent () EvTick -> EventM () (Next GSt)

tick :: Direction -> GSt -> GSt

drawUI :: GSt -> [Widget ()]

loadGame :: Int -> IO Round
```

## Challenges

1. The user guide of the `Brick` library is quite tedious. We could still not figure out where to start after the first several readings.
	- To get started, we went to look at the implementations of listed "featured projects". It helped us acquire some basic knowledge about the Brick through the comparisons of the similar/different parts of their implementation. We also explored the file structures of our past homework to list the minimal components a Haskell project should have and what they look like (e.g., `*.cabal`, `stack.yaml`, etc).
2. There is animation during the game. For example, the rock may fall when the below scaffold supporting it is destroyed by the player. We thought about asynchronous processing methods like multithreading at first glance. But then we found it extremely complex. We need to share states between threads, as well as making a lot of conditions evaluation and state updating to handle the simultaneous actions of the player (What to do when the player moves towards the rock again when it is falling?).
	- To handle this, we decided to add a restriction for the game: the player is not allowed to take any move action when the animation is going on. We simply stop responding to the player till it is finished. So multithreading is unnecessary. Just let the renderer do the job!
	- However, how should we notify the renderer which rock will fall and what are its start and end positions? Either we could precompute states for each timestamp and then send the states list to the renderer, or we could mix the rendering logic and the state transformation logic and let the renderer handle the entire process. Both of them would be ugly.
	- Instead, we put up with another solution. An additional boolean attribute "stable" is added to the global state and we use it to indicate whether we are in the animation or not now. The value is computed at every step. If it is `True`, we will wait to handle another user input. Otherwise, we will wait a moment and then directly start our next state updating process (like an update callback). Take as an example a rock falls from (1, 2) to (1, 0). In the first step, its position is updated to (1, 1), with stable=False. Then, we will not exit the handling process but directly go compute this logic again after a short wait. The position is thus updated to (1, 0) with stable=True. From here we finish the event handling and wait for the next input action. With this, the rendering logic will not need to be modified at all!
3. Another problem arose after solving challenge 2: the user interface can not be redrawn during event handling. The rendering is only called after each event handling. Therefore, no intermediate status but only the static result of object falling is visible, as all of the transformations happen in a single event handling process.
	- To make the TUI refreshable, we created a custom event type `EvTick`. Then we opened a forked process to periodically feed the event type into the channel with the help of `Brick.BChan` module. With it, we could capture this event in the handler regularly and decide whether to transform the global state for UI refresh or not.

## Progress

Currently we have made some progress on the project. The overall framework is drafted and the main APIs of the key modules have been designed. Each one of us has a clear understanding of our roles and is working on our corresponding parts. Hopefully we will start testing this week.

We are confident to get things done on time now. Beyond that, we are also thinking about involving more features, like implementing more game functions, improving the program performance through `lens` libraries, and so on.

