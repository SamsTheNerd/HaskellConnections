
![connections cli startup banner](https://github.com/SamsTheNerd/HaskellConnections/blob/main/banner.png?raw=true)

# Haskell Connections CLI

This is a command line version of nyt connections developed with Haskell. It can play local game files (see below for format) or fetch games [from online](https://github.com/Eyefyre/NYT-Connections-Answers/). 

## Install

The easiest method is to go to the [releases page](https://github.com/SamsTheNerd/HaskellConnections/releases), download the appropriate zip for your OS, and run the executable included. The files in the releases are compiled [via GitHub Actions](https://github.com/SamsTheNerd/HaskellConnections/actions).

If for whatever reason that doesn't work for your system, you can download/clone [this repo](https://github.com/SamsTheNerd/HaskellConnections), install cabal via [ghcup](https://www.haskell.org/ghcup/), run `cabal install` from the directory with the `haskellconnections.cabal` file, and then run `haskellconnections`. 

## Image Gallery 

### Startup screen: 

<img src="https://github.com/SamsTheNerd/HaskellConnections/blob/main/startupscreen.png?raw=true" alt="The game's startup screen" width="400"/>

### Gameplay:

<img src="https://github.com/SamsTheNerd/HaskellConnections/blob/main/gameplayimage.png?raw=true" alt="Screenshot of the middle of a game" width="250"/>

### Win Screen: 

<img src="https://github.com/SamsTheNerd/HaskellConnections/blob/main/winscreen.png?raw=true" alt="Screenshot of the win screen" width="250"/>

## Local File Format

```
Y:<CATEGORY_TITLE> <WORD_1> <WORD_2> <WORD_3> <WORD_4>
G:...
B:...
P:...
```

Example:
```
Y:COOK_WITH_HEAT_AND_WATER BLANCH BOIL POACH STEAM
G:COMMON_PERFUME_INGREDIENTS AMBERGRIS MUSK ROSE VANILLA
B:CHARACTERS_WITH_PET_DOGS CHARLIE DOROTHY SHAGGY WALLACE
P:CAPITAL_CITY_HOMOPHONES KETO ROAM SOPHIA SOUL
```