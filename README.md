# scoresim

Introduction
================
Scoresim is a programming contest scoreboard simulator, for ACM-ICPC teams.
You have to choose a competition and the simulator will show the scoreboard minute by minute.

Screenshots
================
![alt Home Screen](https://raw.githubusercontent.com/karupayun/nek/master/examples/homescreen.png)

![alt An example competition](https://raw.githubusercontent.com/karupayun/nek/master/examples/score.png)

Building
================
First you have to download Stack:
> sudo apt install haskell-stack

> stack setup
> stack build
> stack exec scoresim

If there are not errors, writing http://0.0.0.0:8000/ you can see the home screen.

Usage
================
> You have to create an user.
> You have to load a competition passing an html file.
> You can add your own teams.
> There are a few competitions already loaded.
> Press start/pause/stop.


Modules
================
- Parser: To parse an html.
- Parser Kattis: An instance of parser to parse a kattis html.
- Score: To upload scoreboards.
- Teams: Your teams.
- ContestPrinter: Print the scoreboard
- User: User interface.
- Form: User actions.


For more detail, see the report (Informe.pdf) in spanish.
