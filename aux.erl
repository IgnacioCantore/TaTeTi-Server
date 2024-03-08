-module(aux).
-export([isNum/1, gameList/0, whosTurn/1, winner/1, print/1, help/0]).
-define(DIGITS, [48,49,50,51,52,53,54,55,56,57]).

%% Comprueba si el caracter pasado es un dígito
isNum(C) ->
  lists:member(C, ?DIGITS).

%% Utilizada por varios comandos
gameList() ->
  gamesPid ! {lsg, self()},
  receive
    List -> List
  end.

%% Comprueba la cantidad de casilleros libres, para determinar de quién es
%% el turno de jugar, y devuelve 1 ó 2
whosTurn(Game) ->
  Free = length(lists:filter(fun(B) -> B == " " end, Game)),
  ((Free + 1) rem 2) + 1.

%% Dada una partida, devuelve 'win' si hay un ganador, 'tie' si hay empate ó
%% 'no' si la partida no terminó
winner([A,B,C,D,E,F,G,H,I]) ->
  if (A == B) and (B == C) and (A /= " ") -> win;
     (D == E) and (E == F) and (D /= " ") -> win;
     (G == H) and (H == I) and (G /= " ") -> win;
     (A == D) and (D == G) and (A /= " ") -> win;
     (B == E) and (E == H) and (B /= " ") -> win;
     (C == F) and (F == I) and (C /= " ") -> win;
     (A == E) and (E == I) and (A /= " ") -> win;
     (C == E) and (E == G) and (C /= " ") -> win;
     true                                 -> case lists:member(" ", [A,B,C,D,E,F,G,H,I]) of
                                               true -> no;  % Si hay casilleros vacíos, la partida sigue
                                               _    -> tie
                                             end
  end.

%% Convierte la partida pasada como argumento (como lista) en una string
%% para enviarla al cliente
print([A,B,C]) ->
  "\n   |   |\n " ++
  A ++ " | " ++ B ++ " | " ++ C ++
  "\n   |   |\n\n";
print([A,B,C | Rest]) ->
  "\n   |   |\n " ++
  A ++ " | " ++ B ++ " | " ++ C ++
  "\n" ++ "___|___|___" ++
  print(Rest).

%% Devuelve una string con los comandos disponibles y los argumentos que se esperan
help() ->
  "CON nombre\n" ++
  "LSG\n" ++
  "NEW\n" ++
  "ACC idjuego\n" ++
  "PLA idjuego jugada\n" ++
  "OBS idjuego\n" ++
  "LEA idjuego\n" ++
  "BYE".
