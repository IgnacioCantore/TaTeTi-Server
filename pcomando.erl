-module(pcomando).
-export([pcomando/5]).
-import(game, [play/2]).
-import(aux, [isNum/1, gameList/0, print/1, help/0]).

%% Función encargada de realizar el pedido del cliente y devolver una respuesta
%% a psocket
pcomando(PSocketPid, CMD, UPDPid, false, _) ->
  case CMD of
    ["CON", Name] -> Last = lists:last(Name),
                     case isNum(Last) of
                       true -> "ERROR CON: ultimo caracter no puede ser un numero";  % Porque el Id de las partidas es "Name" + un número
                       _    -> NameAtom = erlang:list_to_atom(Name),
                               case global:register_name(NameAtom, PSocketPid) of
                                 yes -> "OK CON: " ++ Name;
                                 no  -> "ERROR CON: " ++ Name ++ " no disponible"
                               end
                     end;
    ["BYE"]       -> UPDPid ! bye,
                     "OK BYE";
    ["HELP"]      -> help();
    _             -> "ERROR: debe ingresar un nombre primero"
  end;
pcomando(_, CMD, UPDPid, Name, Count) ->
  case CMD of
    ["CON", _]            -> "ERROR CON: ya registrado como " ++ Name;
    ["LSG"]               -> List = gameList(),
                             case List of
                               [] -> "OK LSG: no hay partidas";
                               _  -> Games = [{Id, P1, P2} || {Id, P1, P2, _, _, _, _} <- List],
                                     "OK LSG:\n" ++ lists:map(fun(Game) -> io_lib:format("~p~n", [Game]) end, Games)
                             end;
    ["NEW"]               -> gamesPid ! {new, self(), UPDPid, Name, Count},
                             receive
                               Id -> "OK NEW: " ++ Id
                             end;
    ["ACC", GameId]       -> List = gameList(),
                             case lists:keyfind(GameId, 1, List) of
                               false                              -> "ERROR ACC: " ++ GameId ++ " no existe";
                               {_, P1, esperando, _, _, _, Where} -> if Name == P1 -> "ERROR ACC: " ++ GameId ++ " ya esta jugando esta partida";
                                                                        true       -> {gamesPid, Where} ! {acc, self(), UPDPid, Name, GameId},
                                                                                      receive
                                                                                        ok -> "OK ACC: " ++ GameId;
                                                                                        _  -> "ERROR ACC: " ++ GameId ++ " esta llena"
                                                                                      end
                                                                     end;
                               {_, P1, P2, _, _, _, _}            -> if (Name == P1) or (Name == P2) -> "ERROR ACC: " ++ GameId ++ " ya esta jugando esta partida";
                                                                        true                         -> "ERROR ACC: " ++ GameId ++ " esta llena"
                                                                     end
                             end;
    ["PLA", GameId, Play] -> List = gameList(),
                             case lists:keyfind(GameId, 1, List) of
                               false                          -> "ERROR PLA: " ++ GameId ++ " no existe";
                               {_, P1, P2, _, _, Game, Where} -> PlayNum = list_to_integer(Play),
                                                                 case play(Name, {P1, P2, Game, PlayNum}) of
                                                                   noplayer       -> "ERROR PLA: " ++ GameId ++ " no esta jugando esta partida";
                                                                   waiting        -> "ERROR PLA: " ++ GameId ++ " debe esperar a otro jugador";
                                                                   {turn, Player} -> "ERROR PLA: " ++ GameId ++ " es el turno de " ++ Player;
                                                                   notempty       -> "ERROR PLA: " ++ GameId ++ " el casillero " ++ Play ++ " esta ocupado";
                                                                   novalid        -> "ERROR PLA: " ++ GameId ++ " el casillero " ++ Play ++ " no es valido";
                                                                   {NewGame, Win} -> {gamesPid, Where} ! {pla, self(), Name, GameId, {NewGame, Win}},
                                                                                     receive
                                                                                       ok -> ok
                                                                                     end,
                                                                                     "OK PLA: \n" ++ print(NewGame) ++
                                                                                     if Win == win -> "Ha ganado la partida";
                                                                                        Win == tie -> "La partida termino en empate";
                                                                                        true       -> ""
                                                                                     end
                                                                 end
                             end;
    ["OBS", GameId]       -> List = gameList(),
                             case lists:keyfind(GameId, 1, List) of
                               false                         -> "ERROR OBS: " ++ GameId ++ " no existe";
                               {_, _, _, Obs, UPD, _, Where} -> case lists:member(UPDPid, Obs ++ UPD) of
                                                                  true -> "ERROR OBS: ya esta observando la partida " ++ GameId;
                                                                  _    -> {gamesPid, Where} ! {obs, self(), UPDPid, GameId},
                                                                          receive
                                                                            ok -> ok
                                                                          end,
                                                                          "OK OBS: " ++ GameId
                                                                end
                             end;
    ["LEA", GameId]       -> List = gameList(),
                             case lists:keyfind(GameId, 1, List) of
                               false                         -> "ERROR LEA: " ++ GameId ++ " no existe";
                               {_, _, _, Obs, UPD, _, Where} -> case lists:member(UPDPid, Obs ++ UPD) of
                                                                  false -> "ERROR LEA: no esta observando la partida " ++ GameId;
                                                                  _     -> {gamesPid, Where} ! {lea, self(), UPDPid, GameId},
                                                                           receive
                                                                             ok -> ok
                                                                           end,
                                                                           "OK LEA: " ++ GameId
                                                                end
                             end;
    ["BYE"]               -> List = gameList(),
                             Playing = lists:filter(fun({_, P1, P2, _, _, _, _}) -> (Name == P1) or (Name == P2) end, List),
                             Observing = lists:filter(fun({_, _, _, Obs, _, _, _}) -> lists:member(UPDPid, Obs) end, List),
                             [{gamesPid, Where} ! {bye, Name, GameId} || {GameId, _, _, _, _, _, Where} <- Playing],
                             [{gamesPid, Where} ! {lea, self(), UPDPid, GameId} || {GameId, _, _, _, _, _, Where} <- Observing],
                             UPDPid ! bye,
                             "OK BYE";
    ["HELP"]              -> help();
    _                     -> "ERROR: comando no valido"
  end.
