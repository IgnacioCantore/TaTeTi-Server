-module(game).
-export([games/1, getGames/0, play/2]).
-import(aux, [whosTurn/1, winner/1]).
-define(NEWGAME, [" "," "," "," "," "," "," "," "," "]).
-define(GAMES_TIMEOUT, 2000).

%% Contiene en la lista Local todas las partidas creadas en este nodo
%% Recibe pedidos de "pcomando" y le envía la información solicitada
games(Local) ->
  receive
    {lsg, PCom}                            -> List = rpc:call(node(), game, getGames, [], ?GAMES_TIMEOUT),  % No uso ! para que no se "mezcle" con pedidos de pcomando
                                              case List of
                                                {badrpc, _} -> Games = Local;  % Si hubo algún error, retorna sólo las partidas de este nodo
                                                _           -> Games = List ++ Local
                                              end,
                                              Ordered = lists:reverse(Games),
                                              PCom ! Ordered,
                                              games(Local);
    {new, PCom, UPDPid, Name, Count}       -> Id = Name ++ erlang:integer_to_list(Count),
                                              PCom ! Id,
                                              NewGame = {Id, Name, esperando, [], [UPDPid], ?NEWGAME, node()},
                                              games([NewGame | Local]);
    {acc, PCom, UPDPid, Name, GameId}      -> case lists:keyfind(GameId, 1, Local) of
                                                {_, P1, esperando, Obs, UPD, Game, _} -> PCom ! ok,
                                                                                         [Update ! {acc, Name, GameId} || Update <- UPD ++ Obs],  % Avisa al oponente y los observadores que se unió alguien
                                                                                         NewGame = {GameId, P1, Name, Obs, [UPDPid | UPD], Game, node()},
                                                                                         games(lists:keyreplace(GameId, 1, Local, NewGame));
                                                _                                     -> PCom ! full,  % El usuario intentó acceder a una partida llena
                                                                                         games(Local)
                                              end;
    {pla, PCom, Name, GameId, {Game, Win}} -> {_, P1, P2, Obs, UPD, _, _} = lists:keyfind(GameId, 1, Local),
                                              PCom ! ok,
                                              if Name == P1 -> Player = 1;
                                                 true       -> Player = 2
                                              end,
                                              [Update ! {pla, Name, GameId, Game} || Update <- [lists:nth(Player, UPD) | Obs]],  % Avisa al oponente y los observadores que se hizo una jugada
                                              case Win of
                                                no -> NewGame = {GameId, P1, P2, Obs, UPD, Game, node()},
                                                      games(lists:keyreplace(GameId, 1, Local, NewGame));
                                                _  -> [Update ! {Win, Name, GameId} || Update <- [lists:nth(Player, UPD) | Obs]],  % Avisa al oponente y los observadores el resultado final
                                                      games(lists:keydelete(GameId, 1, Local))
                                              end;
    {obs, PCom, UPDPid, GameId}            -> {_, P1, P2, Obs, UPD, Game, _} = lists:keyfind(GameId, 1, Local),
                                              PCom ! ok,
                                              NewGame = {GameId, P1, P2, [UPDPid | Obs], UPD, Game, node()},
                                              games(lists:keyreplace(GameId, 1, Local, NewGame));
    {lea, PCom, UPDPid, GameId}            -> {_, P1, P2, Obs, UPD, Game, _} = lists:keyfind(GameId, 1, Local),
                                              PCom ! ok,
                                              NewObs = lists:delete(UPDPid, Obs),
                                              NewGame = {GameId, P1, P2, NewObs, UPD, Game, node()},
                                              games(lists:keyreplace(GameId, 1, Local, NewGame));
    {bye, Name, GameId}                    -> {_, P1, _, Obs, UPD, _, _} = lists:keyfind(GameId, 1, Local),
                                              if Name == P1 -> [Update ! {bye, Name, GameId} || Update <- [hd(UPD) | Obs]];  % Avisa al oponente y los observadores que el jugador abandonó la partida
                                                 true       -> [Update ! {bye, Name, GameId} || Update <- [lists:nth(2, UPD) | Obs]]
                                              end,
                                              games(lists:keydelete(GameId, 1, Local));
    {games, Pid}                           -> Pid ! Local,  % Usada por la función "getGames" para solicitar la lista de partidas
                                              games(Local);
    _                                      -> games(Local)  % En caso que quede algún mensaje por el monitoreo de nodos
  end.

%% Es llamada por "games",y  devuelve una lista con las partidas de los demás
%% nodos. Monitorea los nodos porque si se conecta un nuevo nodo, debe agregar
%% un "receive" para que no se pierda la lista de partidas de un nodo; y si un
%% nodo se cae, se puede omitir y la función no queda bloqueada en el "receive"
getGames() ->
  Nodes = length([{gamesPid, Node} ! {games, self()} || Node <- nodes()]),
  net_kernel:monitor_nodes(true),
  List = getGamesAux(Nodes, []),
  net_kernel:monitor_nodes(false),
  List.

%% Función recursiva auxiliar que retorna la lista de partidas del resto
%% de los nodos
getGamesAux(0, Ls) -> Ls;
getGamesAux(N, Ls) ->
  receive
    {nodeup, _}   -> getGamesAux(N, Ls);
    {nodedown, _} -> getGamesAux(N-1, Ls);
    List          -> getGamesAux(N-1, Ls ++ List)
  end.

%% Intenta realizar la jugada "Play" en "Game", verificando que quien intenta
%% hacerla sea el jugador que tiene el turno, devolviendo el motivo por el que
%% no se pudo hacer la jugada, o bien una tupla con el nuevo estado de la partida,
%% y si la partida continúa, terminó en empate o hay un ganador
play(Name, {P1, P2, Game, Play}) ->
  if (Play < 1) or (Play > 9) -> novalid;
     true                     -> case Name of
                                   P1 -> if P2 == esperando -> waiting;
                                            true            -> case whosTurn(Game) of
                                                                 1 -> case lists:nth(Play, Game) of
                                                                        " " -> {First, [_ | Last]} = lists:split(Play - 1, Game),
                                                                               NewGame = First ++ ["X" | Last],
                                                                               case winner(NewGame) of
                                                                                 no  -> {NewGame, no};
                                                                                 win -> {NewGame, win};
                                                                                 tie -> {NewGame, tie}
                                                                               end;
                                                                        _   -> notempty
                                                                      end;
                                                                 _ -> {turn, P2}
                                                               end
                                         end;
                                   P2 -> case whosTurn(Game) of
                                           2 -> case lists:nth(Play, Game) of
                                                  " " -> {First, [_ | Last]} = lists:split(Play - 1, Game),
                                                         NewGame = First ++ ["O" | Last],
                                                         case winner(NewGame) of
                                                           no  -> {NewGame, no};
                                                           win -> {NewGame, win};
                                                           tie -> {NewGame, tie}
                                                         end;
                                                  _   -> notempty
                                                end;
                                           _ -> {turn, P1}
                                         end;
                                   _  -> noplayer
                                 end
  end.
