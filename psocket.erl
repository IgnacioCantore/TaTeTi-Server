-module(psocket).
-export([psocket/4, update/1]).
-import(pcomando, [pcomando/5]).
-import(aux, [print/1]).
-define(PCOM_TIMEOUT, 5000).

%% Función encargada de recibir pedidos del cliente, y de generar un nuevo proceso
%% que se encargue de atenderlos
psocket(Socket, UPDPid, Name, Count) ->
  case gen_tcp:recv(Socket, 0) of
    {ok, BinCMD} -> CMD = string:tokens(string:trim(binary_to_list(BinCMD)), " "),
                    pbalancePid ! {psocket, self()},
                    receive
                      Node -> io:format("Ejecutando en nodo: ~p~n", [Node]),
                              Ans = rpc:call(Node, pcomando, pcomando, [self(), CMD, UPDPid, Name, Count], ?PCOM_TIMEOUT)
                    end,
                    case Ans of
                      {badrpc, _} -> Msg = "ERROR: servidor ocupado, intente nuevamente\n";
                      _           -> Msg = Ans
                    end,
                    gen_tcp:send(Socket, list_to_binary(Msg ++ "\n")),
                    case Msg of
                      "OK CON: " ++ NewName -> psocket(Socket, UPDPid, NewName, 1);
                      "OK NEW" ++ _         -> psocket(Socket, UPDPid, Name, Count + 1);
                      "OK BYE"              -> rpc:call(Node, pcomando, pcomando, [self(), ["BYE"], UPDPid, Name, Count], ?PCOM_TIMEOUT),
                                               gen_tcp:close(Socket);  % Al "morir" el proceso, el nombre registrado globalmente se libera
                      _                     -> psocket(Socket, UPDPid, Name, Count)
                    end;
    {error, _}   -> pbalancePid ! {psocket, self()},
                    receive
                      Node -> rpc:call(Node, pcomando, pcomando, [self(), ["BYE"], UPDPid, Name, Count], ?PCOM_TIMEOUT)
                    end,
                    io:format("El cliente ~p cerró la conexión~n", [Socket]),
                    gen_tcp:close(Socket)  % Al "morir" el proceso, el nombre registrado globalmente se libera
  end.

%% Recibe actualizaciones de las partidas que esté jugando u observando, y las
%% envía al cliente
update(Socket) ->
  receive
    {acc, Name, GameId}       -> Msg = "UPD: " ++ Name ++ " se unio a la partida " ++ GameId ++ "\n";
    {pla, Name, GameId, Game} -> Print = print(Game),
                                 Msg = "UPD: " ++ Name ++ " realizo una jugada en la partida " ++ GameId ++ "\n" ++ Print;
    {win, Name, GameId}       -> Msg = "UPD: " ++ Name ++ " gano la partida " ++ GameId ++ "\n";
    {tie, _, GameId}          -> Msg = "UPD: la partida " ++ GameId ++ " termino en empate\n";
    {bye, Name, GameId}       -> Msg = "UPD: la partida " ++ GameId ++ " finalizo porque " ++ Name ++ " se desconecto\n";
    bye                       -> Msg = bye
  end,
  if Msg == bye -> ok;
     true       -> gen_tcp:send(Socket, list_to_binary(Msg)),
                   update(Socket)
  end.
