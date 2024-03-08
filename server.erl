-module(server).
-export([start/1, dispatcher/1, connect/1]).
-import(game, [games/1]).
-import(load, [pstat/0, pbalance_monitor/0]).

%% Función de inicio del servidor
start(Port) ->
  case gen_tcp:listen(Port, [binary, {active, false}]) of
    {ok, LSocket} -> register(gamesPid, spawn(game, games, [[]])),
                     register(pbalancePid, spawn(load, pbalance_monitor, [])),
                     spawn(load, pstat, []),
                     spawn(server, dispatcher, [LSocket]);
    {error, Msg}  -> io:format("Error al crear socket: ~p~n", [Msg])
  end,
  ok.

%% Acepta la conexión de un cliente y crea un nuevo socket para que puedan comunicarse.
%% Además, lanza un nuevo proceso "psocket" que atiende los pedidos del cliente, y un
%% proceso "update" que envía las actualizaciones de las partidas a través del socket
dispatcher(LSocket) ->
  case gen_tcp:accept(LSocket) of
    {ok, Socket} -> UPDPid = spawn(psocket, update, [Socket]),
                       spawn(psocket, psocket, [Socket, UPDPid, false, 1]),
                       io:format("Nuevo cliente: ~p~n", [Socket]),
                       dispatcher(LSocket);
    {error, Msg} -> io:format("Error en LSocket: ~p~n", [Msg])
  end.

%% Permite conectarse al nodo pasado como argumento
connect(Node) ->
  case net_kernel:connect_node(Node) of
    true  -> io:format("Conectado con ");
    false -> io:format("No se pudo conectar con ")
  end,
  io:format("~p~n", [Node]).
