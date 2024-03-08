-module(load).
-export([pstat/0, pbalance_monitor/0]).
-define(PSTAT_TIMER, 2000).
-define(INITIAL_LOAD, 0).

%% Función encargada de enviar, a intervalos regulares, la carga del nodo actual
%% a los demás nodos
pstat() ->
  {_, Load} = erlang:statistics(reductions),
  [{pbalancePid, Node} ! {node(), Load} || Node <- [node() | nodes()]],
  timer:sleep(?PSTAT_TIMER),
  pstat().

%% Es invocada antes de llamar a pbalance, que activa el monitoreo de nodos.
%% Con esto, cuando se conectao desconecta un nodo, el proceso recibe un mensaje
%% indicando esta situación
pbalance_monitor() ->
  net_kernel:monitor_nodes(true),
  pbalance([{node(), ?INITIAL_LOAD}]).

%% Lleva una lista con las cargas de los nodos, las cuales son enviadas por pstat,
%% y le indica al psocket que lo solicite cuál es el de menor carga. Además, si 
%% un nodo nuevo se conecta (o se cae) se encarga de agregarlo (o eliminarlo)
pbalance(LoadList) ->
  receive
    {psocket, Pid}   -> {Node, _} = hd(lists:keysort(2, LoadList)),
                        Pid ! Node,
                        pbalance(LoadList);
    {nodeup, Node}   -> pbalance([{Node, ?INITIAL_LOAD} | LoadList]);
    {nodedown, Node} -> pbalance(lists:keydelete(Node, 1, LoadList));
    {Node, Load}     -> pbalance(lists:keystore(Node, 1, LoadList, {Node, Load}))
  end.
