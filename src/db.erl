-module(db).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([
         start/0,
         select/1,
         insert/1,
         update/1,
         delete/1,
         select_sql/3,
         delete_sql/2,
         insert_sql/3,
         update_sql/3,
         transaction/1
         ]).
-include("game.hrl").
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
transaction(Fun) ->
    case mysql:transaction(pool, Fun) of
        {atomic, Result} ->{ok, Result};
        Error ->
            ?DEBUG("~p:transaction ~p Error=~p", [?MODULE, ?LINE, Error]),
            {error, Error}
    end.

-spec insert(Sql::string()) -> {ok, integer()} |term().
insert(Sql) ->
    case mysql:fetch(pool, Sql) of
        {updated,{mysql_result,[],[],Num, Sum,[]}} ->
            {ok, Num, Sum};
        Error ->
            ?DEBUG("~p:insert ~p Error=~p", [?MODULE, ?LINE, Error]),
            {error, Error}
    end.

-spec update(Sql::string()) -> {ok, integer()} |term().
update(Sql) ->
    case mysql:fetch(pool, Sql) of
        {updated,{mysql_result,[],[],Num,0,[]}} ->
            {ok, Num};
        Error ->
            ?DEBUG("~p:update ~p Error=~p", [?MODULE, ?LINE, Error]),
            {error, Error}
    end.
    

-spec delete(Sql::string()) -> {ok, integer()}|term().
delete(Sql) ->
    case mysql:fetch(pool, Sql) of
        {updated, {mysql_result, [], [], Num, 0, []}} ->
            {ok, Num};
        Error ->
            ?DEBUG("~p:delete ~p Error=~p", [?MODULE, ?LINE, Error]),
            {error, Error}
    end.

-spec select(Sql::string()) -> list()|term().
select(Sql) ->
    case mysql:fetch(pool, Sql) of
        {data, {mysql_result,Col, Rows,_,_,_}} ->
            get_proplist(Col, Rows); 
        Error ->
            ?DEBUG("~p:select ~p Error=~p", [?MODULE, ?LINE, Error]),
            {error, Error}
    end.

start() ->
    create_pool().

create_pool() ->
    %% Start the MySQL dispatcher and create the first connection
    %% to the database. 'pool' is the connection pool identifier.
    mysql:start_link(pool, "localhost", "root", "root", "game"),
    %% Add 2 more connections to the connection pool
    mysql:connect(pool, "localhost", undefined, "root", "root", "game",
                  true),
    mysql:connect(pool, "localhost", undefined, "root", "root", "game",
                  true).
    

get_proplist(Columns, Rows) ->
    NewColumns = [binary_to_list(element(2,E))||E<-Columns],
    F = fun(E, {Fields, Len}=S) ->     
            {get_field(list_to_tuple(E), Fields, Len, []), S}  
        end, 
    {R, _} = lists:mapfoldl(F, {list_to_tuple(NewColumns), length(NewColumns)}, Rows),
    R.  

get_field(_E, _Fields, 0, R) ->
    R;  
get_field(E, Fields, Len, R) ->
    Fun = fun(Field, {Y, {H, M, S}}) ->
                    {Field, utils:get_datetime({Y, {H, M, trunc(S)}})}; 
             (Field, Value) ->
                    {Field, Value}
          end,
    get_field(E, Fields, Len-1, [Fun(element(Len, Fields), element(Len, E))|R]). 

update_sql(Table, Param, "") ->              
    lists:concat(["update ", Table, " set ", lists:concat(Param), " ;"]);
update_sql(Table, Param, Where) ->              
    lists:concat(["update ", Table, " set ", lists:concat(Param), 
                  " where ", lists:concat(Where), " ;"]).

select_sql(Table, Param, "") ->
    lists:concat(["select ", lists:concat(Param), " from ", Table, " ;"]);
select_sql(Table, Param, Where) ->
    lists:concat(["select ", lists:concat(Param), " from ", Table, " where ", lists:concat(Where) ," ;"]).

insert_sql(Table, Param, Value) ->
    lists:concat(["insert into  ", Table, " (", 
                  lists:concat(Param),
                  ") values(",
                  lists:concat(Value),
                  ");"]).

delete_sql(Table, "") ->              
    lists:concat(["delete from ", Table]);
delete_sql(Table, Where) ->              
    lists:concat(["delete from ", Table, " where ", lists:concat(Where)]).



