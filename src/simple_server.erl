-module(simple_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("game.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([create_city/2,
         get_city/2,
         change_tax/3,
         change_capital/2,
         collect_taxes/1,
         product_food/1,
         is_empty/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec create_city({X::integer(), Y::integer()}, AuthorId::string()) -> 
        ok|{error, Reason::term()}.
create_city({X, Y}, AuthorId) ->
    ?DEBUG("~p:create_city ~p {X, Y}=~p, AuthorId=~p", [?MODULE, ?LINE, {X, Y}, AuthorId]),
    case is_empty({X, Y}, AuthorId) of
        "1" ->
            Fun=fun() ->
                    InsertMap = db:insert_sql("map", 
                                              ['x,', 'y,', 'author_id'],
                                              ["'", X, "','", Y, "','", AuthorId, "'"]),
                    db:insert(InsertMap),
                    Fields = ['x,', 'y,', 'author_id'],
                    Values = ["'", X, "','", Y, "','", AuthorId, "'"],
                    InsertCity = db:insert_sql("city", Fields, Values),
                    db:insert(InsertCity)
                  end,
            db:transaction(Fun),
            ok;
        "0" ->
            {error, "is exists"}
    end.

-spec is_empty({X::integer(), Y::integer()}, AuthorId::string()) ->  IsEmpty::string().
is_empty({X, Y}, AuthorId) ->
    Sql = db:select_sql("map", 
                        ["id,", "x,", "y,", "author_id"], 
                        ["x='", X, "' ",
                         "and y='", Y, "' ",
                         "and author_id='", AuthorId, "'"]),
    ?DEBUG("~p:is_empty ~p Sql=~p", [?MODULE, ?LINE, Sql]),
    case db:select(Sql) of
        [] -> 
            ?DEBUG("~p:is_empty ~p IsEmpty=~p", [?MODULE, ?LINE, "1"]),
            "1";
        [_Map] -> 
            ?DEBUG("~p:is_empty ~p _Map=~p ", [?MODULE, ?LINE, _Map]),
            "0"
    end.

-spec get_city({X::integer(), Y::integer()}, AuthorId::string()) -> 
            city() |{error, Reason::string()}.
get_city({X, Y}, AuthorId) ->
    case is_empty({X, Y}, AuthorId) of
        "1" -> {error, "not exits"};
        "0" -> 
            Fields = ["x, y, foods, golds, gold_tax, people, soldiers, name, is_captial"],
            Where = ["x='", X, "' and ",
                     "y='", Y, "' and ",
                     "author_id='", AuthorId, "'"],
            Sql = db:select_sql("city", Fields, Where),
            case db:select(Sql) of
                Cities when is_list(Cities) ->
                    ?DEBUG("~p:get_city ~p Cites=~p", [?MODULE, ?LINE, Cities]),
                    Cities;
                Error ->
                    ?DEBUG("~p:get_city ~p Error=~p", [?MODULE, ?LINE, Error])
            end
    end.

-spec change_tax({X::integer(), Y::integer()}, AuthorId::string(), Tax::float()) -> 
            ok|{error, Reason::string()}.
change_tax({X, Y}, AuthorId, Tax) ->
    Where = ["x='", X, "' and ",
                     "y='", Y, "' and ",
                     "author_id='", AuthorId, "'"],
    Sql = db:update_sql("city", [" gold_tax='", Tax, "'"], Where),
    case db:update(Sql) of
        {ok, 0} ->
            ?ERROR("~p:change_tax ~p noexists", [?MODULE, ?LINE]),
            {error, "the city is noexists"};
        {ok, _Num} ->
            ok;
        {error, Error} ->
            ?ERROR("~p:change_tax ~p Error=~p", [?MODULE, ?LINE, Error])
    end.

-spec change_capital({X::integer(), Y::integer()}, AuthorId::string()) -> 
            ok|{error, Reason::string()}.
change_capital({X, Y}, AuthorId) ->
    FunSetCapital = 
        fun() -> 
            Where = ["x='", X, "' and ",
                     "y='", Y, "' and ",
                     "author_id='", AuthorId, "'"],
            Set = [" is_captial='", "1", "',",
                   " product_food_rate='", 10000/60/60,"'"],
            Sql = db:update_sql("city", Set, Where),
            case db:update(Sql) of
                {ok, 0} ->
                    ?ERROR("~p:change_capital ~p noexists", [?MODULE, ?LINE]),
                    {error, "the city is noexists"};
                {ok, _Num} ->
                    ok;
                {error, Error} ->
                    ?ERROR("~p:change_capital ~p Error=~p ", [?MODULE, ?LINE, Error])
            end
    end,
    Fun = fun() ->
            Where = ["x='", X, "' and ",
                     "y='", Y, "' and ",
                     "is_captial='", "1", "' and ",
                     "author_id='", AuthorId, "'"],
            Set = [" is_captial='", "0", "',",
                   " product_food_rate='", 1000/60/60,"'"],
            Sql = db:update_sql("city", Set, Where),
            case db:update(Sql) of
                {ok, _Num} ->
                    FunSetCapital(),
                    ok;
                {error, Error} ->
                    ?ERROR("~p:change_capital ~p Error=~p ", [?MODULE, ?LINE, Error])
            end
          end,
     case db:transaction(Fun) of
        {ok, _R} -> ok;
        {error, Error} ->
            ?ERROR("~p:change_capital ~p Error=~p ", [?MODULE, ?LINE, Error])
     end.



           
    

collect_taxes(Time) ->
    ?DEBUG("~p:collect_taxes ~p Time=~p", [?MODULE, ?LINE, Time]),
    receive
    after Time ->
        collect_taxes(),
        collect_taxes(Time)
    end.
    
product_food(Time) ->
    ?DEBUG("~p:product_food ~p Time=~p", [?MODULE, ?LINE, Time]),
    receive 
    after Time ->
        product_food(),
        ?DEBUG("~p:product_food ~p Time=~p", [?MODULE, ?LINE, Time]),
        product_food(Time) 
    end.


    
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    process_flag(trap_exit, true),
    Dict = dict:new(),
    gen_server:cast(?SERVER, {product_food}),
    gen_server:cast(?SERVER, {collect_taxes}),
    gen_server:cast(?SERVER, {consumer_food}),
    {ok, Dict}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast({consumer_food}, State) ->
    Pid = spawn_link(train_soldier, consumer_food, [?CONSUMER_FOOD_TIME]),
    NewState = dict:store(Pid, consumer_food, State),
    {noreply, NewState};
handle_cast({product_food}, State) ->
    Pid = spawn_link(?MODULE, product_food, [?PRODUCT_FOOD_TIME]),
    NewState = dict:store(Pid, product_food, State),
    {noreply, NewState};

handle_cast({collect_taxes}, State) ->
    Pid = spawn_link(?MODULE, collect_taxes, [?COLLECT_TAXES_TIME]),
    NewState = dict:store(Pid, collect_taxes, State),
    {noreply, NewState}.

handle_info({'EXIT', From, Reason}, State) ->
    ?DEBUG("~p:handel_info ~p Reason=~p", [?MODULE, ?LINE, Reason]),
    case dict:fetch(From, State) of
        product_food ->
            Pid = spawn_link(?MODULE, product_food, [?PRODUCT_FOOD_TIME]),
            NewState = dict:store(Pid, product_food, dict:erase(From, State));
        collect_taxes ->
            Pid = spawn_link(?MODULE, collect_taxes, [?COLLECT_TAXES_TIME]),
            NewState = dict:store(Pid, collect_taxes, dict:erase(From, State));
        consumer_food ->
            Pid = spawn_link(train_soldier, consumer_food, [?CONSUMER_FOOD_TIME/100]),
            NewState = dict:store(Pid, consumer_food, State)
            
    end,
    {noreply, NewState};

handle_info(_Info, State) ->
    ?DEBUG("~p:handel_info ~p Reason=~p", [?MODULE, ?LINE, _Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec get_max_people(Num::integer()) ->integer().
get_max_people(Num) when Num < 1000 ->
    Num;
get_max_people(_Num) ->
    1000.

-spec get_golds(Golds::float(), Rate::float()) -> float().
get_golds(Golds, Rate) ->
    get_golds(Golds - Golds*Rate).

-spec get_golds(Golds::float()) -> float().
get_golds(Golds) when Golds < 0 ->
    0.0;
get_golds(Golds) ->
    Golds.



collect_taxes() ->
    ?DEBUG("~p:collect_taxes ~p", [?MODULE, ?LINE]),
    Tag = utils:get_id(),
    AddSql = "update city set people= people*1.05, "++
             "golds=golds-golds*gold_tax, tax_tag='"++Tag++"'  where people<gold_tax*1000;",
    ReduceSql = "update city set people= people*0.95, golds=golds-golds*gold_tax "++
                " where people>gold_tax*1000 and tax_tag<>'"++Tag++ "';",

    ReduceSoldierSql = "update soldier s set s.soldier_sum=s.soldier_sum-(s.soldier_sum*0.1)"++
                       " where s.x in (select x from city where is_food_crisis='1' )"++
                       " and s.y in(select y from city where is_food_crisis='1');",

    CrisisSql = "update city set is_food_crisis='0' where is_food_crisis='1'",
    Fun = fun()->
            db:update(AddSql),
            db:update(ReduceSql),
            db:update(ReduceSoldierSql),
            db:update(CrisisSql)
          end,
    case db:transaction(Fun) of
        {ok, _R} -> ok;
        {error, Error} ->
            ?ERROR("~p:collect_taxes ~p Error=~p ", [?MODULE, ?LINE, Error])
    end.

product_food() ->
    Sql = db:update_sql("city", ["foods=foods+product_food_rate"], ""),
    case db:update(Sql) of
        {ok, _Num} ->
            ok;
        {error, Error} ->
            ?ERROR("~p:product_food ~p Error=~p ", [?MODULE, ?LINE, Error])
            
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(TEST).

init_test1() ->
    application:start(game),
    [?assertMatch(ok, ok)
    ].
create_city_test1() ->
    application:start(game),
    X = 1, Y=1, AuthorId = "langxw",
    EmptyList = is_empty({X, Y}, AuthorId),
    Success = create_city({X, Y}, AuthorId),
    create_city({0, 0}, AuthorId),
    NotEmpty = is_empty({X, Y}, AuthorId),
    {Failure, _} = create_city({X, Y}, AuthorId),
    City = get_city({X, Y}, AuthorId),
    MatchCity = [[{"x", 1}, 
                  {"y", 1}, 
                  {"foods", 0.0},
                  {"golds", 0.0}, 
                  {"gold_tax", 0.2}, 
                  {"people", 100}, 
                  {"soldiers", 0}, 
                  {"name", <<>>},
                  {"is_captial", <<"0">>}]],

    MatchCityTax = [[{"x", 1}, 
                     {"y", 1}, 
                     {"foods", 0.0},
                     {"golds", 0.0}, 
                     {"gold_tax", 0.1}, 
                     {"people", 100}, 
                     {"soldiers", 0}, 
                     {"name", <<>>},
                     {"is_captial", <<"0">>}]],
    MatchCapital = [[{"x", 1}, 
                     {"y", 1}, 
                     {"foods", 0.0},
                     {"golds", 0.0}, 
                     {"gold_tax", 0.1}, 
                     {"people", 100}, 
                     {"soldiers", 0}, 
                     {"name", <<>>},
                     {"is_captial", <<"1">>}]],
    MatchCapitalTime = [[{"x", 1}, 
                     {"y", 1}, 
                     {"foods", 2.78},
                     {"golds", 0.0}, 
                     {"gold_tax", 0.1}, 
                     {"people", 100}, 
                     {"soldiers", 0}, 
                     {"name", <<>>},
                     {"is_captial", <<"1">>}]],

    ChangeTax = change_tax({X, Y}, AuthorId, 0.1),
    {ChangeTaxError, _} = change_tax({X, 2}, AuthorId, 0.1),
    CityTax = get_city({X, Y}, AuthorId),
    change_capital({X, Y}, AuthorId),
    Capital = get_city({X, Y}, AuthorId),
    receive after 1000 -> ok end,
    CapitalTime = get_city({X, Y}, AuthorId),
 %   ?DEBUG("~p MatchCapitalTime=~p", [?LINE, MatchCapitalTime]),
 %   ?DEBUG("~p CapitalTime=~p", [?LINE, CapitalTime]),
    [
     %?assertMatch(ok, Init),
     ?assertMatch("1", EmptyList),
     ?assertMatch(ok, Success),
     ?assertMatch("0", NotEmpty),
     ?assertMatch(error, Failure),
     ?assertMatch(MatchCity, City),
     ?assertMatch(ok, ChangeTax),
     ?assertMatch(error, ChangeTaxError),
     ?assertMatch(MatchCityTax, CityTax),
     ?assertMatch(MatchCapital, Capital),
     ?assertMatch(MatchCapitalTime, CapitalTime),
     ?assert(1==1)
    ].
collect_taxes_test1() ->
    application:start(game),
    X2 = 2, Y2=2, AuthorId = "langxw",
    X1 = 1, Y1=1, AuthorId = "langxw",
    create_city({X2, Y2}, AuthorId),
    create_city({X1, Y1}, AuthorId),
    db:update("update city set golds=100, people=1000 where author_id='langxw';"),
    change_tax({X2, Y2}, AuthorId, 0.1),
    [collect_taxes() ||_K<-lists:seq(1,1)],

    MatchCity22 = [[{"x", 2}, 
                    {"y", 2}, 
                    {"foods", 0.0},
                    {"golds", 90.0}, 
                    {"gold_tax", 0.1}, 
                    {"people", 950}, 
                    {"soldiers", 0}, 
                    {"name", <<>>},
                    {"is_captial", <<"0">>}]],
    CityTax22 = get_city({X2, Y2}, AuthorId),
    [
     ?assertMatch(MatchCity22, CityTax22)
    ].
respawn_test1() ->
    receive after 2000 -> ok end,
    [].

-endif.

