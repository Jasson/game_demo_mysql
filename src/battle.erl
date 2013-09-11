-module(battle).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("game.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-define(SPEND_TIME,1).
%-define(SPEND_TIME,1000).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([
         to_battle/5,
         send_soldier/5
         ]).

-record(state, {pids = dict:new(), 
                cities_id =dict:new(),
                max=5}).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec send_soldier(From::city_id(), To::city_id(), 
                   PikeSoldier::{soldier_type(), integer()}, 
                   ArcherSoldier::{soldier_type(), integer()}, 
                   Cavalry::{soldier_type(), integer()}) ->
              ok|{error, term()}.
send_soldier(From, To, PikeSoldier, ArcherSoldier, Cavalry) ->
    Request = 
        {send_soldier, From, To, PikeSoldier, ArcherSoldier, Cavalry},
    gen_server:call(?SERVER, Request).
    

-spec to_battle(FromCityId::city_id(), ToCityId::city_id(),
                PikeSoldier::{soldier_type(), integer()}, 
                ArcherSoldier::{soldier_type(), integer()}, 
                Cavalry::{soldier_type(), integer()}) ->
        ok|{error, term()}.
to_battle(FromCityId, ToCityId, PikeSoldier, ArcherSoldier, Cavalry) ->
    Distance = get_distance(FromCityId, ToCityId),
    SpendTime = get_spend_time(Distance, PikeSoldier, ArcherSoldier, Cavalry),
    ?DEBUG("~p:to_battle ~p SpendTime=~p", [?MODULE, ?LINE, SpendTime]),
    receive 
        cancle_battle ->
            %%TODO
            ok
    after SpendTime ->
        join_battle(FromCityId, from),
        join_battle(ToCityId, to),
        [kill_soldier(FromCityId, ToCityId) ||_N<-lists:seq(1,3)],
        go_back(ToCityId, "3", "6"),
        go_back(FromCityId, "5", "6"),
        receive 
        after 0 -> %% TODO 
            complete(self(), FromCityId),
            go_back(FromCityId, "3", "5"),
            ok
        end
    end.
    
complete(Pid, CityId) ->
    gen_server:cast(?SERVER, {complete, Pid, CityId}).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    ?DEBUG("~p:init ~p ", [?MODULE, ?LINE]),
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({send_soldier, FromCity, ToCity, PikeSoldier, 
             ArcherSoldier, Cavalry}, _From, 
             #state{max=Max, pids=Pids, cities_id=CitiesId}=State) ->
    ?DEBUG("~p:handle_call ~p send_soldier state=~p", [?MODULE, ?LINE, State]),
    case is_may_battle(FromCity, PikeSoldier, ArcherSoldier, Cavalry, CitiesId, Max) of
        ok ->
            Args = [FromCity, ToCity, PikeSoldier, ArcherSoldier, Cavalry],
            Pid = spawn_link(?MODULE, to_battle, Args),
            NewPids = dict:store(Pid, {send_soldier, Args}, Pids),
            NewCitiesId = dict:update_counter(FromCity, 1, CitiesId),
            NewState = State#state{pids=NewPids, cities_id=NewCitiesId};
        {error, _Error} ->
            NewState = State
        end,
    {reply, ok, NewState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({complete, Pid, CityId}, #state{pids=Pids, cities_id=CitiesId}=State) ->
    NewPids = dict:erase(Pid, Pids),
    NewCitiesId = dict:update_counter(CityId, -1, CitiesId),
    NewState = State#state{pids=NewPids, cities_id=NewCitiesId},
    {noreply, NewState};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'EXIT', _From, normal}, State) ->
    ?DEBUG("~p:handel_info ~p Reason=~p", [?MODULE, ?LINE, normal]),
    {noreply, State};
handle_info({'EXIT', From, Reason}, #state{pids=Pids}=State) ->
    ?DEBUG("~p:handel_info ~p Reason=~p", [?MODULE, ?LINE, Reason]),
    {send_soldier, Args} = dict:fetch(From, Pids),
    Pid = spawn_link(?MODULE, to_battle, Args),
    NewPids = dict:store(Pid, {send_soldier, Args}, 
                         dict:erase(From, Pids)),
    NewState = State#state{pids=NewPids},
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

-spec is_may_battle(CityId::city_id(), 
                    PikeSoldier::{string(), integer()},
                    ArcherSoldier::{string(), integer()},
                    Cavalry::{string(), integer()},
                    CitiesId::dict(),
                    Max::integer()) -> ok |{error, term()}.
    
is_may_battle(CityId, PikeSoldier, ArcherSoldier, Cavalry, CitiesId, Max) ->
    case is_empty(CityId, CitiesId, Max) of
        ok ->
            is_enough_soldier_to_battle(CityId, PikeSoldier, ArcherSoldier, Cavalry);
        Error ->
            ?DEBUG("~p:is_may_battle ~p Error=~p", [?MODULE, ?LINE, Error]),
            Error
    end.
    
-spec is_enough_soldier_to_battle(
            CityId::city_id(),
            PikeSoldier::{string(),integer()}, 
            ArcherSoldier::{string(),integer()}, 
            Cavalry::{string(),integer()})-> ok |{error, term()}.
is_enough_soldier_to_battle(CityId, PikeSoldier, ArcherSoldier, Cavalry) ->
    case {get_soldier(CityId, PikeSoldier), 
          get_soldier(CityId, ArcherSoldier),
          get_soldier(CityId, Cavalry)} of
          {ok, ok, ok} ->
             ok;
          Error ->
             ?DEBUG("~p:is_enough_soldier_to_battle ~p Error=~p", [?MODULE, ?LINE, Error]),
             {error, Error}
    end.
-spec get_soldier(CityId::city_id(), 
                  Soldier::{soldier_type(), integer()}) -> 
            ok|{error, term()}.

get_soldier(_CityId, {_Type, 0}) ->
    ok;
get_soldier({{X,Y}, AuthorId}=CityId, {Type, Num}) ->
    Where = ["x='", X, "' and ",
             "y='", Y, "' and ",
             "type='", Type, "' and ",
             "state='", "3", "' and ",
             "author_id='", AuthorId, "'"],
    Sql = db:select_sql("soldier", ["soldier_sum"], Where),
    case db:select(Sql) of
        [] -> {error, "not enough "++Type};
        [[{"soldier_sum", Sum}]] when Num=<Sum ->
            to_forward(CityId, Type, Num),
            ok;
        [[{"soldier_sum", Sum}]] when Num>Sum ->
            {error, "not enough "++Type};
        Error ->
            {error, Error}
    end.

-spec to_forward(CityId::city_id(), Type::string(), 
            Num::integer()) ->ok.
to_forward(CityId, Type, Num) ->
    ?DEBUG("~p:to_forward ~p Num=~p", [?MODULE, ?LINE, Num]),
    train_soldier:add_soldier(CityId, Num, Type, "4"),
    train_soldier:add_soldier(CityId, -Num, Type, "3").

-spec join_battle(CityId::city_id(), Type::atom()) -> ok|{error, term()}.
join_battle(CityId, from) ->
    PikeNum = get_forward_num("1", CityId, "4"),
    ArcherNum = get_forward_num("2", CityId, "4"),
    CavalryNum =get_forward_num("3", CityId, "4"),

    train_soldier:add_soldier(CityId, PikeNum, "1", "6"),
    train_soldier:add_soldier(CityId, -PikeNum, "1", "4"),

    train_soldier:add_soldier(CityId, ArcherNum, "2", "6"),
    train_soldier:add_soldier(CityId, -ArcherNum, "2", "4"),

    train_soldier:add_soldier(CityId, CavalryNum, "3", "6"),
    train_soldier:add_soldier(CityId, -CavalryNum, "3", "4");
join_battle(CityId, to) ->
    PikeNum = get_forward_num("1", CityId, "3"),
    ArcherNum = get_forward_num("2", CityId, "3"),
    CavalryNum =get_forward_num("3", CityId, "3"),
    train_soldier:add_soldier(CityId, PikeNum, "1", "6"),
    train_soldier:add_soldier(CityId, -PikeNum, "1", "3"),

    train_soldier:add_soldier(CityId, ArcherNum, "2", "6"),
    train_soldier:add_soldier(CityId, -ArcherNum, "2", "3"),

    train_soldier:add_soldier(CityId, CavalryNum, "3", "6"),
    train_soldier:add_soldier(CityId, -CavalryNum, "3", "3").



-spec get_forward_num(Type::string(), CityId::city_id(), 
                      State::string()) -> integer().
get_forward_num(Type, {{X, Y}, AuthorId}, State) ->
    Where = ["x='", X, "' and ",
             "y='", Y, "' and ",
             "type='", Type, "' and ",
             "state='", State, "' and ",
             "author_id='", AuthorId, "'"],
    Sql = db:select_sql("soldier", ["soldier_sum"], Where),
    case db:select(Sql) of
        [] -> 0;
        [[{"soldier_sum", Num}]] -> Num
    end.

    
    

-spec get_distance(FromCityId::city_id(), ToCityId::city_id()) -> float().
get_distance({{X1, Y1}, _}, {{X2, Y2}, _}) ->
    math:sqrt((X1-X2)*(X1-X2)+(Y1-Y2)*(Y1-Y2)).
    

get_spend_time(Distance, PikeSoldier, ArcherSoldier, Cavalry) -> 
    Time =  
        case Cavalry of
            {_, 0} ->
                60/2;
            _Other1 ->
                60/10
        end,
    Time1 = 
        case ArcherSoldier of
            {_, 0} ->
                Time;
            _Other2 ->
                60/2
        end,
    Time3 = 
        case PikeSoldier of
            {_, 0} ->
                Time1;
            _Other3 ->
                60/1.5
        end, 
    %%Distance * Time3.
    trunc(Distance * Time3*2).  %TODO
     



kill_soldier(From, To) ->
    Random = random:uniform(2),
    kill_soldier(From, To, Random).

kill_soldier(_From, To, 1) ->
    kill_soldier(To);
kill_soldier(From, _To, 2) ->
    kill_soldier(From).

kill_soldier({{X,Y}, AuthorId}) ->
    %%TODO ignore back time
    Type = get_random_type(),
    Where = ["x='", X, "' and ",
             "y='", Y, "' and ",
             "type='", Type, "' and ",
             "state='6' and ",
             "author_id='", AuthorId, "'"],
    Sql = db:update_sql("soldier", ["soldier_sum=soldier_sum-1"], Where),
    db:update(Sql),
    db:update("update soldier set soldier_sum=0 where soldier_sum<0").

get_random_type() ->
    Random = random:uniform(3),
    integer_to_list(Random).
    
    
-spec go_back(CityId::city_id(), AddState::string(), ReduceState::string()) -> ok.
go_back(CityId, AddState, ReduceState) ->
    PikeNum = get_forward_num("1", CityId, ReduceState),
    ArcherNum = get_forward_num("2", CityId, ReduceState),
    CavalryNum =get_forward_num("3", CityId, ReduceState),

    train_soldier:add_soldier(CityId, PikeNum, "1", AddState),
    train_soldier:add_soldier(CityId, -PikeNum, "1", ReduceState),

    train_soldier:add_soldier(CityId, ArcherNum, "2", AddState),
    train_soldier:add_soldier(CityId, -ArcherNum, "2", ReduceState),

    train_soldier:add_soldier(CityId, CavalryNum, "3", AddState),
    train_soldier:add_soldier(CityId, -CavalryNum, "3", ReduceState),
    db:update("update soldier set soldier_sum=0 where soldier_sum<0").



-spec is_empty(CityId::city_id(), Cities::dict(), Max::integer()) ->
            ok|{error, Error::term()}.
is_empty(CityId, Cities, Max) ->
    case dict:is_key(CityId, Cities) of
        true ->
            case dict:fetch(CityId, Cities) of
                Value when Value < Max -> 
                    ?DEBUG("~p:is_empty ~p Value=~p", [?MODULE, ?LINE, Value]),
                    ok;
                _IsFull ->
                    ?DEBUG("~p:is_empty ~p _IsFull=~p", [?MODULE, ?LINE, _IsFull]),
                    {error, "is_full"}    
            end;
        false ->
           ok
    end.




-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(TEST).


respawn_test1() -> 
    application:start(game),
    X4 = 4, Y4=4, 
    AuthorId4 = "langxw",
    X9=9, Y9 = 9,
    AuthorId9 = "langxw9",
    CityId = {{X4, Y4}, AuthorId4},
    From = CityId,
    To = {{X9, Y9}, AuthorId9},
    simple_server:create_city({X4, Y4}, AuthorId4),
    City44 = simple_server:get_city({X4, Y4}, AuthorId4),
    %simple_server:change_peoples(City44#city{golds=1000.0}),
    simple_server:create_city({X9, Y9}, AuthorId9),
    City99 = simple_server:get_city({X9, Y9}, AuthorId9),
    %simple_server:change_peoples(City99#city{golds=1000.0}),
    db:update("update city set golds='1000', foods='1000';"),
    train_soldier:add_train("1", From, "1", 15),
    train_soldier:add_train("1", To, "4", 15),
    train_soldier:add_train("2", From, "2", 15),
    train_soldier:add_train("2", To, "5", 15),
    %train_soldier:add_train("3", To, "6", 15),
    %train_soldier:add_train("3", From, "3", 15),
    receive after 1500 -> ok end,
    train_soldier:get_soldier_for_city(From),
    train_soldier:get_soldier_for_city(To),
    send_soldier(From, To, {"1",10}, {"2",10}, {"3",0}),
    receive after 200 -> ok end,
    {timeout, 15, ?_assertEqual(true, begin 
            send_soldier(From, To, {"1",0}, {"2",1}, {"3",0}),
        timer:sleep(10000), true end)},
    respawn_test(). 
    %receive after 2000 -> ok end,
    %train_soldier:get_soldier_for_city(From),
    %train_soldier:get_soldier_for_city(To),
    %kill_soldier(To, From),
    %kill_soldier(To, From),
   %kill_soldier(To, From),
    %receive after 1000 -> ok end,
    %[{timeout, 10000, ?assertMatch(ok, ok)}].
respawn_test() -> 
    application:start(game),
    X4 = 4, Y4=4, 
    AuthorId4 = "langxw",
    X9=9, Y9 = 9,
    AuthorId9 = "langxw9",
    CityId = {{X4, Y4}, AuthorId4},
    From = CityId,
    To = {{X9, Y9}, AuthorId9},
    simple_server:create_city({X4, Y4}, AuthorId4),
    City44 = simple_server:get_city({X4, Y4}, AuthorId4),
    simple_server:create_city({X9, Y9}, AuthorId9),
    City99 = simple_server:get_city({X9, Y9}, AuthorId9),
    db:delete("delete from soldier where state<>3;"),
    db:update("update soldier set soldier_sum= 100;"),
    db:update("update city set golds='1000', foods='1000';"),
    send_soldier(From, To, {"1",10}, {"2",10}, {"3",0}),
    receive after 2000 -> ok end,
    {timeout, 15, ?_assertEqual(true, begin 
            send_soldier(From, To, {"1",0}, {"2",1}, {"3",0}),
        timer:sleep(10000), true end)}.
 
-endif.

