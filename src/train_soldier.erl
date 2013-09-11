-module(train_soldier).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("game.hrl").
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%-define(SPEND_TIME,2).
-define(SPEND_TIME,1).
%-define(SPEND_TIME,1000).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([
         add_train/4,
         cancel_train/3,
         train/3,
         add_golds/2,
         add_soldier/4,
         get_soldier_for_city/1,
         get_train_soldier_for_city/1,
         consumer_food/1,
         is_empty/3,
         complete/3
         ]).

-record(state, {pids = dict:new(), 
                cities_id = dict:new(), 
                trains_id = dict:new(),
                max=5}).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec add_train(Type::string(), CityId::city_id(), TrainId::string(),
                Numbers::integer()) -> ok.
add_train(Type, CityId, TrainId, Numbers) ->
    ?DEBUG("~p:add_train ~p Type=~p, CityId=~p", [?MODULE, ?LINE, Type,CityId]),
    gen_server:call(?SERVER, {add_train, Type, CityId, TrainId, Numbers}).

-spec cancel_train(Type::string(), CityId::city_id(), TrainId::string()) -> ok.
cancel_train(Type, CityId, TrainId) ->
    gen_server:call(?SERVER, {cancel_train, Type, CityId, TrainId}).
    
 

-spec consumer_food(Time::integer()) -> ok.
consumer_food(Time) ->
    receive 
    after Time ->
        consumer_food(),
        consumer_food(Time)
    end.
    

-spec train(Type::string(), CityId::city_id(), Time::integer()) -> ok.
train(Type, CityId, Time) ->
    ?DEBUG("~p:train ~p Time=~p", [?MODULE, ?LINE, Time]),
    receive 
        cancel_train ->
            ok
    after Time ->
        ?DEBUG("~p:train ~p Time=~p", [?MODULE, ?LINE, Time]),
        complete(self(), Type, CityId),
        ok
        %train(Time, CityId, Time) 
    end.

-spec add_golds(CityId::city_id(), Golds::integer()) -> 
            ok|{error, Reason::term()}.
add_golds({{X, Y}, AuthorId}=_CityId, Golds) ->
    Where = ["x='", X, "' and ",
             "y='", Y, "' and ",
             "author_id='", AuthorId, "'"],
    Update = ["golds=golds+'", Golds,"'"],
    Sql = db:update_sql("city", Update, Where),
    case db:update(Sql) of
        {ok, _Num} ->
            ok;
        Error ->
            ?DEBUG("~p:update ~p Error=~p", [?MODULE, ?LINE, Error]),
            {error, Error}
    end.

-spec get_soldier_for_city(CityId::city_id()) -> [soldier()].
get_soldier_for_city({{X, Y}, AuthorId}) ->
    Where = ["x='", X, "' and ",
             "y='", Y, "' and ",
             "author_id='", AuthorId, "'"],
    Sql = db:select_sql("soldier", ["type, state, soldier_sum"], Where),
    db:select(Sql).
    
get_train_soldier_for_city({{X, Y}, AuthorId}) ->
    Where = ["x='", X, "' and ",
             "y='", Y, "' and ",
             "author_id='", AuthorId, "'"],
    Sql = db:select_sql("soldier_queue", ["type, state, soldier_sum", "time"], Where),
    db:select(Sql).


    
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({cancel_train, Type, CityId, TrainId}, _From, 
        #state{pids=Pids, cities_id=CitiesId,
               trains_id=TrainsId}=State) ->
    ?DEBUG("~p:handle_call ~p cancle_train TrainId=~p State=~p",
           [?MODULE, ?LINE, TrainId, State]),
    ?DEBUG("~p:handle_call ~p cancel_train TrainId=~p, TrainsId=~p",
        [?MODULE, ?LINE, TrainId, dict:to_list(TrainsId)]),
    case dict:is_key(TrainId, TrainsId) of
        false ->
            ?DEBUG("~p:handle_call ~p cancel_train", [?MODULE, ?LINE]),
            NewCitiesId = delete_from_queue(CityId, CitiesId, Type, TrainId),
            delete_solder_queue(CityId, Type, TrainId),
            NewState = State#state{cities_id=NewCitiesId};
        true ->
            ?DEBUG("~p:handle_call ~p cancel_train", [?MODULE, ?LINE]),
            NewCitiesId = delete_from_queue(CityId, CitiesId, Type, TrainId),
            delete_solder_queue(CityId, Type, TrainId),
            {Pid} = dict:fetch(TrainId, TrainsId),
            {train, Type, CityId, TrainId, _Numbers} = dict:fetch(Pid, Pids),
            Pid ! cancel_train,
            NewPids = dict:erase(Pid, Pids),
            NewTrainsId = dict:erase(TrainId, TrainsId),
            NewState = State#state{pids = NewPids,
                                   trains_id = NewTrainsId,
                                   cities_id = NewCitiesId}
    end,
    ?DEBUG("~p:handle_call ~p cancel_train", [?MODULE, ?LINE]),
    {reply, ok, NewState};
handle_call({add_train, Type, CityId, TrainId, Numbers}, _From, 
            #state{pids=Pids, trains_id=TrainsId,
            max=Max, cities_id=CitiesId}=State) ->

    ?DEBUG("~p:handle_call ~p add_train TrainId=~p, State=~p",
           [?MODULE, ?LINE, TrainId, State]),
    case is_may_train(CityId, CitiesId, Max, Numbers) of
        ok ->
            SpendTime = get_spend_time(Type),
            case spend_golds(Type, CityId, Numbers) of
                ok ->
                    ?DEBUG("~p:handle_call ~p add_train ", [?MODULE, ?LINE]),
                    Queue = get_queue(CityId, CitiesId),
                    Args = {train, Type, CityId, TrainId, Numbers},
                    QueueElement = {train, Type, CityId, TrainId, Numbers, SpendTime},
                    NewQueue = queue:in(QueueElement, Queue),
                    {NewCitiesId, NewPids, NewTrainsId} = 
                        case dict:size(Pids) of
                            0->
                                Pid = spawn_link(?MODULE, train, [Type, CityId, SpendTime]),
                                train_soldier_queue(CityId, Numbers, Type, TrainId),
                                {dict:store(CityId, NewQueue, CitiesId),
                                 dict:store(Pid, Args, Pids),
                                 dict:store(TrainId, {Pid}, TrainsId)};
                            _ -> 
                                add_soldier_queue(CityId, Numbers, Type, TrainId, "1"),
                                {dict:store(CityId, NewQueue, CitiesId),
                                 Pids,
                                 TrainsId}
                        end,
                    NewState = State#state{pids = NewPids, 
                                           cities_id = NewCitiesId,
                                           trains_id = NewTrainsId
                                           };

                Error1 ->
                    ?ERROR("~p:handle_call ~p add_train Error1=~p", [?MODULE, ?LINE, Error1]),
                    NewState = State
            end;
        Error ->
            ?ERROR("~p:handle_call ~p add_train Error=~p", [?MODULE, ?LINE, Error]),
            NewState = State
    end,
    {reply, ok, NewState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({complete, Pid, Type, CityId}, 
            #state{pids=Pids, 
                   cities_id=CitiesId,
                   trains_id=TrainsId }=State) ->
    ?DEBUG("~p:handle_call ~p complete _train Pid=~p State=~p",
           [?MODULE, ?LINE, Pid, State]),
    case dict:is_key(Pid, Pids) of
        true ->
            {train, Type, CityId, TrainId, _Numbers} = dict:fetch(Pid, Pids),
            ?DEBUG("~p:handle_cast complete ~p TrainId=~p", [?MODULE, ?LINE, TrainId]),
            Queue = get_queue(CityId, CitiesId),
            {_, NewQueue} = queue:out(Queue),
            NewCitiesId = dict:store(CityId, NewQueue, CitiesId),
            change_soldier(CityId, TrainId, Type),
            case get_next_train(NewQueue) of
                empty ->
                    NewPids = dict:erase(Pid, Pids),
                    NewTrainsId = dict:erase(TrainId, TrainsId),
                    NewState = State#state{pids=NewPids, 
                                           cities_id=NewCitiesId, 
                                           trains_id=NewTrainsId};
                {train, NextType, NextCityId, NextTrainId, NextNumbers, NextSpendTime} ->
                    ?DEBUG("~p:handle_cast complete ~p NextTrainId=~p, TrainId=~p", 
                            [?MODULE, ?LINE, NextTrainId, TrainId]),
                    NextPid = spawn_link(?MODULE, train, [NextType, NextCityId, NextSpendTime]),
                    NewNextTrainsId = dict:store(NextTrainId, {NextPid}, 
                                                 dict:erase(TrainId, TrainsId)),
                    ?DEBUG("~p:handle_cast complete ~p NewNextTrainsId=~p", 
                            [?MODULE, ?LINE, dict:to_list(NewNextTrainsId)]),
                    Args = {train, NextType, NextCityId, NextTrainId, NextNumbers},
                    NewNextPids =dict:store(NextPid, Args, dict:erase(Pid, Pids)),
                    train_soldier_queue(NextCityId, NextNumbers, NextType, NextTrainId),
                    NewState = State#state{pids = NewNextPids, 
                                           cities_id = NewCitiesId,
                                           trains_id = NewNextTrainsId}

                    
            end;
        false -> NewState = State
    end,
    {noreply, NewState};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'EXIT', _From, normal}, State) ->
    ?DEBUG("~p:handel_info ~p Reason=~p", [?MODULE, ?LINE, normal]),
    {noreply, State};
handle_info({'EXIT', From, Reason}, 
            #state{pids=Pids, trains_id=TrainsId}=State) ->
    ?DEBUG("~p:handel_info ~p Reason=~p", [?MODULE, ?LINE, Reason]),
    case dict:fetch(From, Pids) of
        {train, Type, CityId, TrainId, Numbers} ->
            SpendTime = get_spend_time(Type),
            Pid = spawn_link(?MODULE, train, [Type, CityId, SpendTime]),
            NewPids = dict:store(Pid, 
                                 {train, Type, CityId, TrainId, Numbers}, 
                                 dict:erase(From, Pids)),
            NewTrainsId =dict:store(TrainId, Pid, TrainsId),
            NewState=State#state{pids = NewPids, trains_id = NewTrainsId}
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


-spec complete(Pid::pid(), Type::string(), CityId::city_id()) -> ok.
complete(Pid, Type, CityId) ->
    gen_server:cast(?SERVER, {complete, Pid, Type, CityId}).

-spec spend_golds(Type::string(), CityId::city_id(), Numbers::integer()) -> 
        ok|{error, Error::term()}.
spend_golds(Type, {{X, Y}, AuthorId}=CityId, Numbers) ->
    SpendGolds = get_spend_golds(Type)*Numbers,
    ?DEBUG("~p:spend_golds ~p Type=~p, CityId=~p, SpendGolds=~p",
            [?MODULE, ?LINE, Type, CityId, SpendGolds]),
    Where = ["x='", X, "' and ",
             "y='", Y, "' and ",
             "author_id='", AuthorId, "'"],
    Sql = db:select_sql("city", ["golds"], Where),
    case db:select(Sql) of
        [] -> {error, "the city is not exists"};
        [[{"golds", Golds}]] when Golds >=SpendGolds ->
            ?DEBUG("~p:spend_golds ~p Type=~p, CityId=~p, SpendGolds=~p, Golds=~p",
            [?MODULE, ?LINE, Type, CityId, SpendGolds, Golds]),
            Set = ["golds=golds-'", SpendGolds, "'"],
            UpdateSql = db:update_sql("city", Set, Where),
            ?DEBUG("~p:spend_golds ~p UpdateSql=~p", [?MODULE, ?LINE, UpdateSql]),
            {ok, _}=db:update(UpdateSql),
            ok;
        [[{"golds", Golds}]] when Golds <SpendGolds ->
            ?DEBUG("~p:spend_golds ~p Type=~p, CityId=~p, SpendGolds=~p, Golds=~p",
            [?MODULE, ?LINE, Type, CityId, SpendGolds, Golds]),
            {error, "not enough glods"};
        Error ->
            ?ERROR("~p:spend_golds ~p Error=~p", [?MODULE, ?LINE, Error]),
            {error, Error}

    end.
    

-spec is_may_train(CityId::city_id(), Cities::dict(), 
            Max::integer(), Numbers::integer()) ->
            ok|{error, Error::term()}.
is_may_train(CityId, Cities, Max, Numbers) ->
    case is_enough_people_to_train(CityId, Numbers) of
        ok ->
            is_empty(CityId, Cities, Max);
        Error ->
            Error
    end.


-spec is_empty(CityId::city_id(), Cities::dict(), Max::integer()) ->
            ok|{error, Error::term()}.
is_empty(CityId, Cities, Max) ->
    case dict:is_key(CityId, Cities) of
        true ->
            case queue:len(dict:fetch(CityId, Cities)) of
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

-spec is_enough_people_to_train(CityId::city_id(), Numbers::integer()) ->
        ok|{error, Error::term()}.
is_enough_people_to_train({{X, Y}, AuthorId}, Numbers) ->
    Where = ["x='", X, "' and ",
             "y='", Y, "' and ",
             "author_id='", AuthorId, "'"],
    Sql = db:select_sql("city", ["people"], Where),
    case db:select(Sql) of
        [] -> {error, "the city is not exists"};
        [[{"people", Peoples}]] when Peoples >= Numbers->
            Set = ["people=people-'", Numbers, "'"],
            UpdateSql = db:update_sql("city", Set, Where),
            {ok, _}=db:update(UpdateSql),
            ok;
        [[{"people", Peoples}]] when Peoples < Numbers->
            {error, "the city is not enough peoples"};
        Error ->
            ?ERROR("~p:is_enough_people_to_train ~p Error=~p", 
                    [?MODULE, ?LINE, Error]),
            {error, Error}
    end.
    
-spec change_soldier(CityId::city_id(), TrainId::string(), Type::string()) ->
        ok|{error, Error::term()}.
change_soldier({{X, Y}, AuthorId}=CityId, TrainId, Type) ->
    ?DEBUG("~p:change_soldier ~p", [?MODULE, ?LINE]),
    Where = ["x='", X, "' and ",
             "y='", Y, "' and ",
             "queue_id='", TrainId, "' and ",
             "author_id='", AuthorId, "'"],
    SelSql = db:select_sql("soldier_queue", ["soldier_sum"], Where),
    ?DEBUG("~p:change_soldier ~p SelSql=~p", [?MODULE, ?LINE, SelSql]),
    case db:select(SelSql) of
        [] -> ok;
        [[{"soldier_sum", Sum}]] ->
            DelSql = db:delete_sql("soldier_queue", Where),
            ?DEBUG("~p:change_soldier ~p SelDql=~p", [?MODULE, ?LINE, DelSql]),
            {ok, _}=db:delete(DelSql),
            add_soldier(CityId, Sum, Type, "3")
    end.

-spec add_soldier(CityId::city_id(), Numbers::integer(), 
                  Type::string(), State::string()) ->
        ok|{error, Error::term()}.
add_soldier({{X, Y}, AuthorId}=_CityId, Numbers, Type, State) ->
    Fields = ["soldier_sum"],
    Where = ["x='", X, "' and ",
             "y='", Y, "' and ",
             "author_id='", AuthorId, "' and ",
             "type='", Type, "' and ",
             "state='", State, "'"],
    Sql = db:select_sql("soldier", Fields, Where),
    %TODO reduce city people 
    case db:select(Sql) of
        [] ->
            Value = ["'", X, "',",
                     "'", Y, "',",
                     "'", AuthorId, "',",
                     "'", Type, "',",
                     "'", Numbers, "',",
                     "'", State, "'"],
            FieldsInsert = ["x, y, author_id, type, soldier_sum, state"],
            SqlInsert =db:insert_sql("soldier", FieldsInsert, Value),
            db:insert(SqlInsert);
        [[{"soldier_sum", _Sum}]] ->
            SetFields = ["soldier_sum=soldier_sum+'", Numbers, "'"],
            SqlUpdate = db:update_sql("soldier", SetFields, Where),
            db:update(SqlUpdate)
    end.


-spec train_soldier_queue(CityId::city_id(), Numbers::integer(), 
                  Type::string(), QueueId::string()) ->
        ok|{error, Error::term()}.
train_soldier_queue({{X, Y}, AuthorId}=CityId, Numbers, Type, TrainId) ->
    ?DEBUG("~p:train_soldier_queue ~p", [?MODULE, ?LINE]),
    Where = ["x='", X, "' and ",
             "y='", Y, "' and ",
             "author_id='", AuthorId, "' and ",
             "type='", Type, "' and ",
             "queue_id='", TrainId, "'"],
    Sql = db:select_sql("soldier_queue", ["id"], Where),
    case db:select(Sql) of
        [] -> add_soldier_queue(CityId, Numbers, Type, TrainId, "2");
        [[{"id", _Id}]] ->
            UpdateSql = db:update_sql("soldier_queue", [" state='2' "], Where),
            {ok, _}=db:update(UpdateSql)
    end.
    


-spec add_soldier_queue(CityId::city_id(), Numbers::integer(), 
                  Type::string(), QueueId::string(), State::string()) ->
        ok|{error, Error::term()}.
add_soldier_queue({{X, Y}, AuthorId}=_CityId, Numbers, Type, QueueId, State) ->
    Fields = ["x, y, author_id, type, soldier_sum, queue_id, state, time"], 
    Value = ["'", X, "',",
             "'", Y, "',",
             "'", AuthorId, "',",
             "'", Type, "',",
             "'", Numbers, "',",
             "'", QueueId, "',",
             "'", State, "',",
             "'", utils:get_datetime(), "'"],
    SqlSoldier = db:insert_sql("soldier_queue", Fields, Value),
    ?DEBUG("~p:add_soldier_queue ~p SqlSoldier=~p", [?MODULE, ?LINE, SqlSoldier]),
    case db:insert(SqlSoldier) of
        {ok, _Num, _Sum} ->
            ok;
        Error ->
            ?ERROR("~p:add_soldier_queue ~p Error=~p", 
                    [?MODULE, ?LINE, Error]),
            {error, Error}
    end.
        
consumer_food() ->
    consumer_food1("1"),
    consumer_food1("2"),
    consumer_food1("3"),
    db:update("update city set foods=0, is_food_crisis='1' "++
              " where foods <0 or foods is null;").

consumer_food1(Type) ->
    ConsumerFoods = get_consumer_food(Type),
    Sql = 
        lists:concat(["update city c set c.foods=c.foods-(select sum(soldier_sum)* ",
                       ConsumerFoods,
                      " from soldier s where s.x=c.x and s.y=c.y and ",
                      " s.type= '",Type,"' group by  s.x, s.y ) ;"]),
    {ok, _}=db:update(Sql).

get_consumer_food("1") ->
    10;
get_consumer_food("2") ->
    13;
get_consumer_food("3") ->
    30.
    


get_spend_golds("1") ->
    1;
get_spend_golds("2") ->
    3;
get_spend_golds("3") ->
    10.

get_spend_time("1") ->
    3*60*?SPEND_TIME;
get_spend_time("2") ->
    12*60*?SPEND_TIME;
get_spend_time("3") ->
    50*60*?SPEND_TIME.
    
-spec get_next_train(Queue::queue()) -> {train, list()}.
get_next_train(Queue) ->
    case queue:out(Queue) of
        {empty,{[],[]}} ->
            empty;
        {{value, Value}, _Queue} ->
            Value
    end.

-spec get_queue(CityId::city_id(), CitiesId::dict()) ->queue().
get_queue(CityId, CitiesId) ->
    case dict:is_key(CityId, CitiesId) of
        true -> dict:fetch(CityId, CitiesId);
        false -> queue:new()
    end.

-spec delete_from_queue(CityId::city_id(), CitiesId::dict(), 
                        Type::string(), TrainId::string()) -> dict().
delete_from_queue(CityIdParm, CitiesId, TypeParm, TrainIdParm) ->
    Queue = get_queue(CityIdParm, CitiesId),
    Fun = fun({train, Type, CityId, TrainId, _Numbers, _SpendTime}) ->
                case {CityIdParm, TypeParm, TrainIdParm} of
                    {CityId, Type, TrainId} -> false;
                    _Other -> true
                end
          end,
    NewQueue = queue:filter(Fun, Queue),
    dict:store(CityIdParm, NewQueue, CitiesId).
    
    
-spec delete_solder_queue(CityId::city_id(), Type::string(), TrainId::string()) ->
            ok|{error, term()}.
delete_solder_queue({{X, Y}, AuthorId}, Type, TrainId) ->
    Where = ["x='", X, "' and ",
             "y='", Y, "' and ",
             "author_id='", AuthorId, "' and ",
             "type='", Type, " ' and ",
             "queue_id='", TrainId, " ' and ",
             "author_id='", AuthorId, "'"],
    Sql = db:delete_sql("soldier_queue", Where),
    db:delete(Sql).
    

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-ifdef(TEST).

respawn_test1() -> application:start(game),
    X = 3, Y=3, AuthorId = "langxw",
    Y2 = 2,
    CityNotExists = add_train("1", {{X, Y}, AuthorId}, "1", 5),
    %receive after 2000 -> ok end,
    simple_server:create_city({X, Y}, AuthorId),
    simple_server:create_city({X, Y2}, AuthorId),
    add_golds({{X, Y}, AuthorId}, 1),
    db:update("update city set foods='1000'"),
    NotEnoughGlods = add_train("2", {{X, Y}, AuthorId}, "2", 5),
    add_golds({{X, Y}, AuthorId}, 3003),
    CityId = {{X, Y}, AuthorId},
    ok = add_train("1", {{X, Y}, AuthorId}, "3", 5),
    ok = add_train("2", {{X, Y}, AuthorId}, "4", 5),
    ok = add_train("3", {{X, Y}, AuthorId}, "5", 5),
    ok = add_train("1", {{X, Y}, AuthorId}, "6", 500), %%%
    ok = add_train("2", {{X, Y}, AuthorId}, "7", 5),
    ok = add_train("2", {{X, Y}, AuthorId}, "8", 5),
    ok = add_train("1", {{X, Y}, AuthorId}, "9", 5),
    get_soldier_for_city(CityId),
    receive after 3*60*?SPEND_TIME -> ok end,
    ok = add_train("3", {{X, Y}, AuthorId}, "10", 5),
    get_soldier_for_city(CityId),
    ok = add_train("1", {{X, Y}, AuthorId}, "11", 5),
    get_soldier_for_city(CityId),
    get_train_soldier_for_city(CityId),
    receive after 3*60*?SPEND_TIME -> ok end,
    cancel_train("3", {{X, Y}, AuthorId}, "5"),
    cancel_train("2", {{X, Y}, AuthorId}, "4"),
    cancel_train("2", {{X, Y}, AuthorId}, "7"),
%    cancel_train("1", {{X, Y}, AuthorId}, "11"),
%    complete(self(), "1", {{X, Y}, AuthorId}),
%    cancel_train("4", {{X, Y2}, AuthorId}, "10"),
 %   receive after 1000 -> ok end,
    [?assertMatch(ok, ok),
     ?assertMatch(1,1)
    ].

-endif.

