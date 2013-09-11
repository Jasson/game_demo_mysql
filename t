==> Entering directory `/home/jason/diudiu/game/mysql_game/deps/mysql'
==> mysql (compile)
==> Leaving directory `/home/jason/diudiu/game/mysql_game/deps/mysql'
==> mysql_game (compile)
Compiled src/battle.erl
==> Entering directory `/home/jason/diudiu/game/mysql_game/deps/mysql'
==> mysql (eunit)
  There were no tests to run.
Cover analysis: /home/jason/diudiu/game/mysql_game/deps/mysql/.eunit/index.html
==> Leaving directory `/home/jason/diudiu/game/mysql_game/deps/mysql'
==> mysql_game (eunit)
src/battle.erl:348: Warning: function respawn_test1/0 is unused
src/battle.erl:358: Warning: variable 'City44' is unused
src/battle.erl:361: Warning: variable 'City99' is unused
src/battle.erl:375: Warning: a term is constructed, but never used
src/battle.erl:397: Warning: variable 'City44' is unused
src/battle.erl:399: Warning: variable 'City99' is unused
Compiled src/battle.erl
mysql_conn:627: greeting version "5.5.32-0ubuntu0.12.04.1" (protocol 10) salt "~Bm!Ws^[" caps 63487 serverchar <<8,2,0,
                                                                                                                 15,128,
                                                                                                                 21,0,0,
                                                                                                                 0,0,0,0,
                                                                                                                 0,0,0,0>>salt2 "0y6>rFGp\"RYH"
mysql_auth:189: mysql_auth send packet 1: <<5,162,0,0,64,66,15,0,8,0,0,0,0,0,0,
                                            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                            114,111,111,116,0,20,68,177,121,40,
                                            198,244,181,80,151,62,140,135,72,
                                            191,59,80,95,124,124,192>>
mysql_conn:433: fetch <<"use game">> (id <0.221.0>)
mysql_conn:627: greeting version "5.5.32-0ubuntu0.12.04.1" (protocol 10) salt "=aRW<ekf" caps 63487 serverchar <<8,2,0,
                                                                                                                 15,128,
                                                                                                                 21,0,0,
                                                                                                                 0,0,0,0,
                                                                                                                 0,0,0,0>>salt2 "@L>2}CtB(BQR"
mysql_auth:189: mysql_auth send packet 1: <<5,162,0,0,64,66,15,0,8,0,0,0,0,0,0,
                                            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                            114,111,111,116,0,20,220,94,48,58,
                                            35,131,214,22,201,39,9,83,122,127,
                                            221,160,31,139,147,78>>
mysql_conn:433: fetch <<"use game">> (id <0.225.0>)
mysql:565: added connection with id 'pool' (pid <0.224.0>) to my list
mysql_conn:627: greeting version "5.5.32-0ubuntu0.12.04.1" (protocol 10) salt "(N/A6.$f" caps 63487 serverchar <<8,2,0,
                                                                                                                 15,128,
                                                                                                                 21,0,0,
                                                                                                                 0,0,0,0,
                                                                                                                 0,0,0,0>>salt2 "Yk.?4'eGr+zI"
mysql_auth:189: mysql_auth send packet 1: <<5,162,0,0,64,66,15,0,8,0,0,0,0,0,0,
                                            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                            114,111,111,116,0,20,233,40,112,80,
                                            207,58,181,215,98,32,10,19,206,153,
                                            123,0,205,118,111,123>>
mysql_conn:433: fetch <<"use game">> (id <0.227.0>)
mysql:565: added connection with id 'pool' (pid <0.226.0>) to my list

=INFO REPORT==== 6-Sep-2013::12:58:23 ===
simple_server:product_food 166 Time=1000

=INFO REPORT==== 6-Sep-2013::12:58:23 ===
simple_server:collect_taxes 158 Time=60000
mysql_conn:433: fetch <<"select id,x,y,author_id from map where x='4' and y='4' and author_id='langxw' ;">> (id <0.227.0>)

=INFO REPORT==== 6-Sep-2013::12:58:23 ===
battle:init 80 

=INFO REPORT==== 6-Sep-2013::12:58:23 ===
simple_server:create_city 36 {X, Y}={4,4}, AuthorId="langxw"

=INFO REPORT==== 6-Sep-2013::12:58:23 ===
simple_server:is_empty 62 Sql="select id,x,y,author_id from map where x='4' and y='4' and author_id='langxw' ;"
mysql_conn:433: fetch <<"select id,x,y,author_id from map where x='4' and y='4' and author_id='langxw' ;">> (id <0.225.0>)

=INFO REPORT==== 6-Sep-2013::12:58:23 ===
simple_server:is_empty 68 _Map=[{"id",1},
                                {"x",4},
                                {"y",4},
                                {"author_id",<<"langxw">>}] 

=INFO REPORT==== 6-Sep-2013::12:58:23 ===
simple_server:is_empty 62 Sql="select id,x,y,author_id from map where x='4' and y='4' and author_id='langxw' ;"
mysql_conn:433: fetch <<"select x, y, foods, golds, gold_tax, people, soldiers, name, is_captial from city where x='4' and y='4' and author_id='langxw' ;">> (id <0.221.0>)

=INFO REPORT==== 6-Sep-2013::12:58:23 ===
simple_server:is_empty 68 _Map=[{"id",1},
                                {"x",4},
                                {"y",4},
                                {"author_id",<<"langxw">>}] 
mysql_conn:433: fetch <<"select id,x,y,author_id from map where x='9' and y='9' and author_id='langxw9' ;">> (id <0.227.0>)

=INFO REPORT==== 6-Sep-2013::12:58:23 ===
simple_server:get_city 85 Cites=[[{"x",4},
                                  {"y",4},
                                  {"foods",1000.54},
                                  {"golds",1.0e3},
                                  {"gold_tax",0.2},
                                  {"people",70},
                                  {"soldiers",0},
                                  {"name",<<>>},
                                  {"is_captial",<<"0">>}]]

=INFO REPORT==== 6-Sep-2013::12:58:23 ===
simple_server:create_city 36 {X, Y}={9,9}, AuthorId="langxw9"

=INFO REPORT==== 6-Sep-2013::12:58:23 ===
simple_server:is_empty 62 Sql="select id,x,y,author_id from map where x='9' and y='9' and author_id='langxw9' ;"
mysql_conn:433: fetch <<"select id,x,y,author_id from map where x='9' and y='9' and author_id='langxw9' ;">> (id <0.225.0>)

=INFO REPORT==== 6-Sep-2013::12:58:23 ===
simple_server:is_empty 68 _Map=[{"id",2},
                                {"x",9},
                                {"y",9},
                                {"author_id",<<"langxw9">>}] 

=INFO REPORT==== 6-Sep-2013::12:58:23 ===
simple_server:is_empty 62 Sql="select id,x,y,author_id from map where x='9' and y='9' and author_id='langxw9' ;"
mysql_conn:433: fetch <<"select x, y, foods, golds, gold_tax, people, soldiers, name, is_captial from city where x='9' and y='9' and author_id='langxw9' ;">> (id <0.221.0>)

=INFO REPORT==== 6-Sep-2013::12:58:23 ===
simple_server:is_empty 68 _Map=[{"id",2},
                                {"x",9},
                                {"y",9},
                                {"author_id",<<"langxw9">>}] 
mysql_conn:433: fetch <<"delete from soldier where state<>3;">> (id <0.227.0>)

=INFO REPORT==== 6-Sep-2013::12:58:23 ===
simple_server:get_city 85 Cites=[[{"x",9},
                                  {"y",9},
                                  {"foods",1000.54},
                                  {"golds",1.0e3},
                                  {"gold_tax",0.2},
                                  {"people",70},
                                  {"soldiers",0},
                                  {"name",<<>>},
                                  {"is_captial",<<"0">>}]]
mysql_conn:433: fetch <<"update soldier set soldier_sum= 100;">> (id <0.225.0>)
mysql_conn:433: fetch <<"update city set golds='1000', foods='1000';">> (id <0.221.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and type='1' and state='3' and author_id='langxw' ;">> (id <0.227.0>)

=INFO REPORT==== 6-Sep-2013::12:58:23 ===
battle:handle_call 87 send_soldier state={state,
                                          {dict,0,16,16,8,80,48,
                                           {[],[],[],[],[],[],[],[],[],[],[],
                                            [],[],[],[],[]},
                                           {{[],[],[],[],[],[],[],[],[],[],[],
                                             [],[],[],[],[]}}},
                                          {dict,0,16,16,8,80,48,
                                           {[],[],[],[],[],[],[],[],[],[],[],
                                            [],[],[],[],[]},
                                           {{[],[],[],[],[],[],[],[],[],[],[],
                                             [],[],[],[],[]}}},
                                          5}

=INFO REPORT==== 6-Sep-2013::12:58:23 ===
battle:to_forward 194 Num=10
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='1' and state='4' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"insert into  soldier (x, y, author_id, type, soldier_sum, state) values('4','4','langxw','1','10','4');">> (id <0.221.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='1' and state='3' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'-10' where x='4' and y='4' and author_id='langxw' and type='1' and state='3' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and type='2' and state='3' and author_id='langxw' ;">> (id <0.221.0>)

=INFO REPORT==== 6-Sep-2013::12:58:23 ===
battle:to_forward 194 Num=10
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='2' and state='4' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"insert into  soldier (x, y, author_id, type, soldier_sum, state) values('4','4','langxw','2','10','4');">> (id <0.225.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='2' and state='3' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'-10' where x='4' and y='4' and author_id='langxw' and type='2' and state='3' ;">> (id <0.227.0>)

=INFO REPORT==== 6-Sep-2013::12:58:23 ===
battle:to_battle 54 SpendTime=565
mysql_conn:433: fetch <<"update city set foods=foods+product_food_rate ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and type='1' and state='4' and author_id='langxw' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and type='2' and state='4' and author_id='langxw' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and type='3' and state='4' and author_id='langxw' ;">> (id <0.225.0>)

=INFO REPORT==== 6-Sep-2013::12:58:24 ===
simple_server:product_food 170 Time=1000

=INFO REPORT==== 6-Sep-2013::12:58:24 ===
simple_server:product_food 166 Time=1000
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='1' and state='6' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"insert into  soldier (x, y, author_id, type, soldier_sum, state) values('4','4','langxw','1','10','6');">> (id <0.227.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='1' and state='4' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'-10' where x='4' and y='4' and author_id='langxw' and type='1' and state='4' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='2' and state='6' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"insert into  soldier (x, y, author_id, type, soldier_sum, state) values('4','4','langxw','2','10','6');">> (id <0.225.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='2' and state='4' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'-10' where x='4' and y='4' and author_id='langxw' and type='2' and state='4' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='3' and state='6' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"insert into  soldier (x, y, author_id, type, soldier_sum, state) values('4','4','langxw','3','0','6');">> (id <0.221.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='3' and state='4' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"insert into  soldier (x, y, author_id, type, soldier_sum, state) values('4','4','langxw','3','0','4');">> (id <0.225.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='9' and y='9' and type='1' and state='3' and author_id='langxw9' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='9' and y='9' and type='2' and state='3' and author_id='langxw9' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='9' and y='9' and type='3' and state='3' and author_id='langxw9' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='9' and y='9' and author_id='langxw9' and type='1' and state='6' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"insert into  soldier (x, y, author_id, type, soldier_sum, state) values('9','9','langxw9','1','100','6');">> (id <0.227.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='9' and y='9' and author_id='langxw9' and type='1' and state='3' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'-100' where x='9' and y='9' and author_id='langxw9' and type='1' and state='3' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='9' and y='9' and author_id='langxw9' and type='2' and state='6' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"insert into  soldier (x, y, author_id, type, soldier_sum, state) values('9','9','langxw9','2','100','6');">> (id <0.225.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='9' and y='9' and author_id='langxw9' and type='2' and state='3' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'-100' where x='9' and y='9' and author_id='langxw9' and type='2' and state='3' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='9' and y='9' and author_id='langxw9' and type='3' and state='6' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"insert into  soldier (x, y, author_id, type, soldier_sum, state) values('9','9','langxw9','3','100','6');">> (id <0.221.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='9' and y='9' and author_id='langxw9' and type='3' and state='3' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'-100' where x='9' and y='9' and author_id='langxw9' and type='3' and state='3' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum-1 where x='9' and y='9' and type='2' and state='6' and author_id='langxw9' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=0 where soldier_sum<0">> (id <0.227.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum-1 where x='4' and y='4' and type='3' and state='6' and author_id='langxw' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=0 where soldier_sum<0">> (id <0.221.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum-1 where x='4' and y='4' and type='1' and state='6' and author_id='langxw' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=0 where soldier_sum<0">> (id <0.225.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='9' and y='9' and type='1' and state='6' and author_id='langxw9' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='9' and y='9' and type='2' and state='6' and author_id='langxw9' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='9' and y='9' and type='3' and state='6' and author_id='langxw9' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='9' and y='9' and author_id='langxw9' and type='1' and state='3' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'100' where x='9' and y='9' and author_id='langxw9' and type='1' and state='3' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='9' and y='9' and author_id='langxw9' and type='1' and state='6' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'-100' where x='9' and y='9' and author_id='langxw9' and type='1' and state='6' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='9' and y='9' and author_id='langxw9' and type='2' and state='3' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'99' where x='9' and y='9' and author_id='langxw9' and type='2' and state='3' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='9' and y='9' and author_id='langxw9' and type='2' and state='6' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'-99' where x='9' and y='9' and author_id='langxw9' and type='2' and state='6' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='9' and y='9' and author_id='langxw9' and type='3' and state='3' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'100' where x='9' and y='9' and author_id='langxw9' and type='3' and state='3' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='9' and y='9' and author_id='langxw9' and type='3' and state='6' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'-100' where x='9' and y='9' and author_id='langxw9' and type='3' and state='6' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"update city set foods=foods+product_food_rate ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=0 where soldier_sum<0">> (id <0.227.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and type='1' and state='6' and author_id='langxw' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and type='2' and state='6' and author_id='langxw' ;">> (id <0.221.0>)

=INFO REPORT==== 6-Sep-2013::12:58:25 ===
simple_server:product_food 170 Time=1000

=INFO REPORT==== 6-Sep-2013::12:58:25 ===
simple_server:product_food 166 Time=1000
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and type='3' and state='6' and author_id='langxw' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='1' and state='5' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"insert into  soldier (x, y, author_id, type, soldier_sum, state) values('4','4','langxw','1','9','5');">> (id <0.221.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='1' and state='6' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'-9' where x='4' and y='4' and author_id='langxw' and type='1' and state='6' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='2' and state='5' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"insert into  soldier (x, y, author_id, type, soldier_sum, state) values('4','4','langxw','2','10','5');">> (id <0.227.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='2' and state='6' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'-10' where x='4' and y='4' and author_id='langxw' and type='2' and state='6' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='3' and state='5' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"insert into  soldier (x, y, author_id, type, soldier_sum, state) values('4','4','langxw','3','0','5');">> (id <0.225.0>)
  All 5 tests passed.
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='3' and state='6' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'0' where x='4' and y='4' and author_id='langxw' and type='3' and state='6' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=0 where soldier_sum<0">> (id <0.225.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and type='1' and state='5' and author_id='langxw' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and type='2' and state='5' and author_id='langxw' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and type='3' and state='5' and author_id='langxw' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='1' and state='3' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'9' where x='4' and y='4' and author_id='langxw' and type='1' and state='3' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='1' and state='5' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'-9' where x='4' and y='4' and author_id='langxw' and type='1' and state='5' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='2' and state='3' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'10' where x='4' and y='4' and author_id='langxw' and type='2' and state='3' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='2' and state='5' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'-10' where x='4' and y='4' and author_id='langxw' and type='2' and state='5' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='3' and state='3' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'0' where x='4' and y='4' and author_id='langxw' and type='3' and state='3' ;">> (id <0.221.0>)
mysql_conn:433: fetch <<"select soldier_sum from soldier where x='4' and y='4' and author_id='langxw' and type='3' and state='5' ;">> (id <0.227.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=soldier_sum+'0' where x='4' and y='4' and author_id='langxw' and type='3' and state='5' ;">> (id <0.225.0>)
mysql_conn:433: fetch <<"update soldier set soldier_sum=0 where soldier_sum<0">> (id <0.221.0>)

=INFO REPORT==== 6-Sep-2013::12:58:25 ===
battle:handel_info 111 Reason=normal
Cover analysis: /home/jason/diudiu/game/mysql_game/.eunit/index.html

=INFO REPORT==== 6-Sep-2013::12:58:25 ===
    application: game
    exited: stopped
    type: temporary
