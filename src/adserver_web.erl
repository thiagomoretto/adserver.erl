%% @author Thiago Moretto <thiago@moretto.eng.br>
%% @copyright 2012 GerupAd <dev@gerup.com>

-module(adserver_web).
-author("Thiago Moretto <thiago@gerup.com>").

-export([start/1, stop/0, loop/4]).
-define(X_GERUP_AD_UID, "X-Gerup-Aduid").
-define(GERUP_AD_HIT_PATH,      "hit").
-define(GERUP_AD_CONTENT_PATH,  "content").
-define(GERUP_SERVER,           "GerupAdServer").
-define(DEF_REP_HEADER, [{ "Content-Type", "text/plain"}, { "Server", ?GERUP_SERVER }]).

%% External API
start(Options) ->
  {DocRoot, Options1} = get_option(docroot, Options),
  {ok, LogFile} = file:open("views.log", [write, append, {delayed_write, 512, 1000}]),
  {ok, RedisCli} = eredis:start_link(),
  Loop = fun (Req) ->
    ?MODULE:loop(Req, LogFile, RedisCli, DocRoot)
  end,
  mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
  mochiweb_http:stop(?MODULE).

loop(Req, LogFile, RedisCli, _DocRoot) ->
  "/" ++ Path = Req:get(path),
  try
    case Req:get(method) of
      Method when Method =:= 'GET'; Method =:= 'HEAD' ->
        case Path of
          "ad/" ++ AdPath ->
            resolve_adpath(Req, LogFile, RedisCli, AdPath);
            _ ->
              Req:respond({404, ?DEF_REP_HEADER, ""})
              % Req:serve_file(Path, DocRoot)
        end;
      _ ->
        Req:respond({501, [], []})
    end
    catch
      Type:What ->
        Report = ["web request failed",
          {path, Path},
          {type, Type}, {what, What},
          {trace, erlang:get_stacktrace()}],
        error_logger:error_report(Report),
        Req:respond({500, ?DEF_REP_HEADER, ""})
    end.

%% Internal API
resolve_adpath(Req, LogFile, RedisCli, AdPath) ->
  PathElements = re:split(AdPath, "/", [{return,list}]),
  case PathElements of
    [ ApplicationID, AdID, ?GERUP_AD_HIT_PATH ] ->
      % TODO: get Ad UID from header or from Redis.
      count_hit(Req, LogFile, RedisCli, ApplicationID, AdID),
      Req:respond({200, ?DEF_REP_HEADER, ""});
    [ ApplicationID, AdID, ?GERUP_AD_CONTENT_PATH ] ->
      case eredis:q(RedisCli, ["GET", lists:concat([ApplicationID, "-", AdID, "-Invalidate"])]) of
        {ok, <<"TRUE">>} ->
          reply_ad_invalid(Req);
        _ ->
          case eredis:q(RedisCli, 
              ["MGET", 
                lists:concat([ApplicationID, "-", AdID, "-Location"]), 
                lists:concat([ApplicationID, "-", AdID, "-UID"])]) of
            {ok, Values} ->
              case Values of
                [ AdLocation, AdUID ] when AdLocation /= undefined, AdUID /= undefined ->
                  reply_with_ad(Req, AdLocation, AdUID);
                _ -> 
                  reply_ad_not_found(Req)
              end;
            _ ->
              reply_ad_not_found(Req)
          end
        end;
    _ ->
      reply_ad_not_found(Req)
  end.
  
reply_ad_invalid(Req) ->
  Req:respond({205, ?DEF_REP_HEADER, ""}).
  
reply_ad_not_found(Req) ->
  Req:respond({404, ?DEF_REP_HEADER, ""}).
  
reply_with_ad(Req, AdLocation, AdUID) when AdLocation /= nil, AdUID /= nil ->
  Req:respond({302, 
    lists:merge(?DEF_REP_HEADER, [{ "Location", AdLocation }, { ?X_GERUP_AD_UID, AdUID }]), ""}).
  
count_hit(Req, LogFile, RedisCli, ApplicationID, AdID) ->
  HitKey = lists:concat([ApplicationID, "-", AdID, "-HitCount"]),
  ViewsKey = lists:concat([ApplicationID, "-", AdID, "-Views"]),
  {ok, _Hits} = eredis:q(RedisCli, [ "INCR", HitKey ]),
  {ok, AdUID} = eredis:q([ApplicationID, "-", AdID, "-UID"]),
  % device add, device profile, etc
  {Date={Year, Month, Day}, Time={Hour, Minutes, Seconds}} = erlang:localtime().
  io:format(LogFile, "~s-~s-~s ~s:~s,~s,~s,~s,~s,~s,~s,~s~n", 
     [ integer_to_list(Year), 
       integer_to_list(Month), 
       integer_to_list(Day), 
       integer_to_list(Hour), 
       integer_to_list(Minutes), 
       ApplicationID, 
       AdID, 
       AdUID,
       Req:get(peer),
       "DEVICE_UID",
       "DEVICE_IDENTIFIER",
       "USER_DATE_TIME"
     ]).
  
  

get_option(Option, Options) ->
  {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
