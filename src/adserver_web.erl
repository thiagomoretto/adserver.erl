%% @author Thiago Moretto <thiago@moretto.eng.br>
%% @copyright 2012 GerupAd <dev@gerup.com>

-module(adserver_web).
-author("Thiago Moretto <thiago@gerup.com>").

-export([start/1, stop/0, loop/2]).
-define(X_GERUP_AD_UID, "X-Gerup-Aduid").
-define(GERUP_AD_HIT_PATH,      "hit").
-define(GERUP_AD_CONTENT_PATH,  "content").
-define(GERUP_SERVER,           "GerupAdServer").

%% External API
start(Options) ->
  {DocRoot, Options1} = get_option(docroot, Options),
  Loop = fun (Req) ->
    ?MODULE:loop(Req, DocRoot)
  end,
  mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
  mochiweb_http:stop(?MODULE).

loop(Req, _DocRoot) ->
  "/" ++ Path = Req:get(path),
  try
    case Req:get(method) of
      Method when Method =:= 'GET'; Method =:= 'HEAD' ->
        case Path of
          "ad/" ++ AdPath ->
            resolve_adpath(Req, AdPath);
            _ ->
              Req:respond({404, [{ "Content-Type", "text/plain"}], "Nothing todo here!"})
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
        Req:respond({500, [{"Content-Type", "text/plain"}], ""})
    end.

%% Internal API
resolve_adpath(Req, AdPath) ->
  PathElements = re:split(AdPath, "/", [{return,list}]),
  case PathElements of
    [ ApplicationID, AdID, ?GERUP_AD_HIT_PATH ] ->
      HitKey = lists:concat([ApplicationID, "-", AdID, "-HitCount"]),
      {ok, RedisCli} = eredis:start_link(),
      {ok, _Hits} = eredis:q(RedisCli, [ "INCR", HitKey ]),
      Req:respond({200, [{ "Content-Type", "text/plain"}, { "Server", ?GERUP_SERVER }], ""});
    [ ApplicationID, AdID, ?GERUP_AD_CONTENT_PATH ] ->
      {ok, RedisCli} = eredis:start_link(),
      case eredis:q(RedisCli, 
          ["MGET", 
            lists:concat([ApplicationID, "-", AdID, "-Location"]), 
            lists:concat([ApplicationID, "-", AdID, "-UID"])]) of
        {ok, Values} ->
          case Values of
            [ AdLocation, AdUID ] when AdLocation /= undefined, AdUID /= undefined ->
              reply_with_ad(Req, AdLocation, AdUID);
            _ -> 
              ad_not_found(Req)
          end;
        _ ->
          ad_not_found(Req)
      end;
    _ ->
      ad_not_found(Req)
  end.
  
ad_not_found(Req) ->
  Req:respond({404, [{ "Content-Type", "text/plain"}], "Not found\n"}).
  
reply_with_ad(Req, AdLocation, AdUID) when AdLocation /= nil, AdUID /= nil ->
  Req:respond({302, [
    { "Content-Type" , "text/plain" },
    { "Location",      AdLocation },
    { "Server",       ?GERUP_SERVER },
    { ?X_GERUP_AD_UID, AdUID }
  ], ""}).

get_option(Option, Options) ->
  {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.
