%%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%%% use this file except in compliance with the License. You may obtain a copy of
%%% the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%%% License for the specific language governing permissions and limitations under
%%% the License.
%%%-------------------------------------------------------------------
%%% @author Roberto Majadas <roberto.majadas@openshine.com>
%%% @copyright (C) 2012, OpenShine S.L.
%%% @doc
%%%
%%% @end
%%% Created  :  6 Nov 2012 by Roberto Majadas <roberto.majadas@openshine.com>
%%% Modified :  4 Aug 2016 by Anton Panasenko <anton.panasenko@gmail.com>
%%%-------------------------------------------------------------------
-module(kucumberl_feature).
-include("kucumberl.hrl").

%% API
-export([run/1]).

%%%===================================================================
%%% API
%%%===================================================================

run(Feature) ->
  kucumberl_report:log(Feature#feature{scenarios = []}),
  case is_enabled_feature(Feature) of
    true ->
      kucumberl_feature_code:load(Feature),
      kucumberl_log:init_feature(Feature),
      F = run_feature(Feature),
      kucumberl_log:end_feature(),
      kucumberl_feature_code:unload(F);
    false ->
      Feature
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_enabled_feature(Feature) ->
  EnabledScns =
    lists:foldl(
      fun(ScnID, Acc) ->
        Scn = kucumberl_feature_scn:get_scenario(Feature, ScnID),
        case Scn#scenario.enabled of
          true -> Acc + 1;
          false -> Acc
        end
      end,
      0,
      lists:seq(1, length(Feature#feature.scenarios))),
  case EnabledScns of
    0 -> false;
    _ -> true
  end.

run_feature(Feature) ->
  lists:foldl(
    fun(ScnID, F) ->
      Scn = kucumberl_feature_scn:get_scenario(Feature, ScnID),
      case Scn#scenario.enabled of
        true ->
          kucumberl_log:init_scenario(ScnID),
          report(scenario, F, Scn),
          F = kucumberl_feature_scn:run(F, ScnID),
          kucumberl_log:end_scenario(),
          F;
        false ->
          F
      end
    end,
    Feature,
    lists:seq(1, length(Feature#feature.scenarios))).


report(scenario, F, Scn) ->
  Actions = case Scn#scenario.enabled of
    true -> [];
    false -> lists:map(fun(#action{} = Action) ->
      Action#action{
        status = skipped,
        location = F#feature.path,
        featureId = F#feature.id,
        scenarioId = Scn#scenario.id
      }
    end, Scn#scenario.actions)
  end,
  kucumberl_report:log(Scn#scenario{
    featureId = F#feature.id,
    actions = Actions,
    tags = Scn#scenario.tags ++ F#feature.tags
  }).

