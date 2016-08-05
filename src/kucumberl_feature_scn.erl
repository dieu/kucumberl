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
%%% @copyright (C) 2012, Roberto Majadas
%%% @doc
%%%
%%% @end
%%% Created  : 10 Nov 2012 by Roberto Majadas <roberto.majadas@openshine.com>
%%% Modified :  4 Aug 2016 by Anton Panasenko <anton.panasenko@gmail.com>
%%%-------------------------------------------------------------------
-module(kucumberl_feature_scn).
-include("kucumberl.hrl").

%% API
-export([run/2,
  get_scenario/2
]).

%%%===================================================================
%%% API
%%%===================================================================

run(Feature, ScnID) ->
  case get_scenario_type(Feature, ScnID) of
    scenario ->
      F1 = run_scenario_setup(Feature, ScnID, 1),
      F2 = run_scenario_background(F1, ScnID, 1),
      F3 = run_scenario(F2, ScnID, 1),
      run_scenario_teardown(F3, ScnID, 1);
    scenario_out ->
      run_scenario_out(Feature, ScnID)
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

run_scenario_setup(F, ScnID, EID) ->
  kucumberl_feature_act:run(F, {setup, ScnID, EID}).


run_scenario_teardown(F, ScnID, EID) ->
  F1 = kucumberl_feature_act:run(F, {teardown, ScnID, EID}),
  report(tags, F1, ScnID, EID),
  F1.

run_scenario_background(F, ScnID, EID) ->
  case F#feature.background of
    [] -> F;
    BScn ->
      lists:foldl(
        fun(ActID, F1) ->
          NewAct = kucumberl_feature_act:run(F1, {background, ScnID, EID, ActID}),
          report(step, F1, background, ScnID, EID, ActID, NewAct),
          F1
        end,
        F,
        lists:seq(1, length(BScn#scenario.actions)))
  end.

run_scenario(F, ScnID, EID) ->
  S = get_scenario(F, ScnID),

  lists:foldl(
    fun(ActID, F1) ->
      NewAct = kucumberl_feature_act:run(F1, {normal, ScnID, EID, ActID}),
      report(step, F, normal, ScnID, EID, ActID, NewAct),
      F1
    end,
    F,
    lists:seq(1, length(S#scenario.actions))).

run_scenario_out(F, ScnID) ->
  Scn = get_scenario(F, ScnID),

  case Scn#scenario.examples of
    [] -> F;
    [_EHeader | ERows] ->
      lists:foldl(
        fun(EID, Fx) ->
          F1 = run_scenario_setup(Fx, ScnID, EID),
          F2 = run_scenario_background(F1, ScnID, EID),
          F3 = run_scenario(F2, ScnID, EID),
          run_scenario_teardown(F3, ScnID, EID)
        end,
        F,
        lists:seq(1, length(ERows)))
  end.


%%%===================================================================
%%% Util functions
%%%===================================================================

get_scenario(F, ScnID) ->
  lists:nth(ScnID, F#feature.scenarios).

get_scenario_type(F, ScnID) ->
  Scn = get_scenario(F, ScnID),
  Scn#scenario.type.

report(step, F, Type, ScnID, EID, ActID, Act) ->
  [[R]] = ets:match(kctx, {{F#feature.id, Type, ScnID, EID, ActID}, '$1'}),
  Scenario = get_scenario(F, ScnID),
  NewAct = case R of
    ok -> Act#action{status = passed};
    {failed, _Reason} -> Act#action{status = failed, error = _Reason};
    not_implementated -> Act#action{status = pending};
    disabled -> Act#action{status = skipped}
  end,
  kucumberl_report:log(NewAct#action{
    location = F#feature.path,
    featureId = F#feature.id,
    scenarioId = Scenario#scenario.id
  }).

report(tags, F, ScnID, EID) ->
  [[State]] = ets:match(kctx, {{F#feature.id, state, ScnID, EID}, '$1'}),
  case lookup(tags, State) of
    undefined -> ok;
    Value ->
      Scn = get_scenario(F, ScnID),
      kucumberl_report:log({Value, Scn#scenario{
        featureId = F#feature.id
      }})
  end.

lookup(Key, State) ->
  case lists:keysearch(Key, 1, State) of
    {value, {Key, Val}} ->
      Val;
    _ ->
      undefined
  end.

