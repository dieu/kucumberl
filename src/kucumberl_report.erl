%%% Licensed under the Apache License, Version 2.0 (the License); you may not
%%% use this file except in compliance with the License. You may obtain a copy of
%%% the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an AS IS BASIS, WITHOUT
%%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%%% License for the specific language governing permissions and limitations under
%%% the License.
%%%-------------------------------------------------------------------
%%% @author Anton Panasenko <anton.panasenko@gmail.com>
%%% @doc
%%%
%%% @end
%%% Created : 8 Jan 2016 by Anton Panasenko <anton.panasenko@gmail.com>
%%%-------------------------------------------------------------------
-module(kucumberl_report).

-behaviour(gen_server).

-include("kucumberl.hrl").

%% API
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3,
  format_status/2
]).

-export([start_link/2]).

-export([
  log/1,
  log/2,
  save/0,
  save/1
]).

-define(REF, ?MODULE).
-define(FEATURE, str("Feature")).

-record(state, {
  foramt = json,
  output = none,
  report = []
}).

start_link(Format, Output) ->
  gen_server:start_link({local, ?REF}, ?MODULE, [Format, Output], []).

log(Item) ->
  log(?REF, Item).

log(Ref, #feature{} = Feature) ->
  gen_server:cast(Ref, {feature, Feature});

log(Ref, #scenario{} = Scenario) ->
  gen_server:cast(Ref, {scenario, Scenario});

log(Ref, #action{} = Step) ->
  gen_server:cast(Ref, {step, Step}).

save() ->
  save(?REF).

save(Ref) ->
  gen_server:call(Ref, {save}).

init([Format, Output]) ->
  {ok, #state{foramt = Format, output = Output, report = []}}.

handle_call({save}, _From, #state{output = none} = State) ->
  {reply, file_not_found, State};


handle_call({save}, _From, #state{report = Report, foramt = json, output = Output} = State) ->
  Features = lists:map(fun feature/1, lists:reverse(Report)),
  {ok, Json} = jsone_encode:encode(Features, [{indent, 1}, {space, 2}]),
  file:write_file(Output, io_lib:fwrite("~s\n", [Json])),
  {reply, ok, State}.

handle_cast({feature, #feature{id = Id} = Feature}, #state{report = Report} = State) ->
  NewReport = put(Id, 2, Feature, Report),
  {noreply, State#state{report = NewReport}};

handle_cast({scenario, #scenario{featureId = FeatureId, id = Id} = Scenario}, #state{report = Report} = State) ->
  NewReport = case lists:keyfind(FeatureId, 2, Report) of
               #feature{} = Feature ->
                 lists:keyreplace(FeatureId, 2, Report, Feature#feature{
                   scenarios = put(Id, 2, Scenario, Feature#feature.scenarios)
                 })
             end,
  {noreply, State#state{report = NewReport}};

handle_cast({step, #action{featureId = FeatureId, scenarioId = ScenarioId} = Step}, #state{report = Report} = State) ->
  NewReport = case lists:keyfind(FeatureId, 2, Report) of
               #feature{} = Feature ->
                 NewScenario = case lists:keyfind(ScenarioId, 4, Feature#feature.scenarios) of
                                 #scenario{} = Scenario ->
                                   Scenario#scenario{
                                     actions = [Step | Scenario#scenario.actions]
                                   }
                               end,
                 lists:keyreplace(FeatureId, 2, Report, Feature#feature{
                   scenarios = lists:keyreplace(ScenarioId, 4, Feature#feature.scenarios, NewScenario)
                 })
             end,
  {noreply, State#state{report = NewReport}}.

handle_info(_Info, _State) ->
  erlang:error(not_implemented).

terminate(normal, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

format_status(_Opt, _StatusData) ->
  erlang:error(not_implemented).

put(Id, Pos, Item, List) ->
  case lists:keyfind(Id, Pos, List) of
    false ->
      [Item | List]
  end.

feature(#feature{} = Feature) -> #{
  uri => str(Feature#feature.path),
  keyword => ?FEATURE,
  id => str(Feature#feature.id),
  name => str(Feature#feature.name),
  line => str(Feature#feature.line),
  description => str(Feature#feature.desc),
  tags => lists:map(fun tag/1, Feature#feature.tags),
  elements => lists:map(fun scenario/1, lists:reverse(Feature#feature.scenarios))
}.

scenario(#scenario{type = scenario} = Scenario) -> #{
  keyword => keyword(Scenario),
  id => str(Scenario#scenario.id),
  name => str(Scenario#scenario.name),
  line => str(Scenario#scenario.line),
  description => str(Scenario#scenario.desc),
  tags => lists:map(fun tag/1, Scenario#scenario.tags),
  type => type(Scenario),
  steps => lists:map(fun step/1, lists:reverse(Scenario#scenario.actions))
};

scenario(#scenario{type = scenario_out} = Scenario) ->
  Actions = lists:reverse(Scenario#scenario.actions),
  lists:map(fun({Index, Example}) ->
    scenario(Scenario#scenario {
      type = scenario,
      actions = lists:reverse(lists:sublist(Actions, length(Example) * Index +1, length(Example)))
    })
  end, lists:zip(lists:seq(0, length(Scenario#scenario.examples)-2), tl(Scenario#scenario.examples))).

step(#action{} = Step) -> #{
  keyword => keyword(Step),
  name => str(Step#action.desc),
  line => str(Step#action.line),
  match => #{
    location => str(Step#action.location)
  },
  result => result(Step)
}.

tag(Tag) -> #{
  name => str(Tag),
  line => null
}.

keyword(#action{step = Step}) ->
  case Step of
    given_step -> str("Given ");
    when_step -> str("When ");
    then_step -> str("Then ");
    and_step -> str("And ")
  end;

keyword(#scenario{type = Type}) ->
  case Type of
    scenario -> str("Scenario");
    scenario_out -> str("Scenario Outline")
  end.

type(#scenario{type = Type}) ->
  case Type of
    scenario -> str("scenario");
    scenario_out -> str("scenario_outline")
  end.

result(#action{error = none, status = Status}) -> #{
  status => str(Status),
  duration =>  1
};

result(#action{error = Error, status = Status}) -> #{
  status => str(Status),
  error_message => str(Error),
  duration => 1
}.

str(Str) when is_list(Str) ->
  list_to_bitstring(Str);

str(Any) ->
  Any.
