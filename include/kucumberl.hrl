-record(module, {
  path,
  mod = [],
  warnings = [],
  errors = []
}).

-record(feature, {
  id,
  path,
  desc = " ",
  name = none,
  line = none,
  background = [],
  scenarios = [],
  fcode = [],
  tags = []
}).

-record(scenario, {
  type,
  desc = " ",
  id = none,
  featureId = none,
  name = none,
  line = none,
  actions = [],
  examples = [],
  tags = [],
  enabled = true
}).

-record(action, {
  step,
  desc,
  line,
  featureId = none,
  scenarioId = none,
  keyword = none,
  status = none,
  error = none,
  location = undefined,
  text = "",
  table = [],
  tabletxt = ""
}).

-record(feature_code, {
  modules = [],
  steps = [],
  setup_mod = [],
  teardown_mod = [],
  status = ok
}).
