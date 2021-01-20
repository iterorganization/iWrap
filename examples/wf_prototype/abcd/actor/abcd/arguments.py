from iwrap.arguments import Argument, DiagnosticInfo, Parameters

class ABCDInArguments :
  def __init__(self):
    self.in_arg1 = Argument("IDS", "core_profiles", Argument.IN)
    self.in_arg2 = Argument("IDS", "numeric", Argument.IN)

class ABCDOutArguments :
  def __init__(self):
    self.out_arg1 = Argument("IDS", "equilibrium", Argument.OUT)
    self.out_arg2 = Argument("IDS", "distribution", Argument.OUT)
    self.diagnostic_info = DiagnosticInfo()

