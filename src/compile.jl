vseval(ex) = eval(__main__, ex)

macro vs_str(s)
  :(vseval($(Expr(:quote, parse(s)))))
end
