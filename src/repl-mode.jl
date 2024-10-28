module ReplMode

using REPL, REPL.LineEdit
using ..Raven

function __init__()
  if isdefined(Base, :active_repl)
    repl_init(Base.active_repl)
  else
    atreplinit() do repl
      isinteractive() && repl isa REPL.LineEditREPL && repl_init(repl)
    end
  end
end

function repl_init(repl)
  main_mode = repl.interface.modes[1]
  rv_mode = create_mode(repl, main_mode)
  push!(repl.interface.modes, rv_mode)
  keymap = Dict{Any,Any}('=' => function (s,args...)
    if isempty(s) || position(LineEdit.buffer(s)) == 0
      buf = copy(LineEdit.buffer(s))
      LineEdit.transition(s, rv_mode) do
        LineEdit.state(s, rv_mode).input_buffer = buf
      end
    else
      LineEdit.edit_insert(s, '=')
    end
  end)
  main_mode.keymap_dict = LineEdit.keymap_merge(main_mode.keymap_dict, keymap)
  return
end

function create_mode(repl, main)
  rv_mode = LineEdit.Prompt("raven> ",
    prompt_prefix = repl.options.hascolor ? Base.text_colors[:magenta] : "",
    prompt_suffix = "",
    on_enter = return_callback,
    sticky = true)

  rv_mode.repl = repl
  hp = main.hist
  hp.mode_mapping[:rv] = rv_mode
  rv_mode.hist = hp

  search_prompt, skeymap = LineEdit.setup_search_keymap(hp)
  prefix_prompt, prefix_keymap = LineEdit.setup_prefix_keymap(hp, rv_mode)

  rv_mode.on_done = (s, buf, ok) -> begin
    ok || return REPL.transition(s, :abort)
    input = String(take!(buf))
    REPL.reset(repl)
    repl_eval(repl, input)
    REPL.prepare_next(repl)
    REPL.reset_state(s)
    s.current_mode.sticky || REPL.transition(s, main)
  end

  mk = REPL.mode_keymap(main)

  b = Dict{Any,Any}[
    skeymap, mk, prefix_keymap, LineEdit.history_keymap,
    LineEdit.default_keymap, LineEdit.escape_defaults
  ]
  rv_mode.keymap_dict = LineEdit.keymap(b)
  return rv_mode
end

function return_callback(s)
  input = String(take!(copy(LineEdit.buffer(s))))
  count(x -> x in ('(', '['), input) <= count(x -> x in (')', ']'), input)
end

repl = nothing

function repl_eval(_, input)
  global repl
  try
    repl == nothing && (repl = Raven.REPL())
    Raven.eval!(repl, input)
  catch
    Base.display_error(stdout, Base.catch_stack())
  end
  return
end

end
