import REPL: REPL, LineEdit, REPLCompletions

function __init__()
  if isdefined(Base, :active_repl)
    repl_init(Base.active_repl)
  else
    atreplinit() do repl
      if isinteractive() && repl isa REPL.LineEditREPL
        isdefined(repl, :interface) || (repl.interface = REPL.setup_interface(repl))
        repl_init(repl)
      end
    end
  end
end

function repl_init(repl)
  main_mode = repl.interface.modes[1]
  pkg_mode = create_mode(repl, main_mode)
  push!(repl.interface.modes, pkg_mode)
  keymap = Dict{Any,Any}(
    '=' => function (s,args...)
      if isempty(s) || position(LineEdit.buffer(s)) == 0
        buf = copy(LineEdit.buffer(s))
        LineEdit.transition(s, pkg_mode) do
          LineEdit.state(s, pkg_mode).input_buffer = buf
        end
      else
        LineEdit.edit_insert(s, '\\')
      end
    end
  )
  main_mode.keymap_dict = LineEdit.keymap_merge(main_mode.keymap_dict, keymap)
  return
end

function create_mode(repl, main)
  pkg_mode = LineEdit.Prompt("vespa> ";
    prompt_prefix = repl.options.hascolor ? Base.text_colors[:magenta] : "",
    prompt_suffix = "",
    on_enter = return_callback,
    sticky = true)

  pkg_mode.repl = repl
  hp = main.hist
  hp.mode_mapping[:vespa] = pkg_mode
  pkg_mode.hist = hp

  search_prompt, skeymap = LineEdit.setup_search_keymap(hp)
  prefix_prompt, prefix_keymap = LineEdit.setup_prefix_keymap(hp, pkg_mode)

  pkg_mode.on_done = (s, buf, ok) -> begin
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
  pkg_mode.keymap_dict = LineEdit.keymap(b)
  return pkg_mode
end

function return_callback(s)
  input = String(take!(copy(LineEdit.buffer(s))))
  return true
end

function repl_eval(repl, input)
  try
    show(evalstring(input))
    println()
  catch e
    Base.showerror(stdout, e)
    println()
  end
end
