# Trim unreachable code

function trim_unreachable(ir)
  pr = IRTools.Pipe(ir)
  for bl in blocks(pr)
    flag = false
    for (v, st) in bl
      if flag
        delete!(pr, v)
      elseif isexpr(st, :branch)
        br = st.expr
        if br.args[2] == nothing
          flag = true
        else
          cond = exprtype(ir, br.args[2])
          @assert issubset(cond, RType(Bool))
          if cond == RType(false)
            delete!(pr, v)
          elseif cond == RType(true)
            flag = true
            pr[v] = IRTools.branch(br.args[1], arguments(br)...)
          end
        end
      elseif st.type == ⊥
        push!(pr, IRTools.unreachable)
        flag = true
      end
    end
  end
  return IRTools.finish(pr)
end

function fuseable(bl)
  preds = predecessors(bl)
  isempty(preds) || (length(preds) == 1 && length(successors(only(preds))) == 1)
end

function fuseblocks(ir)
  bls = blocks(ir)
  ir = empty(ir)
  skip = Set(i for i = 2:length(bls) if fuseable(bls[i]))
  env = Dict()
  env[0] = 0
  for bl in bls
    bl.id in skip && continue
    bl.id == 1 || block!(ir)
    env[bl.id] = length(blocks(ir))
    for arg in arguments(bl)
      env[arg] = argument!(blocks(ir)[end], nothing, exprtype(bl.ir, arg), insert = false)
    end
    function inlineblock!(ir, bl)
      for (v, st) in bl
        if isexpr(st, :branch) && (target = st.expr.args[1]) in skip
          for (arg, x) in zip(arguments(bls[target]), arguments(st.expr))
            env[arg] = rename(env, x)
          end
          inlineblock!(ir, bls[target])
        else
          env[v] = push!(ir, rename(env, st))
        end
      end
    end
    inlineblock!(ir, bl)
  end
  for (v, st) in ir
    isexpr(st, :branch) && (ir[v] = Expr(:branch, env[st.expr.args[1]], st.expr.args[2:end]...))
  end
  return ir
end

# Simple dynamic binding for recursive types

function dynamic_bind(f, k, v)
  stack = get!(task_local_storage(), k, [])
  try
    push!(stack, v)
    f()
  finally
    pop!(stack)
  end
end

dynamic_value(k) = task_local_storage()[k][end]

function dynamic_value(k, default)
  tls = task_local_storage()
  (!haskey(tls, k) || isempty(tls[k])) ? default :
  tls[k][end]
end

withpath(f, p) = dynamic_bind(f, :path, p)
path() = dynamic_value(:path)

# Compiler options

struct Options
  # checkAllocations() call after main
  memcheck::Bool
  # Use JS interop for error handling
  jspanic::Bool
  # JS interop uses malloc/refcounting
  jsalloc::Bool
  # Allow inlining
  inline::Bool
end

Options(; memcheck = true, jspanic = true, jsalloc = true, inline = true) =
  Options(memcheck, jspanic, jsalloc, inline)

withoptions(f, p) = dynamic_bind(f, :options, p)
options() = dynamic_value(:options, Options())::Options

# Union splitting

function union_downcast!(ir, T::RType, i::Integer, x)
  offset = sum(nregisters, layout.(T.union[1:i-1]), init = 0)+1
  parts = [push!(ir, Expr(:ref, x, j+offset)) for j = 1:nregisters(layout(T.union[i]))]
  return layout(T.union[i]) isa Tuple ? push!(ir, stmt(Expr(:tuple, parts...), type = T.union[i])) : parts[1]
end

# `f` is reponsible for freeing its argument value, but not for freeing `x`
# (since they are the same object)
function union_cases!(f, ir, T::RType, x)
  @assert isfield(T, :union)
  j = push!(ir, Expr(:ref, x, 1))
  for case in 1:length(T.union)
    cond = push!(ir, stmt(xcall(i32.eq, j, Int32(case)), type = RType(Bool)))
    branch!(ir, length(blocks(ir))+1, when = cond)
    branch!(ir, length(blocks(ir))+2)
    block!(ir)
    val = union_downcast!(ir, T, case, x)
    ret = f(T.union[case], val)
    return!(ir, ret)
    block!(ir)
  end
  push!(ir, stmt(xcall(WebAssembly.unreachable), type = ⊥))
  return ir
end

function log!(ir, s::String)
  s = push!(ir, stmt(Expr(:ref, s), type = rlist(String)))
  push!(ir, stmt(xcall(tag"common.println", s), type = nil))
end
