local DeepCompare = require("fast_deep_compare")

--[[
Our CN runtime state. This exists as nested tables, each corresponding
to a main part of our CN runtime that we're implementing in Lua:

- Error Stack: Stack of errors that users can push/pop to ensure helpful error messaging
- Frames: Holds any allocations that must persist for the duration of the frame (right now, the
  main usage is any CN pre condition variables that are later used during the post condition). Each
  frame is keyed to the current stack depth, meaning when the frame is popped at the end of the function,
  all frame-specific variables are easily cleaned up with GC. I initially also thought I'd use this to map
  C vars/addresses into CN types (something we discussed in our last call) but I haven't needed to do that...yet.
- Ghost state: Cororallary to the ownership-ghost-state
- Current Stack Depth: Self explanatory
- Spec Mode: ENUM style mapping of spec modes for usage later

NOTE: This file crafts the core runtime under the CN table. Later on, we add to this table when we
generate our file specific Lua runtime code (in this case append.cn.lua). To the outside consumer (i.e. C),
the entire thing falls under CN.
--]]

local CN = {
    error_stack = {},
    frames = {},
    ghost_state = {},
    current_stack_depth = 0,
    spec_mode = {
        PRE  = 0,
        POST = 1
    },
    equals = DeepCompare,
    Error = {},
    Frame = {},
    Ghost = {},
    C = {}
}

--[[
ERROR HANDLING
--]]

function CN.Error.Push(msg)
    table.insert(CN.error_stack, msg)
end

function CN.Error.Pop()
    table.remove(CN.error_stack)
end

function CN.Error.Dump()
    error(table.concat(CN.error_stack, '\n'))
end

--[[
FRAME
--]]

function CN.Frame.Push()
    CN.current_stack_depth = CN.current_stack_depth + 1
    CN.frames[#CN.frames + 1] = {}
end

function CN.Frame.Pop()
    CN.frames[#CN.frames] = nil
    CN.current_stack_depth = CN.current_stack_depth - 1
end

local function GetCurrentFrame()
    return CN.frames[#CN.frames]
end

function CN.Frame.SetLocal(name, value)
    GetCurrentFrame()[name] = value
end

function CN.Frame.GetLocal(name)
    return GetCurrentFrame()[name]
end

--[[
OWNERSHIP GHOST STATE
--]]

function CN.Ghost.Add(base_addr, size)    
    if CN.Ghost.IsOwned(base_addr, size, CN.current_stack_depth) then
        CN.Error.Push("duplicate ownership of address " .. base_addr)
        CN.Error.Dump()  
    end

    -- Range Map-ish entry. Alternative would be to add an entry per
    -- address offset, which might bloat the table fast and would be a step backwards
    -- in terms of perf. Still, this is very early days. Think about it some more.
    table.insert(CN.ghost_state, {
        base  = base_addr,
        size  = size,
        depth = CN.current_stack_depth
    })
end

function CN.Ghost.Remove(base_addr, size)
    for i, r in ipairs(CN.ghost_state) do
        if r.base == base_addr and r.size == size then
            table.remove(CN.ghost_state, i)
            return
        end
    end

    CN.Error.Push("removing non-owned region " .. addr)
    CN.Error.Dump()
end

function CN.Ghost.UpdateDepth(base_addr, stack_depth)
    for i, r in ipairs(CN.ghost_state) do
        if r.base == base_addr then
            CN.ghost_state[i].depth = stack_depth
            return
        end
    end

    CN.Error.Push("cannot update non-owned region " .. base_addr)
    CN.Error.Dump()
end

function CN.Ghost.IsOwned(addr, size, stack_depth)
    for _, r in ipairs(CN.ghost_state) do
      if addr >= r.base and addr < (r.base + r.size) and stack_depth == r.depth then
        return true
      end
    end
    
    return false
end

function CN.Ghost.GetOwnership(base_addr)
    CN.Ghost.UpdateDepth(base_addr, CN.current_stack_depth)
end

function CN.Ghost.PutOwnership(base_addr)
    CN.Ghost.UpdateDepth(base_addr, CN.current_stack_depth - 1)
end

function CN.Ghost.GetOrPutOwnership(base_addr, spec_mode)
    if spec_mode == CN.spec_mode.PRE then
        CN.Ghost.GetOwnership(base_addr)
    elseif spec_mode == CN.spec_mode.POST then
        CN.Ghost.PutOwnership(base_addr)
    else
        -- do loop ownership stuff
    end
end

return CN
