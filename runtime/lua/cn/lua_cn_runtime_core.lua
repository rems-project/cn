local deep_compare = require("fast_deep_compare")

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
the entire thing falls under cn.
--]]

local cn = {
    error_stack = {},
    frames = {},
    ghost_state = {},
    spec_mode = {
        PRE  = 1,
        POST = 2,
        LOOP = 3,
        STATEMENT = 4,
        C_ACCESS = 5,
        NON_SPEC = 6
    },
    equals = deep_compare,
    c = {
        -- c asserts
        assert = {},

        -- c ghost state
        add_to_ghost_state = {},
        remove_from_ghost_state = {},
        get_or_put_ownership = {},
        ghost_state_depth_incr = {},
        ghost_state_depth_decr = {},
        postcondition_leak_check = {},

        -- c error handling
        update_error_msg_info = {},
        pop_msg_info = {},
        dump_error_msgs = {},

        -- c types reading
        get_integer = {},
        get_pointer = {}
    }
}

function cn.assert(cond, spec_mode)
    cn.c.assert(cond, spec_mode);
end

--[[
ERROR HANDLING
--]]

function cn.error_stack.push(msg)
    cn.c.update_error_msg_info(msg)
end

function cn.error_stack.pop()
    cn.c.pop_msg_info()
end

function cn.error_stack.dump()
    cn.c.dump_error_msgs();
end

--[[
FRAME
--]]

function cn.frames.push()
    cn.frames[#cn.frames + 1] = {}
    cn.c.ghost_state_depth_incr()
end

function cn.frames.pop()
    cn.c.ghost_state_depth_decr()
    cn.c.postcondition_leak_check()
    cn.frames[#cn.frames] = nil
end

local function get_current_frame()
    return cn.frames[#cn.frames]
end

function cn.frames.set_local(name, value)
    get_current_frame()[name] = value
end

function cn.frames.get_local(name)
    return get_current_frame()[name]
end

--[[
OWNERSHIP GHOST STATE
--]]

function cn.ghost_state.get_or_put_ownership(mode, base_addr, size)
    cn.c.get_or_put_ownership(mode, base_addr, size, 0)
end

function cn.ghost_state.stack_depth_incr()
    return cn.c.ghost_state_depth_incr()
end

function cn.ghost_state.stack_depth_decr()
    return cn.c.ghost_state_depth_decr()
end

function cn.ghost_state.postcondition_leak_check()
    cn.c.postcondition_leak_check();
end

return cn
