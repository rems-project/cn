local cn = require("lua_cn_runtime_core")

-- C calls
cn.c.read_int_list = {}

--[[
CN code mapping
--]]
local seq = {}

function seq.nill()
    return { tag = "Nil" }
end

function seq.cons(head, tail)
    return { tag = "Cons", head=head, tail=tail }
end

function seq.equals(a, b)
    return cn.equals(a, b)
end

local function append(xs, ys)
    if xs.tag == "Nil" then
        return ys
    else
        return seq.cons(xs.head, append(xs.tail, ys))
    end
end

local function IntList(p, spec_mode)
    if p == 0 then
        return seq.nill()
    end

    local f1, s1, f2, s2, size = cn.c.read_int_list(p);

    cn.error_stack.push("take H = RW<struct int_list>(p) " .. p)
    cn.ghost_state.get_or_put_ownership(spec_mode, p, size)
    local H = cn.c.get_integer(f1);
    cn.error_stack.pop()

    cn.error_stack.push("take tl = IntList(H.tail)")
    local tl = IntList(cn.c.get_pointer(f2), spec_mode)
    cn.error_stack.pop()

    return seq.cons(H, tl)
end

--[[
Pre/Post conditions

NOTE how much more closely these align to actual CN
--]]

cn.IntList_append = {}

function cn.IntList_append.precondition(xs, ys)
    cn.frames.push()

    cn.ghost_state.stack_depth_incr();

    cn.error_stack.push("requires take L1 = IntList(xs)")
    cn.frames.set_local("L1", IntList(xs, cn.spec_mode.PRE))
    cn.error_stack.pop()

    cn.error_stack.push("requires take L2 = IntList(ys)")
    cn.frames.set_local("L2", IntList(ys, cn.spec_mode.PRE))
    cn.error_stack.pop()

    print("Precondition Passed in Lua")
end

function cn.IntList_append.postcondition(ret)
    cn.error_stack.push("ensures take L3 = IntList(return)")
    cn.frames.set_local("L3", IntList(ret, cn.spec_mode.POST))
    cn.error_stack.pop()

    cn.error_stack.push("ensures L3 == append(L1, L2)")
    -- I think this can be made easier to read by using a metatable that calls seq.Equals
    -- for using the == operator. Not sure if that's worth the generation complexity though
    if not seq.equals(
        cn.frames.get_local("L3"), 
        append(cn.frames.get_local("L1"), cn.frames.get_local("L2"))) then
            cn.error_stack.dump()
            return
    end
    cn.error_stack.pop()

    cn.ghost_state.stack_depth_decr();

    cn.ghost_state.postcondition_leak_check();

    cn.frames.pop()

    print("Postcondition Passed in Lua")
end

return cn