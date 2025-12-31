local CN = require("lua_cn_runtime_core")

-- C calls
CN.C = {}
CN.C.read_int_list = {}

--[[
CN code mapping
--]]
local Seq = {}

function Seq.Nil()
    return { tag="Nil" }
end

function Seq.Cons(head, tail)
    return { tag="Cons", head=head, tail=tail }
end

function Seq.Equals(a, b)
    return CN.equals(a, b)
end

local function Append(xs, ys)
    if xs.tag == "Nil" then
        return ys
    else
        return Seq.Cons(xs.head, Append(xs.tail, ys))
    end
end

local function IntList(p, spec_mode)
    if p == 0 then
        return Seq.Nil()
    end

    CN.error_stack.Push("take H = RW<struct int_list>(p) " .. p)
    CN.ghost_state.GetOrPutOwnership(p, spec_mode)
    local H, tail_addr = CN.C.read_int_list(p)
    CN.error_stack.Pop()

    CN.error_stack.Push("take tl = IntList(H.tail)")
    local tl = IntList(tail_addr, spec_mode)
    CN.error_stack.Pop()

    return Seq.Cons(H, tl)
end

--[[
Pre/Post conditions

NOTE how much more closely these align to actual CN
--]]

CN.IntList_append = {}

function CN.IntList_append.Precondition(xs, ys)
    CN.frames.Push()

    CN.error_stack.Push("requires take L1 = IntList(xs)")
    CN.frames.SetLocal("L1", IntList(xs, CN.spec_mode.PRE))
    CN.error_stack.Pop()

    CN.error_stack.Push("requires take L2 = IntList(ys)")
    CN.frames.SetLocal("L2", IntList(ys, CN.spec_mode.PRE))
    CN.error_stack.Pop()

    print("Precondition Passed in Lua ")
end

function CN.IntList_append.Postcondition(ret)
    CN.error_stack.Push("ensures take L3 = IntList(return)")
    CN.frames.SetLocal("L3", IntList(ret, CN.spec_mode.POST))
    CN.error_stack.Pop()

    CN.error_stack.Push("ensures L3 == append(L1, L2)")
    -- I think this can be made easier to read by using a metatable that calls Seq.Equals
    -- for using the == operator. Not sure if that's worth the generation complexity though
    if not Seq.Equals(
        CN.frames.GetLocal("L3"), 
        Append(CN.frames.GetLocal("L1"), CN.frames.GetLocal("L2"))) then
            CN.error_stack.Dump()
            return
    end
    CN.error_stack.Pop()

    CN.frames.Pop()

    print("Post Condition Passed in Lua")
    -- Do Post condition Leak Check here
end

return CN