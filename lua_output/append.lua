local CN = require("lua_cn_runtime_core")

--[[
C calls. Will be linked by the embedded C Code

For the most part, our CN runtime boundary between C and Lua is one-directional
i.e. C calls into Lua and Lua does the thing required. This is the one exception.
Our Lua implementation of the CN IntList needs to recursively traverse the C
int_list and craft an ownership-verified CN seq out of it. This requires being able
to deduce the C int_list values. 

In the C runtime, owned_struct_int_list is basically
the main function where the interaction between C and CN happens - but it feels easier
because both CN and C live in C land. 

Here, our CN is in Lua, so we need some way of talking back to C to gather the data we need. 
This is why this table entry exists and is formally linked to a C function when we setup
our Lua state (right now in main() -> extrapolate to whichever place we setup our
Lua runtime).
--]] 
CN.C.read_int_list = {}
CN.IntList_append = {}

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

    CN.Error.Push("take H = RW<struct int_list>(p) " .. p)
    CN.Ghost.GetOrPutOwnership(p, spec_mode)
    local H, tail_addr = CN.C.read_int_list(p)
    CN.Error.Pop()

    CN.Error.Push("take tl = IntList(H.tail)")
    local tl = IntList(tail_addr, spec_mode)
    CN.Error.Pop()

    return Seq.Cons(H, tl)
end

--[[
Pre/Post conditions

NOTE how much more closely these align to actual CN
--]]

function CN.IntList_append.Precondition(xs, ys)
    CN.Frame.Push()

    CN.Error.Push("requires take L1 = IntList(xs)")
    CN.Frame.SetLocal("L1", IntList(xs, CN.spec_mode.PRE))
    CN.Error.Pop()

    CN.Error.Push("requires take L2 = IntList(ys)")
    CN.Frame.SetLocal("L2", IntList(ys, CN.spec_mode.PRE))
    CN.Error.Pop()

    print("Precondition Passed in Lua ")
end

function CN.IntList_append.Postcondition(ret)
    CN.Error.Push("ensures take L3 = IntList(return)")
    CN.Frame.SetLocal("L3", IntList(ret, CN.spec_mode.POST))
    CN.Error.Pop()

    CN.Error.Push("ensures L3 == append(L1, L2)")
    -- I think this can be made easier to read by using a metatable that calls Seq.Equals
    -- for using the == operator. Not sure if that's worth the generation complexity though
    if not Seq.Equals(
        CN.Frame.GetLocal("L3"), 
        Append(CN.Frame.GetLocal("L1"), CN.Frame.GetLocal("L2"))) then
            CN.Error.Dump()
    end
    CN.Error.Pop()

    CN.Frame.Pop()

    print("Post Condition Passed in Lua")
    -- Do Post condition Leak Check here
end

return CN