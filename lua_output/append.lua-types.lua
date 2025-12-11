Seq = {}

function Seq.Nil()
    return { tag="Nil" }
end

function Seq.Cons(head, tail)
    return { tag="Cons", head=head, tail=tail }
end

function append(xs, ys)
    if xs.tag == "Nil" then
        return ys
    else
        return Seq.Cons(xs.head, append(xs.tail, ys))
    end
end

function IntList(p, mode)
    --[[
    I think we'll likely have to create a pointer type/class in Lua to track the address
    and provenance of a pointer?
    --]]
    if Pointer.is_null(p) then
        return Seq.Nil()
    end

    local H = -- figure out how this part should work. 

    local tl = IntList(H.tail, mode)

    return Seq.Cons(H.head, tl)
end

function seq_equal(a, b)
    --[[
    this will be a core util function that can generically compare any
    two user-defined types (since each user-defined type in Lua will be a table,
    a deepEquals lua algo should work generically I think. Example like this:
    https://github.com/luvit/luvit/blob/master/tests/libs/deep-equal.lua)
    --]]
    return deep_equal(a, b)
end
