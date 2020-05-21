local recordtype = require('lua-graph.lua-modules.recordtype')

-- The design of this list is basically as follows
-- An empty doubly linked list looks like:
-- lst = double.new() = {<front> <back>}
-- (where <front> and <back> are unique to the list)
-- lst.front() = <front>
-- lst.back() = <back>
-- <front>.after() = <back>
-- <front>.before() = <front>
-- <back>.after() = <back>
-- <back>.before() = front
-- One can then add into the list:
-- <front>.insert_before('a') => returns a node, now lst = {<front>, a, <back>}
-- <front>.insert_after('a') => returns a node, now lst = {<front>, a, <back>}
-- <back>.insert_before('a') => returns a node, now lst = {<front>, a, <back>}
-- <back>.insert_after('a') => returns a node, now lst = {<front>, a, <back>}
-- lets say you set a_node to the result of <front>.insert_before('a')
-- now a_node.insert_after('b') => now lst = {<front>, a, b, <back>}
-- a_node.insert_before('c') => now lst = {<front>, c, a, b, <back>}
--
-- The whole list has the fields
-- front -- the front
-- back -- the back

--- Doubly linked list type.
local double = {}

--- Metatable for the front of a doubly linked
--- list.
double.front_metatable = {}

--- Tostring for the front of a doubly linked list
function double.front_metatable.__tostring()
   return "<front>"
end

--- Metatable for the back of a doubly linked
--- list.
double.back_metatable = {}

--- Tostring for the back of a doubly linked list
function double.back_metatable.__tostring()
   return "<back>"
end

--- Check if the node NODE is the front of the doubly linked list
function double.is_front(node)
   return getmetatable(node) == double.front_metatable
end

--- Check if the node NODE is the back of the doubly linked list
function double.is_back(node)
   return getmetatable(node) == double.back_metatable
end

--- Check whether NODE is in fact a node.
function double.is_node(node)
   return double.data_node.is(node) or double.is_front(node) or double.is_back(node)
end

--- Take node N, and insert A into a new node after it.
--- Returns the newly created node.
function double.insert_after(n, a)
   assert(double.is_node(n), "n is not a node")
   local c = double.data_node.new({data = a, list = n.list})
   local a = n
   local b = n.after
   c.after = b
   c.before = a
   a.after = c
   b.before = c
   n.list.size = n.list.size + 1
   return c
end

--- Take node N, and insert A into a new node before it.
--- Returns the newly created node.
function double.insert_before(n, a)
   assert(double.is_node(n), "n is not a node")
   return n.before:insert_after(a)
end

--- Take a node N, and delete it in the list.
--- Returns the deleted node.
function double.remove_node(n)
   assert(double.is_node(n), "n is note a node")
   n.before.after = n.after
   n.after.before = n.before

   n.before = nil
   n.after = nil

   n.list.size = n.list.size - 1
   n.list = nil
   return n
end

--- Node data type for nodes that contain data (i.e., not <front> nor <back>)
double.data_node = recordtype.new("node",
                                  {
                                     --- The data attached to the node
                                     data = recordtype.NIL;
                                     --- The list this node is a part of
                                     list = recordtype.NIL;
                                     --- The item after this node
                                     after = recordtype.NIL;
                                     --- The item before this node
                                     before = recordtype.NIL;
                                     ---------------------------------------------------------------------
                                     -- Methods of a node
                                     ---------------------------------------------------------------------
                                     --- Insert after the node
                                     insert_after = double.insert_after;
                                     --- Insert before the node
                                     insert_before = double.insert_before;
                                     --- Delete this node
                                     remove = double.remove_node;
                                  })

--- Metatable for doubly linked list
double.double_metatable = {}

--- Iterator over all of the data
--- in the doubly linked list
function double.double_metatable.__pairs(t)
   local curr = t.front.after
   local back = t.back
   local count = 1

   return function(t, k)
      if curr == back then
         return nil
      else
         local k = count
         local v = curr.data
         count = count + 1
         curr = curr.after
         return k, v
      end
   end
end

--- Index the doubly linked list.
--- This should have terrible runtime, but it
--- would be strange to leave it out.
function double.double_metatable.__index(t, key)
   if type(key) ~= "number" or math.floor(key) ~= key then
      return nil
   else
      for k, v in double.double_metatable.__pairs(t) do
         if k == key then
            return v
         end
      end
   end
end

--- Turn the doubly linked list into a string
function double.double_metatable.__tostring(lst)
   -- First pass: see if there's nothing in the table
   local has_some = lst.front.after ~= lst.back

   if not has_some then
      return "{}"
   end

   local out = "{" .. tostring(lst.front.after.data)

   local curr = lst.front.after.after
   while curr ~= lst.back do
      out = out .. ", " .. tostring(curr.data)
      curr = curr.after
   end

   out = out .. "}"

   return out
end

function double.double_metatable.__len(lst)
   return lst.size
end

--- Make a new doubly linked list
function double.new()
   local out = {}
   out.size = 0

   out.front = {}
   setmetatable(out.front, double.front_metatable)

   out.back = {}
   setmetatable(out.back, double.back_metatable)

   out.front.after = out.back
   out.front.before = out.front
   out.front.list = out
   out.front.insert_after = double.insert_after
   out.front.insert_before = double.insert_after
   out.front.remove = function(_) return out.front end

   out.back.after = out.back
   out.back.before = out.front
   out.back.list = out
   out.back.insert_after = double.insert_before
   out.back.insert_before = double.insert_before
   out.back.remove = function(_) return out.back end

   out = setmetatable(out, double.double_metatable)
   return out
end

--- Make a copy of the list LST,
--- but not the data contained.
function double.shallow_copy(lst)
   local out = double.new()
   for _, v in ipairs(lst) do
      out.back:insert_before(v)
   end
   return out
end


-- TODO move test somewhere
-- local function test_double()
--    local d = double.new()
--    assert(tostring(d) == "{}", "D is: " .. tostring(d))

--    d.front:insert_after("0")
--    assert(tostring(d) == "{0}", "D is: " .. tostring(d))

--    d = double.new()
--    assert(tostring(d) == "{}", "D is: " .. tostring(d))

--    d.front:insert_before("0")
--    assert(tostring(d) == "{0}", "D is: " .. tostring(d))

--    d = double.new()
--    assert(tostring(d) == "{}", "D is: " .. tostring(d))

--    d.front:insert_after("0"):insert_after("1"):insert_after("2"):insert_after("3")
--    assert(tostring(d) == "{0, 1, 2, 3}", "D is: " .. tostring(d))

--    d = double.new()
--    assert(tostring(d) == "{}", "D is: " .. tostring(d))

--    d.back:insert_before("0"):insert_before("1"):insert_before("2"):insert_before("3")
--    assert(tostring(d) == "{3, 2, 1, 0}", "D is: " .. tostring(d))

--    d = double.new()
--    assert(tostring(d) == "{}", "D is: " .. tostring(d))

--    assert(tostring(d.front:remove()) == "<front>")
--    assert(tostring(d) == "{}", "D is: " .. tostring(d))

--    assert(tostring(d.back:remove()) == "<back>")
--    assert(tostring(d) == "{}", "D is: " .. tostring(d))

--    d = double.new()
--    local b = d.front:insert_after('a'):insert_after('b')
--    b:insert_after('c')
--    b:remove()
--    assert(tostring(d) == "{a, c}", "D is: " .. tostring(d))
-- end

-- test_double()

return double
