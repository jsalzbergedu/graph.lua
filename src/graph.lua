recordtype = require('lua-graph.lua-modules.recordtype')
list = require('lua-graph.lua-modules.list')
double = require('lua-graph.double')

local graph = {}


--- Get the edge from vertex A to vertex B
local function get_edge(a, b)
   assert(graph.vertex.is(a), "A is not a vertex")
   assert(graph.vertex.is(b), "B is not a vertex")
   assert(a.graph == b.graph, "A and B are not in the same graph")
   return a.adjacencies[b]
end

--- Remove vertex V from its graph,
--- and clear out its references.
local function remove_vertex(v)
   -- do nothing if this vertex is already
   -- removed
   if v.graph == nil then
      return
   end

   for other_vertex, edge in pairs(v.adjacencies) do
      edge:remove()
      other_vertex.incoming_edges[v] = nil
   end
   for _, edge in ipairs(v.incoming_edges) do
      edge:remove()
   end
   v.node:remove()
   v.data = nil
   v.adjacencies = nil
   v.edges = nil
   v.graph = nil
   incoming_edges = nil
   v.node = nil
end

--- TODO rewrite so that data is private

graph.vertex = recordtype.new("vertex",
                              {
                                 --- The data stored in the vertex
                                 data = recordtype.NIL;
                                 --- A map of adjacent verticies to the edges.
                                 adjacencies = {};
                                 --- A map of edges to adjacent verticies
                                 edges = {};
                                 --- Which graph this vertex belongs to
                                 graph = recordtype.NIL;
                                 --- Incoming edges, a map from the
                                 --- vertex opposite this one to edges.
                                 incoming_edges = {};
                                 --- The node in the verticies list that
                                 --- corresponds to this vertex
                                 node = recordtype.NIL;
                                 --- The degree of this vertex
                                 --- (number of outgoing edges)
                                 degree = 0;
                                --------------------------------------------------------------------
                                -- Methods of a vertex
                                --------------------------------------------------------------------
                                 --- Get edge from this vertex to the next
                                 get_edge = get_edge;
                                 --- Remove this vertex
                                 remove = remove_vertex;
                              },
                              -- must have a constructor because
                              -- adjacencies and edges must be set to
                              -- _new_ empty tables
                              function(t)
                                 local new_vtx = {}
                                 new_vtx.adjacencies = {}
                                 new_vtx.edges = {}
                                 new_vtx.incoming_edges = {}
                                 if t ~= nil then
                                    for k, v in pairs(t) do
                                       new_vtx[k] = v
                                    end
                                 end
                                 local constructed = graph.vertex.factory(new_vtx)
                                 local mt = getmetatable(constructed)
                                 local nmt = {}
                                 for k, v in pairs(mt) do
                                    nmt[k] = v
                                 end
                                 function nmt.__eq(v1, v2)
                                    return v1.data == v2.data
                                 end
                                 function nmt.__lt(v1, v2)
                                    return v1.data < v2.data
                                 end
                                 function nmt.__le(v1, v2)
                                    return v1.data <= v2.data
                                 end
                                 setmetatable(constructed, nmt)
                                 return constructed
                              end)

--- Remove edge E from the graph,
--- and clear out its fields.
function remove_edge(e)
   if e.graph == nil then
      return
   end

   e.data = nil
   e.from.adjacencies[e.to] = nil
   e.to.incoming_edges[e.from] = nil
   e.from.edges[e] = nil
   e.from.degree = e.from.degree - 1
   e.graph = nil
   e.node:remove()
end

--- The graph edge type
graph.edge = recordtype.new("edge",
                            {
                               --- The data stored in the edge
                               data = recordtype.NIL;
                               --- The starting endpoint
                               from = recordtype.NIL;
                               --- The ending endpoint
                               to = recordtype.NIL;
                               --- Which graph this edge belongs to
                               graph = recordtype.NIL;
                               --- The node in the edge list
                               --- that corresponds to this edge
                               node = recordtype.NIL;
                               ---------------------------------------------------------------------
                               -- Methods
                               ---------------------------------------------------------------------
                               --- Remove this edge
                               remove = remove_edge;

                            },
                            function(t)
                               local new_edge = {}
                               if t ~= nil then
                                  for k, v in pairs(t) do
                                     new_edge[k] = v
                                  end
                               end
                               local constructed = graph.edge.factory(new_edge)
                               local mt = getmetatable(constructed)
                               local nmt = {}
                               for k, v in pairs(mt) do
                                  nmt[k] = v
                               end
                               function nmt.__lt(e1, e2)
                                  return e1.data < e2.data
                               end
                               function nmt.__eq(e1, e2)
                                  return e1.data == e2.data
                               end
                               function nmt.__le(e1, e2)
                                  return e1.data <= e2.data
                               end
                               setmetatable(constructed, nmt)
                               return constructed
                            end)


--- Using the graph G, insert X as vertex data.
--- Return the vertex created.
local function insert_vertex(g, x)
   assert(graph.graph.is(g), "The object " .. tostring(g) .. " is not a graph")
   local vertex = graph.vertex.new({data = x})
   vertex.graph = g;
   vertex.node = g.__verticies.back:insert_before(vertex)
   return vertex
end

--- Using the graph G and the verticies A and B
--- insert an edge from A to B with the edge data X.
--- X can only be retrieved by knowing A and B.
--- Returns the newly created edge.
--- Does nothing if an edge is already there.
local function insert_edge(g, a, b, x)
   assert(graph.graph.is(g), "The object " .. tostring(g) .. " is not a graph")
   assert(a.graph == g, "The vertex a is not in the graph")
   assert(b.graph == g, "The vertex b is not in the graph")

   if a.adjacencies[b] ~= nil then
      return a.adjacencies[b]
   end

   -- Set up the edge
   local edge = graph.edge.new({data = x, from = a, to = b})
   edge.graph = g
   edge.node = g.__edges.back:insert_before(edge)

   -- Add the edge to A
   a.edges[edge] = b
   a.adjacencies[b] = edge
   a.degree = a.degree + 1

   -- Register with B that there is an incoming edge
   -- (Important for vertex removal)
   b.incoming_edges[#b.incoming_edges + 1] = edge

   return edge
end

--- Using graph G, get an iterator over all the
--- edges.
local function edges(g)
   local it = pairs(g.__edges)
   local k = nil
   local v = nil
   return function(t, k)
      k, v = it(g, k)
      if k == nil then
         return nil
      else
         return v
      end
   end
end

--- Using graph G, get an iterator over all the
--- verticies
local function verticies(g)
   local it = pairs(g.__verticies)
   local k = nil
   local v = nil
   return function(t, k)
      k, v = it(g, k)
      if k == nil then
         return nil
      else
         return v
      end
   end
end


local function print_graph(g)
   assert(graph.graph.is(g), "The object " .. tostring(g) .. " is not a graph")

   local num_verticies = 0
   local num_edges = 0

   local cell_width;

   local vertex_max_size;

   for vertex in g:verticies() do
      cell_width = #tostring(vertex.data)
      vertex_max_size = #tostring(vertex.data)
      break
   end

   for vertex in g:verticies() do
      local vtx_curr_len = #tostring(vertex.data)
      if vtx_curr_len > cell_width then
         cell_width = vtx_curr_len
      end

      if vtx_curr_len > vertex_max_size then
         vertex_max_size = vtx_curr_len
      end
      num_verticies = num_verticies + 1
   end

   for edge in g:edges() do
      local edge_curr_len = #tostring(edge.data)

      if edge_curr_len > cell_width then
         cell_width = edge_curr_len
      end

      num_edges = num_edges + 1
   end

   print("Graph with " .. tostring(num_verticies) .. " verticies and " .. tostring(num_edges) .. " edges")

   if num_verticies == 0 then
      return
   end

   -- add a bit of extra padding so words
   -- arent squished together
   cell_width = cell_width + 1

   -- add a single empty cell label
   for i = 1, vertex_max_size, 1 do
      io.write(" ")
   end

   -- print an extra space
   io.write(" ")

   -- write horizontal cell labels
   for vertex in g:verticies() do
      io.write(string.format("%-" .. tostring(cell_width) .. "s", tostring(vertex.data)))
   end

   print("")

   for vertex in g:verticies() do
      io.write(string.format("%" .. tostring(vertex_max_size) .. "s", tostring(vertex.data)))
      io.write(" ")
      for other_vertex in g:verticies() do
         local cell_data = vertex.adjacencies[other_vertex]
         cell_data = (cell_data ~= nil and cell_data.data) or nil
         cell_data = tostring(cell_data)
         io.write(string.format("%-" .. tostring(cell_width) .. "s", tostring(cell_data)))
      end
      print("")
   end

end

local function num_verticies(g)
   return #g.__verticies
end

local function num_edges(g)
   return #g.__edges
end

--- A graph type.
graph.graph = recordtype.new("graph",
                             {
                                --- The verticies of a graph
                                __verticies = double.new();
                                --- The edges of this graph
                                __edges = double.new();
                                --------------------------------------------------------------------
                                -- Methods of a graph
                                --------------------------------------------------------------------
                                --- Insert a vertex into the graph
                                insert_vertex = insert_vertex;
                                --- Insert an edge into the graph
                                insert_edge = insert_edge;
                                --- Get the edges of the graph
                                edges = edges;
                                --- Get the verticies of this graph
                                verticies = verticies;
                                --- Print tihs graph
                                print = print_graph;
                                --- Get the number of edges
                                num_edges = num_edges;
                                --- Get the number of verticies
                                num_verticies = num_verticies;
                             },
                             function(t)
                                 local new_graph = {}
                                 if t ~= nil then
                                    for k, v in pairs(t) do
                                       new_graph[k] = v
                                    end
                                 end
                                 new_graph.__verticies = double.new();
                                 new_graph.__edges = double.new();
                                 return graph.graph.factory(new_graph)
                             end)

---------------------------------------------------------------------------------------------------
-- Utility methods
---------------------------------------------------------------------------------------------------
--- Get a shallow copy of the graph G,
--- but not the data contained.
function graph.shallow_copy(g)
   local out = graph.graph.new()
   local verticies = {}

   for vertex in g:verticies() do
      verticies[vertex] = out:insert_vertex(vertex.data)
   end

   for vertex in g:verticies() do
      for other_vertex, edge in pairs(vertex.adjacencies) do
         out:insert_edge(verticies[vertex], verticies[other_vertex], edge.data)
      end
   end

   return out
end

--- Get the plantuml diagram
--- of graph G.
--- If file F is given, write to that file.
function graph.plantuml(g, f)
   if f == nil then
      f = io.stdout
   end
   assert(graph.graph.is(g), "G is not a graph.")
   found_edges = {}
   for vertex in g:verticies() do
      f:write("(", tostring(vertex.data), ")", "\n")
      for other_vertex in g:verticies() do
         if vertex.adjacencies[other_vertex] then
            f:write("(",
                    tostring(vertex.data),
                    ")",
                    " --> ",
                    "(",
                    tostring(other_vertex.data),
                    ")",
                    " : ",
                    tostring(vertex.adjacencies[other_vertex].data),
                    "\n")
         end
      end
   end
end

--- Compute the transpose of a graph,
--- returning it as a new graph where each vertex
--- and edge refers to the verticies
--- and edges of the old graph
function graph.transpose(g)
   assert(graph.graph.is(g), "G is not a graph")
   local out = graph.graph.new()
   local found_verticies = {}

   for vertex in g:verticies() do
      found_verticies[vertex] = out:insert_vertex(vertex)
   end

   for edge in g:edges() do
      out:insert_edge(found_verticies[edge.to], found_verticies[edge.from], edge)
   end

   return out
end

--- Find the transitive closure
--- of a graph.
--- Return it as a whole new graph,
--- whose verticies contain the verticies
--- of the original graph, and whose edges
--  and whose edges are set to true..
--- This is also known as "floyd-warshall's algorithm"
--- TODO Give credit somehow
function graph.transitive_closure(g)
   assert(graph.graph.is(g), "G is not a graph")
   local out_matrix = {}
   local verticies = {}
   local out_graph = graph.graph.new()
   for vertex in g:verticies() do
      verticies[#verticies + 1] = vertex
   end

   for i = 1, #verticies, 1 do
      out_matrix[i] = {}
      for j = 1, #verticies, 1 do
         out_matrix[i][j] = false
      end
   end

   for i, vertex in ipairs(verticies) do
      for j, other_vertex in ipairs(verticies) do
         if vertex.adjacencies[other_vertex] ~= nil then
            out_matrix[i][j] = true
         end
      end
   end

   for i = 1, #verticies, 1 do
      for j = 1, #verticies, 1 do
         for k = 1, #verticies, 1 do
            out_matrix[j][k] = out_matrix[j][k] or (out_matrix[j][i] and out_matrix[i][k])
         end
      end
   end

   local found_verticies = {}

   for i = 1, #verticies, 1 do
      for j = 1, #verticies, 1 do
         if out_matrix[i][j] then
            if found_verticies[verticies[i]] == nil then
               found_verticies[verticies[i]] = out_graph:insert_vertex(verticies[i])
            end

            if found_verticies[verticies[j]] == nil then
               found_verticies[verticies[j]] = out_graph:insert_vertex(verticies[j])
            end

            out_graph:insert_edge(found_verticies[verticies[i]],
                                  found_verticies[verticies[j]],
                                  true)
         end
      end
   end

   return out_graph
end

--- Get a spanning tree of the graph G
--- starting at vertex V using
--- breadth first search.
--- Return the tree as a graph.
--- If your graph was
--- ({"apple", "carrot", "bannana"},
---  {("apple", "carrot", "apple to carrot"),
---   ("carrot", "bannana", "carrot to bannana"),
---   ("bannana", "apple", "apple to bannana")})
--- and ran this algorithm starting at apple,
--- It would return something that looks like this:
--- ({"apple", "carrot", "bannana"},
---  {("apple", "carrot", true),
---   ("carrot", "bannana", true)}
--- (Of course wrapped in another layer of vertex objects)
function graph.spanning_tree(v)
   assert(graph.vertex.is(v), "V is not a vertex")

   local out = graph.graph.new()
   local found_verticies = {}
   local queue = double.new()
   queue.back:insert_before(v)
   found_verticies[v] = out:insert_vertex(v)

   while #queue ~= 0 do
      local vertex = queue.front.after.data
      queue.front.after:remove()
      for other_vertex, edge in pairs(vertex.adjacencies) do
         if found_verticies[other_vertex] == nil then
            found_verticies[other_vertex] = out:insert_vertex(other_vertex)
            out:insert_edge(found_verticies[vertex], found_verticies[other_vertex], true)
            queue.back:insert_before(other_vertex)
         end
      end
   end

   return out
end

--- Get a subgraph that includes
--- V and all of its descendants.
--- Return it as a new graph.
function graph.subgraph(v)
   assert(graph.vertex.is(v), "V is not a vertex")
   local out = graph.graph.new()

   local tree = graph.spanning_tree(v)
   local lst = list.new()

   local found_verticies = {}

   for vertex in tree:verticies() do
      if found_verticies[vertex] == nil then
         found_verticies[vertex] = out:insert_vertex(vertex)
      end
      for other_vertex, edge in pairs(other_vertex.adjacencies) do
         if found_verticies[other_vertex] == nil then
            found_verticies[other_vertex] = out:insert_vertex(other_vertex)
         end
         out:insert_edge(vertex, other_vertex, edge)
      end
   end
   return out
end

--- Get all the descendants of vertex V
--- except for V
function graph.descendants(v)
   assert(graph.vertex.is(v), "V is not a vertex")

   local tree = graph.spanning_tree(v)
   local lst = list.new()

   for vertex in tree:verticies() do
      if vertex.data ~= v then
         lst = list.cons(vertex.data, lst)
      end
   end

   return lst
end

--- Remove all of the descendants of a vertex V
--- except for V
function graph.remove_descendants(v)
   assert(graph.vertex.is(v))
   for _, descendant in pairs(graph.descendants(v)) do
      descendant:remove()
   end
end

--- Wrap every vertex in a
--- new vertex object and every
--- edge in a new edge object, returning a graph
function graph.wrap(g)
   local out = graph.graph.new()
   local found_verticies = {}
   for vertex in g:verticies() do
      found_verticies[vertex] = out:insert_vertex(vertex)
   end

   for edge in g:edges() do
      out:insert_edge(found_verticies[edge.from], found_verticies[edge.to], edge)
   end

   return out
end

--- When a vertex in graph G matches predicate P,
--- replace it with output generated
--- from function F, and remove all of its
--- descendants, unless they match P.
--- Returns a new graph.
function graph.reduce_when(g, p, f)
   local out = graph.shallow_copy(g)
   local matches = list.new()
   for vertex in out:verticies() do
      if p(vertex.data) then
         matches = list.cons(vertex, matches)
      end
   end

   local marked = {}

   for _, vertex in pairs(matches) do
      for _, descendant in pairs(graph.descendants(vertex)) do
         marked[descendant] = descendant
      end
   end

   for _, vertex in pairs(matches) do
      marked[vertex] = nil
   end

   for _, vertex in pairs(marked) do
      vertex:remove()
   end

   for _, vertex in pairs(matches) do
      vertex.data = f(vertex.data)
   end

   return out
end

--- Get a graph whose verticies
--- refer to strongly connected componenents
--- starting at the vertex V and using the preorder count table P
--- a table A of vertcies to their components,
--- and a doubly-linked list L of components (P, A, and L can be empty).
--- (Path based strong component algorithm)
--- TODO give credit (I checked out the book)
--- Also TODO rewrite as iterative
local function path_based_strong_component(v, p, a, l)
   -- When the vertex was visited
   local count = 0

   -- Stack of all verticies not yet assigned
   -- a strongly connected component
   local not_assigned_stack = double.new()

   -- Stack of verticies that may or may not
   -- belong to a strongly connected component
   local possibly_connected_stack = double.new()

   local function f(vertex)
      p[vertex] = count
      count = count + 1

      not_assigned_stack.back:insert_before(vertex)
      possibly_connected_stack.back:insert_before(vertex)

      for other_vertex, _ in pairs(vertex.adjacencies) do
         if p[other_vertex] == nil then
            f(other_vertex)
         elseif a[other_vertex] == nil then
            while p[possibly_connected_stack.back.before.data] > p[other_vertex] do
               possibly_connected_stack.back.before:remove()
            end
         end
      end
      if possibly_connected_stack.back.before.data == vertex then
         local new_component = double.new()
         new_component.back:insert_before(vertex)
         a[vertex] = new_component
         while not_assigned_stack.back.before.data ~= vertex do
            local other_vertex = not_assigned_stack.back.before.data
            new_component.back:insert_before(other_vertex)
            a[other_vertex] = new_component
            not_assigned_stack.back.before:remove()
         end
         -- pop vertex from not_assigned_stack
         not_assigned_stack.back.before:remove()
         -- pop vertex from possibly_connected_stack
         possibly_connected_stack.back.before:remove()

         l.back:insert_before(new_component)
      end
   end
   f(v)
end

--- Find all strongly connected components of the graph G,
--- returning both a map of verticies to their components
--- and a list of components.
function graph.strongly_connected_components(g)
   assert(graph.graph.is(g), "G is not a graph")
   local preorder = {}
   local vertex_to_component = {}
   local components = double.new()
   for vertex in g:verticies() do
      if preorder[vertex] == nil then
         path_based_strong_component(vertex, preorder, vertex_to_component, components)
      end
   end
   return vertex_to_component, components
end

--- Fold each strongly connected components into a single
--- vertex, returning the condensation of graph G.
--- as well as the map of verticies to the list verticies in
--- the same component.
--- This condensation is gaurenteed to be an acyclic
--- directed graph.
function graph.condense(g)
   local vertex_to_component, components = graph.strongly_connected_components(g)
   local out = graph.graph.new()

   -- A map of components to its adjacencies
   local component_adjacencies = {}

   -- Find all of the edges going out of a component
   for _, component in pairs(components) do
      component_adjacencies[component] = {}
      local adjacencies = component_adjacencies[component]

      local adjacencies_from_verticies = list.new()
      for _, vertex in pairs(component) do
         for other_vertex, edge in pairs(vertex.adjacencies) do
            if vertex_to_component[other_vertex] ~= component then
               if adjacencies[vertex_to_component[other_vertex]] == nil then
                  adjacencies[vertex_to_component[other_vertex]] = list.new(edge)
               else
                  adjacencies[vertex_to_component[other_vertex]] =
                     list.cons(edge, adjacencies[vertex_to_component[other_vertex]])
               end
            end
         end
      end
   end

   -- Now, build the condensation.

   -- Insert all the verticies
   local found_components = {}
   for _, component in pairs(components) do
      found_components[component] = out:insert_vertex(component)
   end

   -- Insert all the edges
   for _, component in pairs(components) do
      for other_component, edge in pairs(component_adjacencies[component]) do
         out:insert_edge(found_components[component],
                         found_components[other_component],
                         edge)
      end
   end

   return out, vertex_to_component
end

-- TODO move tests somewhere
-- ---------------------------------------------------------------------------------------------------
-- -- Tests
-- ---------------------------------------------------------------------------------------------------
-- local function test_graph_one()
--    local g = graph.graph.new()
--    local apple = assert(g:insert_vertex("apple"))
--    local carrot = assert(g:insert_vertex("carrot"))
--    local bannana = assert(g:insert_vertex("bannana"))

--    local apple_carrot = g:insert_edge(apple, carrot, "apple to carrot")
--    local carrot_bannana = g:insert_edge(carrot, bannana, "carrot to bannana")
--    local bannana_apple = g:insert_edge(bannana, apple, "bannana to apple")
--    assert(apple:get_edge(carrot).data == "apple to carrot")
--    assert(carrot:get_edge(bannana).data == "carrot to bannana")
--    assert(bannana:get_edge(apple).data == "bannana to apple")
-- end

-- test_graph_one()

return graph
