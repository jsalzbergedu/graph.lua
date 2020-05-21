rockspec_format = "3.0"
package = "graphlua"
version = "dev-1"
source = {
   url = "git+https://github.com/jsalzbergedu/luagraph.git"
}
description = {
   homepage = "",
   license = "MIT"
}
build = {
   type = "builtin",
   modules = {
      graph = "src/graph.lua",
      ["lua-graph.double"] = "src/lua-graph/double.lua",
      ["lua-graph.lua-modules.list"] = "src/lua-graph/lua-modules/list.lua",
      ["lua-graph.lua-modules.list-test"] = "src/lua-graph/lua-modules/list-test.lua",
      ["lua-graph.lua-modules.recordtype"] = "src/lua-graph/lua-modules/recordtype.lua",
      ["lua-graph.lua-modules.recordtype-test"] = "src/lua-graph/lua-modules/recordtype-test.lua",
      ["lua-graph.lua-modules.set"] = "src/lua-graph/lua-modules/set.lua",
      ["lua-graph.lua-modules.set-test"] = "src/lua-graph/lua-modules/set-test.lua",
      ["lua-graph.lua-modules.strict"] = "src/lua-graph/lua-modules/strict.lua",
      ["lua-graph.lua-modules.submodule"] = "src/lua-graph/lua-modules/submodule.lua",
      ["lua-graph.lua-modules.submodule-test"] = "src/lua-graph/lua-modules/submodule-test.lua",
      ["lua-graph.lua-modules.termcolor"] = "src/lua-graph/lua-modules/termcolor.lua",
      ["lua-graph.lua-modules.termcolor-test"] = "src/lua-graph/lua-modules/termcolor-test.lua",
      ["lua-graph.lua-modules.test"] = "src/lua-graph/lua-modules/test.lua",
      ["lua-graph.lua-modules.test-test"] = "src/lua-graph/lua-modules/test-test.lua",
      ["lua-graph.lua-modules.thread"] = "src/lua-graph/lua-modules/thread.lua",
      ["lua-graph.lua-modules.thread-test"] = "src/lua-graph/lua-modules/thread-test.lua"
   }
}
